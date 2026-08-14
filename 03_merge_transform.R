################################################################################
# Created 8.8.2026 
# Author: Sophie Handley 
# Purpose: Weight dollar shifts in naics x year imports by labor shares with QCEW

################################################################################

rm(list = ls())

# qcewdata 
path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
setwd(path)

acs <- rbind(fread(paste0(path, "/acs/co-est00int-tot.csv")), 
             fread(paste0(path,"/acs/co-est2020.csv")), fill = TRUE)

acs <- melt(
  acs,
  id.vars = c("STATE", "COUNTY"),
  measure.vars = patterns("^POPESTIMATE"),
  variable.name = "year",
  value.name = "population"
) |>
  fmutate(year = as.integer(substr(year, 12, 15)))



url_1995 <- paste0(
  "https://www2.census.gov/programs-surveys/popest/",
  "tables/1990-2000/intercensal/st-co/stch-icen1995.txt"
)

pop1995 <- fread(url_1995)
names <- c("year","area_fips","age","sex","eth","population")

names(pop1995) <- names
pop1995 <- pop1995 |> 
  fgroup_by(area_fips) |> 
  fsummarize(population = fsum(population)) |> 
  fmutate(year = 1995) 

acs <- rbind(acs, pop1995, fill = TRUE)

acs[,area_fips := as.character(as.integer(area_fips))]


# Drop rows created from years not covered by that source file
acs <- acs[!is.na(population)]

# Drop state totals
acs <- acs[COUNTY != 0]

acs[, area_fips := as.integer(paste0(
  sprintf("%02d", STATE),
  sprintf("%03d", COUNTY)
))]


qcew <- fread(paste0(path, "/output/weighted_qcew.csv")) 
qcew[, .(
  N = .N,
  mean_US  = mean(IPW_US, na.rm = TRUE),
  median_US = median(IPW_US, na.rm = TRUE),
  min_US   = min(IPW_US, na.rm = TRUE),
  max_US   = max(IPW_US, na.rm = TRUE),
  mean_OTH = mean(IPW_OTH, na.rm = TRUE)
), by = year]

qcew[,area_fips_str := sprintf("%06d", area_fips)]
qcew[,state := floor(area_fips/1000)]
lodes <- fread(paste0(path, "/output/lodes_collapsed_all_no_crosswalk.csv"))
lodes[year %in% c(2002, 2003, 2004), year := 2000]

irs <- fread(paste0(path, "/irs_migration_full.csv"))
# check regs 2013 --------------------------------------------------------------

reg <- merge(qcew, lodes, 
             by.x = c("area_fips", "year"), 
             by.y = c("county", "year"))
nrow(reg)
reg <- merge(reg, irs,
             by = c("area_fips", "year"),
             all.x = TRUE)
nrow(reg)
reg <- merge(reg, acs,
             by = c("area_fips", "year"),
             all.x = TRUE)

setorder(reg, area_fips, year)

# reg <- reg[year %in% c(2007, 2013), ]# Period indicator
reg[, t2 := as.integer(year == 2013)]

# State FIPS for clustering
reg[, statefip := floor(as.integer(area_fips) / 1000)]


# ---------------------------------------------------------------------------
# Net migration flows
# ---------------------------------------------------------------------------

reg[, net_migration :=
      as.integer(returns_3_inflow) - as.integer(returns_3_outflow)]

reg[, net_migration_share_emp :=
      net_migration / total_emp]

reg[, d_net_migration_pctchange :=
      (net_migration - shift(net_migration, n = 1)) /
      shift(net_migration, n = 1),
    by = .(area_fips)]


# Winsorize net migration share -----------------------------------------------

q99 <- quantile(reg[, net_migration_share_emp], .99, na.rm = TRUE)
q01 <- quantile(reg[, net_migration_share_emp], .01, na.rm = TRUE)

reg[, w_net_migration_share_emp := net_migration_share_emp]

reg[
  net_migration_share_emp >= q99,
  w_net_migration_share_emp := q99
]

reg[
  net_migration_share_emp <= q01,
  w_net_migration_share_emp := q01
]


# Winsorize change in net migration -------------------------------------------

q99 <- quantile(reg[, d_net_migration_pctchange], .99, na.rm = TRUE)
q01 <- quantile(reg[, d_net_migration_pctchange], .01, na.rm = TRUE)

reg[, w_d_net_migration_pctchange := d_net_migration_pctchange]

reg[
  d_net_migration_pctchange >= q99,
  w_d_net_migration_pctchange := q99
]

reg[
  d_net_migration_pctchange <= q01,
  w_d_net_migration_pctchange := q01
]

# ---------------------------------------------------------------------------
# Variables to difference in levels
# ---------------------------------------------------------------------------

level_vars <- c(
  grep("^outside.*_jobs$", names(reg), value = TRUE),
  grep("^total.*_jobs$", names(reg), value = TRUE),
  grep("^inside.*_jobs$", names(reg), value = TRUE)
)

level_vars <- unique(level_vars)

for (i in level_vars) {
  
  # denominator = total jobs ---------------------------------------------------
  
  var_d_all <- paste0(i, "_share_total_jobs")
  d_var_d_all <- paste0("d_", i, "_share_total_jobs")
  
  reg[, (var_d_all) := get(i) / total_jobs]
  
  reg[, (d_var_d_all) :=
        get(var_d_all) - shift(get(var_d_all), type = "lag", n = 1),
      by = .(area_fips)]
  
  w_var_d_all <- paste0("w_", d_var_d_all)
  
  q99 <- quantile(reg[[d_var_d_all]], .99, na.rm = TRUE)
  q01 <- quantile(reg[[d_var_d_all]], .01, na.rm = TRUE)
  
  reg[, (w_var_d_all) := get(d_var_d_all)]
  reg[get(w_var_d_all) >= q99, (w_var_d_all) := q99]
  reg[get(w_var_d_all) <= q01, (w_var_d_all) := q01]
  
  
  # denominator = outside jobs -------------------------------------------------
  
  var_d_out <- paste0(i, "_share_outside_jobs")
  d_var_d_out <- paste0("d_", i, "_share_outside_jobs")
  
  reg[, (var_d_out) := get(i) / outside_jobs]
  
  reg[, (d_var_d_out) :=
        get(var_d_out) - shift(get(var_d_out), type = "lag", n = 1),
      by = .(area_fips)]
  
  w_var_d_out <- paste0("w_", d_var_d_out)
  
  q99 <- quantile(reg[[d_var_d_out]], .99, na.rm = TRUE)
  q01 <- quantile(reg[[d_var_d_out]], .01, na.rm = TRUE)
  
  reg[, (w_var_d_out) := get(d_var_d_out)]
  reg[get(w_var_d_out) >= q99, (w_var_d_out) := q99]
  reg[get(w_var_d_out) <= q01, (w_var_d_out) := q01]
  
  
  # denominator = service jobs -------------------------------------------------
  
  var_d_svc <- paste0(i, "_share_service_jobs")
  d_var_d_svc <- paste0("d_", i, "_share_service_jobs")
  
  reg[, (var_d_svc) := get(i) / total_servc_jobs]
  
  reg[, (d_var_d_svc) :=
        get(var_d_svc) - shift(get(var_d_svc), type = "lag", n = 1),
      by = .(area_fips)]
  
  w_var_d_svc <- paste0("w_", d_var_d_svc)
  
  q99 <- quantile(reg[[d_var_d_svc]], .99, na.rm = TRUE)
  q01 <- quantile(reg[[d_var_d_svc]], .01, na.rm = TRUE)
  
  reg[, (w_var_d_svc) := get(d_var_d_svc)]
  reg[get(w_var_d_svc) >= q99, (w_var_d_svc) := q99]
  reg[get(w_var_d_svc) <= q01, (w_var_d_svc) := q01]
  
  # denominator = goods jobs ---------------------------------------------------
  
  var_d_good <- paste0(i, "_share_goods_jobs")
  d_var_d_good <- paste0("d_", i, "_share_goods_jobs")
  
  reg[, (var_d_good) := get(i) / total_goods_jobs]
  
  reg[, (d_var_d_good) :=
        get(var_d_good) - shift(get(var_d_good), type = "lag", n = 1),
      by = .(area_fips)]
  
  w_var_d_good <- paste0("w_", d_var_d_good)
  
  q99 <- quantile(reg[[d_var_d_good]], .99, na.rm = TRUE)
  q01 <- quantile(reg[[d_var_d_good]], .01, na.rm = TRUE)
  
  reg[, (w_var_d_good) := get(d_var_d_good)]
  reg[get(w_var_d_good) >= q99, (w_var_d_good) := q99]
  reg[get(w_var_d_good) <= q01, (w_var_d_good) := q01]
  
  # denominator = population ---------------------------------------------------
  
  var_d_pop <- paste0(i, "_share_population")
  d_var_d_pop <- paste0("d_", i, "_share_population")
  
  reg[, (var_d_pop) := get(i) / shift(population, type = "lag", n = 1), by = .(area_fips)]
  
  reg[, (d_var_d_pop) :=
        get(var_d_pop) - shift(get(var_d_pop), type = "lag", n = 1),
      by = .(area_fips)]
  
  w_d_var_d_pop <- paste0("w_", d_var_d_pop)
  
  q99 <- quantile(reg[[d_var_d_pop]], .99, na.rm = TRUE)
  q01 <- quantile(reg[[d_var_d_pop]], .01, na.rm = TRUE)
  
  reg[, (w_d_var_d_pop) := get(d_var_d_pop)]
  reg[get(w_d_var_d_pop) >= q99, (w_d_var_d_pop) := q99]
  reg[get(w_d_var_d_pop) <= q01, (w_d_var_d_pop) := q01]
}

fwrite(reg, paste0(path, "/output/transformed_reg.csv"))


