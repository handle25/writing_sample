################################################################################
# Created 8.15.2026 
# Author: Sophie Handley 
# Purpose: Construct regression outcomes and merge QCEW, LODES, IRS, population
################################################################################


rm(list = ls())

# qcewdata 
path <- "D:/writing_sample/data"
setwd(path)

crosswalk <- read_excel("cz00eqvv1.xls") |> 
  data.table() |> 
  clean_names() |> 
  fselect(fips, commuting_zone_id_2000) |> 
  fmutate(
    fips = sprintf("%05d", as.integer(fips)),
    state = as.integer(substr(fips, 1, 2))
  ) |>
  fgroup_by(fips, commuting_zone_id_2000) |>
  fsummarize(state = flast(state))

cw_s <- copy(crosswalk) |> 
  fgroup_by(commuting_zone_id_2000) |> 
  fsummarize(state = flast(state))
crosswalk[, state := NULL ]

################################################################################
# Population
################################################################################

acs <- fread(paste0(path, "/output/population_1995_2023_CZ.csv"))

acs_1y <- 
  fread(paste0(path, "/acs/acs_1y_2005_2024_commuting.csv"))
acs_1y[,fips := as.character(area_fips)]
# acs[, area_fips := as.integer(paste0(sprintf("%02.0f", state), sprintf("%03.0f", county)))]


#collapse acs to commuting zone values 
acs_1y <- merge(acs_1y, crosswalk, 
             by = "fips", 
             all.x = TRUE)


select <- grep("commute", names(acs_1y), value = TRUE)

acs_1y <- acs_1y[
  ,
  lapply(.SD, sum, na.rm = TRUE),
  by = .(year, commuting_zone_id_2000),
  .SDcols = select
]
  

################################################################################
# Read datasets
################################################################################

qcew <- fread(
  paste0(path, "/output/lp_weighted_qcew_CZ.csv")
)

qcew[, .(
  N = .N,
  mean_US   = mean(IPW_US, na.rm = TRUE),
  median_US = median(IPW_US, na.rm = TRUE),
  min_US    = min(IPW_US, na.rm = TRUE),
  max_US    = max(IPW_US, na.rm = TRUE),
  mean_OTH  = mean(IPW_OTH, na.rm = TRUE)
), by = year]


# PAUSE INCOMPLETE 
# LODES ------------------------------------------------------------------------

lodes <- fread(
  paste0(
    path,
    "/output/lp_lodes_collapsed_all_no_crosswalk.csv"
  )
)


crosswalk[, county := as.integer(fips)]
lodes <- merge(lodes, crosswalk, 
               by = "county", 
               all.x = TRUE)

lodes[, state_str := NULL]
lodes[, fips := NULL]

lodes <- lodes |> 
  fgroup_by(commuting_zone_id_2000, year) |> 
  fsum()

# drop all share vars to avoid summing shares 
drop_vars <- grep(
  "share|outside_d_",
  names(lodes),
  value = TRUE,
  ignore.case = TRUE
)

drop_vars   # inspect first

lodes[, (drop_vars) := NULL]

# IRS --------------------------------------------------------------------------

irs <- fread(
  paste0(path, "/irs/lp_irs_migration_full.csv")
)

irs <- merge(irs, crosswalk, 
             by.x = "area_fips" ,
             by.y = "county", 
             all.x = TRUE
             )
irs[,(names(irs)) := lapply(.SD, as.integer), .SDcols = names(irs)]
irs[,c("fips", "county", "area_fips") := NULL]
irs <- irs |> 
  fgroup_by(commuting_zone_id_2000,  year) |> 
  fsum()


################################################################################
# Merge datasets
################################################################################

reg <- merge(
  qcew,
  lodes,
  by = c("commuting_zone_id_2000", "year"),
  all.x = TRUE
)

nrow(reg)

reg <- merge(
  reg,
  irs,
  by = c("commuting_zone_id_2000", "year"),
  all.x = TRUE
)

nrow(reg)

reg <- merge(
  reg,
  acs,
  by = c("commuting_zone_id_2000", "year"),
  all.x = TRUE
)

reg <- merge(
  reg, 
  acs_1y, 
  by = c("commuting_zone_id_2000","year"), 
  all.x = TRUE
)

setorder(
  reg,
  commuting_zone_id_2000,
  year
)

# winsorize function 

winsor <- function(dt, var, p = 0.01) {
  
  q <- quantile(
    dt[[var]],
    probs = c(p, 1 - p),
    na.rm = TRUE
  )
  
  w_var <- paste0("w_", var)
  
  dt[, (w_var) := pmin(
    pmax(get(var), q[1]),
    q[2]
  )]
}



################################################################################
# Rename fundamental employment concepts
################################################################################

# QCEW:
# Employment located at establishments in the county
setnames(
  reg,
  "total_emp",
  "workplace_emp"
)

# LODES:
# Employed residents of the county, regardless of workplace county
setnames(
  reg,
  "total_jobs",
  "resident_emp"
)


################################################################################
# Basic regression variables
################################################################################

################################################################################
# Net migration flows
################################################################################

reg[, net_migration :=
      as.integer(returns_3_inflow) -
      as.integer(returns_3_outflow)]

# Net migration relative to employed residents
reg[, net_migration_share_resident_emp :=
      net_migration / resident_emp]

reg[, net_migration_share_workplace_emp :=
      net_migration / workplace_emp]

reg[, net_migration_share_population :=
      net_migration / population]

reg[, net_migration_share_population_t0 :=
      net_migration / shift(population, type = "lag", n = 1), by = .(commuting_zone_id_2000)]

# Winsorize net migration share -----------------------------------------------
for (v in c("net_migration_share_workplace_emp",
            "net_migration_share_resident_emp",
            "net_migration_share_population",
            "net_migration_share_population_t0", 
            "IPW_US", "IPW_OTH")){
  winsor(reg, v)
}


# Winsorize vars ---------------------------------------------------------------
################################################################################
# Employment-to-population ratios
################################################################################

# Employment of county residents / county population
reg[, resident_emp_population_ratio :=
      resident_emp / population]

# Employment located in county / county population
reg[, workplace_emp_population_ratio :=
      workplace_emp / population]

winsor(reg, "resident_emp_population_ratio")
winsor(reg, "workplace_emp_population_ratio")

################################################################################
# Construct level shares for LP outcomes
################################################################################

level_vars <- c(
  grep("^outside.*_jobs$", names(reg), value = TRUE),
  grep("^total.*_jobs$", names(reg), value = TRUE),
  grep("^inside.*_jobs$", names(reg), value = TRUE)
)

level_vars <- unique(level_vars)

for (i in level_vars) {
  
  # Resident employment
  reg[, (paste0(i, "_share_resident_emp")) :=
        get(i) / resident_emp]
  
  # Workplace employment
  reg[, (paste0(i, "_share_workplace_emp")) :=
        get(i) / workplace_emp]
  
  # Outside employment
  reg[, (paste0(i, "_share_outside_jobs")) :=
        get(i) / outside_jobs]
  
  # Resident service employment
  reg[, (paste0(i, "_share_service_jobs")) :=
        get(i) / total_servc_jobs]
  
  # Resident goods employment
  reg[, (paste0(i, "_share_goods_jobs")) :=
        get(i) / total_goods_jobs]
  
  # Population
  reg[, (paste0(i, "_share_population")) :=
        get(i) / population]
}


################################################################################
# Winsorize level shares at 1st / 99th percentiles
################################################################################

share_vars <- grep(
  "_share_(resident_emp|workplace_emp|outside_jobs|service_jobs|goods_jobs|population)$",
  names(reg),
  value = TRUE
)

for (v in share_vars) {
  winsor(reg, v)
}

reg[, workplace_emp_share_resident_emp := workplace_emp / resident_emp]

################################################################################
# Manufacturing employment relative to resident employment
################################################################################

reg[, manuf_share_emp :=
      manufac_emp / workplace_emp]

reg[, manuf_emp_share_pop :=
      manufac_emp / population]

winsor(reg, "manuf_share_emp")
winsor(reg, "manuf_emp_share_pop")



################################################################################
# Save
################################################################################
# Duplicate county-years?
reg <- merge(reg, 
              cw_s, 
              by = "commuting_zone_id_2000")

reg[, .N, by = .(commuting_zone_id_2000, year)][N > 1]


# Sample coverage
reg[, .(
  N = .N,
  counties = uniqueN(commuting_zone_id_2000),
  min_year = min(year, na.rm = TRUE),
  max_year = max(year, na.rm = TRUE)
)]

# Missingness in variables used by the LP
reg[, .(
  miss_mfg = sum(is.na(w_manuf_share_emp)),
  miss_mfg_pop = sum(is.na(w_manuf_emp_share_pop)),
  miss_US = sum(is.na(w_IPW_US)),
  miss_OTH = sum(is.na(w_IPW_OTH)),
  miss_control = sum(is.na(l_shind_manuf))
)]
fwrite(
  reg,
  paste0(
    path,
    "/output/lp_transformed_reg_CZ.csv"
  )
)