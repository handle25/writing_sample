################################################################################
# Created 8.8.2026 
# Author: Sophie Handley 
# Purpose: Construct regression outcomes and merge QCEW, LODES, IRS, population
################################################################################

rm(list = ls())

# qcewdata 
path <- "D:/writing_sample/data"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
setwd(path)

# define functions -------------------------------------------------------------
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

make_share_diff <- function(dt, num, denom, id = "area_fips") {
  
  share_var <- paste0(num, "_share_", denom)
  diff_var  <- paste0("d_", share_var)
  
  dt[, (share_var) := get(num) / get(denom) * 100 ]
  
  dt[, (diff_var) :=
       get(share_var) -
       shift(get(share_var), 1),
     by = id]
  
  winsor(dt, diff_var)
}

diff_denom_all <- function(dt, var) { 
  make_share_diff(dt, var, "resident_emp")
  make_share_diff(dt, var, "workplace_emp")
  make_share_diff(dt, var, "outside_jobs")
  make_share_diff(dt, var, "total_servc_jobs")
  make_share_diff(dt, var, "total_goods_jobs")
  make_share_diff(dt, var, "population")
  make_share_diff(dt, var, "population_2000")
}


make_base_year <- function(dt, var, base_year = 2000, id = "area_fips") {
  newvar <- paste0(var, "_", base_year)
  dt_year <- dt[
    year == base_year,
    .(value = mean(get(var), na.rm = TRUE)),
    by = area_fips
  ]
  setnames(dt_year, "value", newvar)
  dt <- merge(dt, dt_year, 
              by = "area_fips", 
              all.x = T)
  return(dt)
}

################################################################################
# Population
################################################################################
acs <- fread(paste0(path, "/acs/population_1995_2023.csv"))

acs <- make_base_year(acs, "population")

################################################################################
# Read datasets
################################################################################

qcew <- fread(
  paste0(path, "/output/weighted_qcew.csv")
)

qcew[, .(
  N = .N,
  mean_US   = mean(IPW_US, na.rm = TRUE),
  median_US = median(IPW_US, na.rm = TRUE),
  min_US    = min(IPW_US, na.rm = TRUE),
  max_US    = max(IPW_US, na.rm = TRUE),
  mean_OTH  = mean(IPW_OTH, na.rm = TRUE)
), by = year]

qcew[, area_fips_str := sprintf("%06d", area_fips)]
qcew[, state := floor(area_fips / 1000)]

# LAUS for unemployment --------------------------------------------------------
laus <- read_excel(paste0(path, "/laus/laucnty90.xlsx"), skip = 1)
for (year in c(1991:2024)) {
  y <- sprintf("%02.f", as.integer(substr(as.character(year), 3,4)))
  laus <- rbind(laus, 
                read_excel(paste0(path, "/laus/laucnty", y, ".xlsx"),
                           skip = 1)
  )
}
laus <- laus |> 
  clean_names() |>
  data.table() |> 
  fmutate(area_fips = as.integer(
    paste0(state_fips_code, county_fips_code)), 
    year = as.integer(year))
# LODES ------------------------------------------------------------------------

lodes <- fread(
  paste0(
    path,
    "/output/lp_new_lodes_collapsed_all_no_crosswalk.csv"
  )
)

lodes <- lodes[year %in% c(1990, 2000, 2007, 2013)]

setnames(
  lodes,
  "total_jobs",
  "resident_emp"
)


lodes <- merge(
  lodes,
  acs,
  by.x = c("county", "year"),
  by.y = c("area_fips", "year"),
  all.x = TRUE
) 

setorder(lodes, county, year)
lodes[, population_t_1 := shift(population), by = county]
make_share_diff(lodes, "outside_jobs", "population", id = "county")
make_share_diff(lodes, "outside_jobs", "population_2000", id = "county")
make_share_diff(lodes, "outside_jobs", "resident_emp", id = "county")


lodes[, d_outside_jobs :=
        outside_jobs - shift(outside_jobs),
      by = county]

lodes[, d_outside_jobs_share_population_t_1 :=
        d_outside_jobs / population_t_1 * 100]

winsor(lodes, "d_outside_jobs_share_population_t_1")


lodes[, population_2000 := NULL]
lodes[, population := NULL]
# IRS --------------------------------------------------------------------------

irs <- fread(
  paste0(path, "/irs/lp_irs_migration_full.csv")
)

cols <- setdiff(names(irs), c("area_fips", "year"))

irs[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

irs[year %in% c(1990:1995), new_year := 1995]
irs[year %in% c(1996:2000), new_year := 2000]
irs[year %in% c(2001:2006), new_year := 2007]
irs[year %in% c(2007:2012), new_year := 2013]

irs[year %in% c(1990:1995), base_year := 1990]
irs[year %in% c(1996:2000), base_year := 1995]
irs[year %in% c(2001:2006), base_year := 2000]
irs[year %in% c(2007:2012), base_year := 2007]

irs[, year := new_year]
irs <- irs |> fgroup_by(new_year, base_year, year, area_fips) |> 
  fsum()

irs <- merge(irs, acs, 
             by.x = c("area_fips","base_year"), 
             by.y = c("area_fips", "year"), 
             all.x = T)

# get base period population 
setnames(irs, "population", "population_base_year") 
irs[,population_2000 := NULL]

################################################################################
# Merge datasets
################################################################################

reg <- merge(
  qcew,
  lodes,
  by.x = c("area_fips", "year"),
  by.y = c("county", "year"), 
  all = TRUE
)

nrow(reg)

reg <- merge(
  reg,
  irs,
  by = c("area_fips", "year"),
  all.x = TRUE
)

nrow(reg)

reg <- merge(
  reg,
  laus,
  by = c("area_fips", "year"),
  all.x = TRUE
)

reg <- merge(
  reg,
  acs,
  by = c("area_fips", "year"),
  all.x = TRUE
)

setorder(
  reg,
  area_fips,
  year
)


################################################################################
# Basic regression variables
################################################################################

reg[, t2 := as.integer(year == 2013)]

# State FIPS for clustering
reg[, statefip :=
      floor(as.integer(area_fips) / 1000)]


################################################################################
# Net migration flows
################################################################################

reg[, net_migration := (returns_3_inflow) - (returns_3_outflow)]

reg[,lfp := labor_force / population*100]
make_share_diff(reg, "labor_force", "population")
make_share_diff(reg, "labor_force", "workplace_emp")
make_share_diff(reg, "unemployed", "labor_force")

reg[,unemp_check := unemployed / labor_force*100 ]

# Net migration relative to employed residents
diff_denom_all(reg, "net_migration")
make_share_diff(reg, "net_migration", "population_base_year")
diff_denom_all(reg, "resident_emp")

################################################################################
# Employment-to-population ratios
################################################################################

# Employment of county residents / county population
make_share_diff(reg, "resident_emp", "population")
make_share_diff(reg, "workplace_emp", "population")
make_share_diff(reg, "workplace_emp", "resident_emp")


################################################################################
# Variables to difference in levels
################################################################################

# level_vars <- c(
#   grep("^outside.*_jobs$", names(reg), value = TRUE),
#   grep("^total.*_jobs$", names(reg), value = TRUE),
#   grep("^inside.*_jobs$", names(reg), value = TRUE)
# )
# 
# level_vars <- unique(level_vars)
# 
# for (i in level_vars) {
#   diff_denom_all(reg, i)
# }

winsor(reg, "IPW_US") 
winsor(reg, "IPW_OTH") 
winsor(reg, "IPW_US_10yr") 
winsor(reg, "IPW_OTH_10yr") 

# net migration are already flows, no need to difference. 
winsor(reg, "net_migration_share_resident_emp")
winsor(reg, "net_migration_share_workplace_emp")
winsor(reg, "net_migration_share_population")
winsor(reg, "net_migration_share_population_2000")
reg[,net_migration_share_population_2000_t := net_migration / population_2000]
winsor(reg, "net_migration_share_population_base_year")

################################################################################
# Save
################################################################################

fwrite(
  reg,
  paste0(
    path,
    "/output/transformed_reg.csv"
  )
)

fwrite(reg, paste0(local, "/output/transformed_reg.csv"))