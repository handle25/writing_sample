################################################################################
# Created 8.15.2026 
# Author: Sophie Handley 
# Purpose: Construct LP regression outcomes and merge QCEW, LODES, IRS, population
################################################################################
rm(list = ls())

# Paths ------------------------------------------------------------------------

local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
path  <- "D:/writing_sample/data"
setwd(path)

# Functions --------------------------------------------------------------------

winsor <- function(dt, var, p = 0.01) {
  q <- quantile(dt[[var]], probs = c(p, 1 - p), na.rm = TRUE)
  
  w_var <- paste0("w_", var)
  
  dt[, (w_var) := pmin(pmax(get(var), q[1]), q[2])]
}


make_share <- function(dt, num, denom) {
  
  share_var <- paste0(num, "_share_", denom)
  
  dt[, (share_var) := get(num) / get(denom)]
  
  winsor(dt, share_var)
}

share_denom_all <- function(dt, var) {
  make_share(dt, var, "resident_emp")
  make_share(dt, var, "workplace_emp")
  make_share(dt, var, "outside_jobs")
  make_share(dt, var, "total_servc_jobs")
  make_share(dt, var, "total_goods_jobs")
  make_share(dt, var, "population")
  make_share(dt, var, "population_2000")
  make_share(dt, var, "population_2007")
}

make_base_year <- function(dt, var, base_year = 2000) {
  
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

# Read in data -----------------------------------------------------------------
# Population
acs <- fread(paste0(path, "/acs/population_1995_2023.csv"))
acs_1y <- fread(paste0(path, "/acs/acs_1y_2005_2024_commuting.csv"))

# QCEW
qcew <- fread(paste0(path, "/output/lp_weighted_qcew.csv"))

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


# LODES commuting --------------------------------------------------------------
lodes <- fread(paste0(path,"/output/lp_new_lodes_collapsed_all_no_crosswalk.csv"))
lodes_measure <- fread(paste0(path, "/output/lp_lodes_measures.csv"))
setnames(lodes, "county", "area_fips")
setnames(lodes_measure, "county", "area_fips")
# Labor force denominator -------------------------------------------------------
lodes <- merge(
  lodes,
  laus,
  by = c("area_fips", "year"),
  all.x = TRUE
)

setorder(lodes, area_fips, year)

# Standard outside-jobs / labor-force share and long difference
make_share(lodes, "outside_jobs", "labor_force")
cols <- setdiff(names(laus), c("area_fips", "year"))
lodes[, (cols) := lapply(.SD, function(x) NULL), .SDcols = cols]

# IRS migration ----------------------------------------------------------------
irs <- fread(paste0(path, "/irs/new_lp_irs_migration_full.csv"))

cols <- names(irs)[sapply(irs, is.character)]
irs[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

irs_detail <- fread(paste0(path, "/irs/lp_irs_destination_conditions_full.csv"))
irs <- merge(irs, irs_detail, 
             by = c("area_fips", "year"), 
             all.x=T)

towin <- setdiff(grep("ew|rw", names(irs) , value = T), grep("share", names(irs), value = T))
for (i in 1:length(towin)){
  winsor(irs, towin[i])
}
irs_measure <- fread(paste0(path, "/irs/lp_irs_agi_measures.csv")) |> 
  fmutate(area_fips = as.integer(from_area_fips))
irs_agi <- fread(paste0(path, "/irs/lp_irs_agi_full.csv")) |> 
  fmutate(area_fips = as.integer(area_fips))
irs_agi[, ln_agi_per_return := log(agi_per_return)]
setorder(irs_agi, area_fips, year)


irs <- merge(irs, irs_measure, 
             by = c("area_fips", "year"), all.x = T)

irs <- merge(irs, irs_agi, 
             by = c("area_fips", "year"), all.x = T)



# Merge datasets ---------------------------------------------------------------

reg <- merge(
  qcew,
  lodes,
  by = c("area_fips", "year"),
  all.x = TRUE
)

nrow(reg)

reg <- merge(
  reg,
  irs,
  by = c("area_fips", "year"),
  all.x = TRUE
)

reg <- merge(
  reg,
  laus,
  by = c("area_fips", "year"),
  all.x = TRUE
)
nrow(reg)

reg <- merge(
  reg,
  acs,
  by = c("area_fips", "year"),
  all.x = TRUE
)

reg <- merge(
  reg,
  acs_1y,
  by = c("area_fips", "year"),
  all.x = TRUE
)

reg <- merge(
  reg, 
  lodes_measure, 
  by = c("area_fips", "year"), 
  all.x = T
)

setorder(reg, area_fips, year)

# Rename fundamental employment concepts
# QCEW:
# Employment located at establishments in the county
setnames(reg, "total_emp", "workplace_emp")

# LODES:
# Employed residents of the county, regardless of workplace county
setnames(reg, "total_jobs", "resident_emp")

# Basic regression variables ---------------------------------------------------
# State FIPS for clustering
reg[, statefip :=
      floor(as.integer(area_fips) / 1000)]

# Predetermined population

reg <- make_base_year(reg, "population", 2000)
reg <- make_base_year(reg, "population", 2007)
reg <- make_base_year(reg, "workplace_emp", 2000)
reg <- make_base_year(reg, "resident_emp", 2000)

# Net migration
reg[, resident_workplace_emp_gap := resident_emp - workplace_emp]
reg[, l_resident_workplace_emp_gap := log(resident_workplace_emp_gap)]

# Migration --------------------------------------------------------------------
reg[, exemptions_net_migration := exemptions_3_inflow - exemptions_3_outflow]
reg[, returns_net_migration := returns_3_inflow - returns_3_outflow]
reg[, exemptions_total_migration := exemptions_3_inflow + exemptions_3_outflow]
reg[, returns_total_migration := returns_3_inflow + returns_3_outflow]

# Migration shares
make_share(reg, "exemptions_net_migration", "population")
make_share(reg, "returns_net_migration", "population")
make_share(reg, "exemptions_net_migration", "exemptions")
make_share(reg, "returns_net_migration", "returns")

# Direction of migration
make_share(reg, "exemptions_3_outflow", "exemptions_total_migration")
make_share(reg, "returns_3_outflow", "returns_total_migration")

# reg[, net_migration :=
#       exemptions_3_inflow - exemptions_3_outflow]
# 
# reg[, net_outmigration := exemptions_3_outflow / (exemptions_3_outflow + exemptions_3_inflow) * 100]

# # Standard contemporaneous denominators
# make_share(reg, "net_migration", "resident_emp")
# make_share(reg, "net_migration", "workplace_emp")
# make_share(reg, "net_migration", "population")
# 
# # Preferred LP migration rate:
# # current migration flow / population immediately before period t
# make_share(reg, "net_migration", "population_2007")
# make_share(reg, "net_migration", "population_2000")

# Manufacturing employment shares

make_share(reg, "manufac_emp", "resident_emp")
make_share(reg, "manufac_emp", "workplace_emp")
make_share(reg, "manufac_emp", "population")
make_share(reg, "manufac_emp", "population_2000")
make_share(reg, "manufac_emp", "workplace_emp_2000")
make_share(reg, "manufac_emp", "resident_emp_2000")

# Employment-to-population / resident-workplace ratios

make_share(reg, "resident_emp", "population")
make_share(reg, "workplace_emp", "population")
make_share(reg, "workplace_emp", "resident_emp")
make_share(reg, "unemployed", "labor_force")
make_share(reg, "labor_force", "population")

# LODES employment composition

level_vars <- c(
  grep("^outside.*_jobs$", names(reg), value = TRUE),
  grep("^total.*_jobs$", names(reg), value = TRUE),
  grep("^inside.*_jobs$", names(reg), value = TRUE)
)

level_vars <- unique(level_vars)

for (i in level_vars) {
  share_denom_all(reg, i)
}

# Winsorize shift-share variables
winsor(reg, "IPW_US")
winsor(reg, "IPW_OTH")
winsor(reg, "IPW_US_pop")
winsor(reg, "IPW_OTH_pop")
winsor(reg, "IPW_US_wap")
winsor(reg, "IPW_OTH_wap")
winsor(reg, "resident_workplace_emp_gap")
winsor(reg, "l_resident_workplace_emp_gap")


# Save
fwrite(
  reg,
  paste0(path, "/output/lp_transformed_reg.csv"))

fwrite(
  reg,
  paste0(local, "/output/lp_transformed_reg.csv"))
  
county_conditions <- reg[, .(
  area_fips,
  year,
  IPW_US = IPW_US,
  unemployed_share_labor_force,
  sh_empl_mfg,
  labor_force_share_population
)]

fwrite(
  county_conditions,
  paste0(path, "/output/lp_shock_exposure.csv"))
