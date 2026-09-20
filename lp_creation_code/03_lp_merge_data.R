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

# create share vars, in line with ADH 
share_denom_all <- function(dt, var) {
  make_share(dt, var, "resident_emp")
  make_share(dt, var, "workplace_emp")
  make_share(dt, var, "outside_jobs")
  make_share(dt, var, "population")
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
# acs_1y <- fread(paste0(path, "/acs/acs_1y_2005_2024_commuting.csv"))

# QCEW
qcew <- fread(paste0(path, "/output/lp_weighted_qcew.csv"))
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

# characteristics of migration patterns
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

# State FIPS for clustering
reg[, statefip :=
      floor(as.integer(area_fips) / 1000)]

# Predetermined population
# Save
fwrite(
  reg,
  paste0(path, "/output/lp_merged.csv"))

