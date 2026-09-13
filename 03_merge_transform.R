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
  q <- quantile(dt[[var]], probs = c(p, 1 - p), na.rm = TRUE)
  
  w_var <- paste0("w_", var)
  
  dt[, (w_var) := pmin(pmax(get(var), q[1]), q[2])]
}

make_share_diff <- function(dt, num, denom, id = "area_fips") {
  share_var <- paste0(num, "_share_", denom)
  diff_var <- paste0("d_", share_var)
  dt[, (share_var) := get(num) / get(denom) * 100]
  dt[, (diff_var) := get(share_var) - shift(get(share_var)), by = id]
  winsor(dt, diff_var)
}

make_diff_t0 <- function(dt, num, denom, id = "area_fips") {
  diff_var <- paste0("d_", num, "_share_", denom, "_t0")
  dt[, (diff_var) := (get(num) - shift(get(num))) / shift(get(denom)) * 100, by = id]
  winsor(dt, diff_var)
}

diff_denom_all <- function(dt, var) { 
  make_share_diff(dt, var, "resident_emp")
  make_share_diff(dt, var, "workplace_emp")
  make_share_diff(dt, var, "outside_jobs")
  make_share_diff(dt, var, "total_servc_jobs")
  make_share_diff(dt, var, "total_goods_jobs")
  make_share_diff(dt, var, "population")
  make_share_diff(dt, var, "labor_force")

}

make_base_year <- function(dt, var, base_year = 2000) {
  newvar <- paste0(var, "_", base_year)
  dt[, (newvar) := get(var)[match(base_year, year)], by = area_fips]
}

# Population -------------------------------------------------------------------
acs <- fread(paste0(path, "/acs/population_1995_2023.csv"))

make_base_year(acs, "population")

# census controls --------------------------------------------------------------
census <- fread(
  paste0(
    path, 
    "/census/DECENNIALDPSF42000.DP2_2026-09-09T191530/DECENNIALDPSF42000.DP2-Data.csv"),
  skip = 1) |> clean_names() 

census_f <- fread(
  paste0(
    path, 
    "/census/DECENNIALSF32000.P043_2026-09-12T133538/DECENNIALSF32000.P043-Data.csv"), 
  skip = 1) |> clean_names() 

census_f[, area_fips := as.integer(substr(geography, nchar(geography) - 4, nchar(geography)))]
cols <- setdiff(names(census_f)[sapply(census_f, is.numeric)], "area_fips")
census_f <- census_f[, lapply(.SD, sum, na.rm = TRUE), by = area_fips, .SDcols = cols]

census[, area_fips := as.integer(substr(geography, nchar(geography) - 4, nchar(geography)))]

census <- census |> 
  fmutate(
    foreign_born = as.numeric(number_nativity_and_place_of_birth_total_population_foreign_born),
    population = as.numeric(number_nativity_and_place_of_birth_total_population),
    college = as.numeric(number_educational_attainment_population_25_years_and_over_percent_bachelors_degree_or_higher),
    pop_25 = as.numeric(number_educational_attainment_population_25_years_and_over)
  ) |> 
  fgroup_by(area_fips) |> 
  fsummarize(
    foreign_born = fsum(foreign_born, na.rm = TRUE),
    cps_population = fsum(population, na.rm = TRUE),
    college = fsum(college, na.rm = TRUE),
    pop_25 = fsum(pop_25, na.rm = TRUE)
  ) |> 
  fmutate(
    sh_popfborn = foreign_born / cps_population * 100,
    sh_popedu_c = college / pop_25 * 100
  )

census <- census |> 
  fselect(area_fips, sh_popfborn, sh_popedu_c)
census <- merge(census, census_f, 
                by = "area_fips", all = T) |> 
  fmutate(sh_empl_f = total_female_in_labor_force /
            (total_male_in_labor_force + total_female_in_labor_force))

summary(census$sh_popedu_c)
summary(census$sh_popfborn)

# Read datasets ----------------------------------------------------------------
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
  paste0(path,"/output/lp_new_lodes_collapsed_all_no_crosswalk.csv"))

setnames(lodes, "county", "area_fips")

make_base_year(lodes, "outside_jobs", 2000)

# Long-difference years only
lodes <- lodes[year %in% c(1990, 2002, 2007, 2013)]
setnames(lodes, "total_jobs", "resident_emp")

# Merge population
lodes <- merge(
  lodes,
  acs,
  by = c("area_fips", "year"),
  all.x = TRUE
)

make_share_diff(lodes, "outside_jobs", "population")
make_diff_t0(lodes, "outside_jobs", "population")
lodes[, outside_jobs_population_2000 := outside_jobs_2000 /population_2000]
setorder(lodes, area_fips, year)
# Remove population variables before later merge
lodes[, c("population", "population_2000") := NULL]

# Labor force denominator -------------------------------------------------------
lodes <- merge(
  lodes,
  laus,
  by = c("area_fips", "year"),
  all.x = TRUE
)

setorder(lodes, area_fips, year)

# Standard outside-jobs / labor-force share and long difference
make_share_diff(lodes, "outside_jobs", "labor_force")
make_share_diff(lodes, "outside_earn3333_jobs", "labor_force")
make_share_diff(lodes, "outside_age30_54_jobs", "labor_force")

make_share_diff(lodes, "outside_earn3333_jobs", "outside_jobs")
make_share_diff(lodes, "outside_age30_54_jobs", "outside_jobs")

make_diff_t0(lodes, "outside_jobs", "labor_force")
make_diff_t0(lodes, "outside_earn3333_jobs", "labor_force")
make_diff_t0(lodes, "outside_age30_54_jobs", "labor_force")

# Remove LAUS variables after constructing labor-force measures
laus_vars <- setdiff(names(laus), c("area_fips", "year"))


lodes[, (laus_vars) := NULL]
lodes[year==2002, year:= 2000]

# IRS --------------------------------------------------------------------------
irs <- fread(paste0(path, "/irs/lp_irs_migration_full.csv"))
irs <- irs[year %in% 1990:2012]

cols <- setdiff(names(irs), c("area_fips", "year"))
irs[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

irs[year %in% 1993:1994, `:=`(new_year = 1995L, base_year = 1990L)]
irs[year %in% 1998:1999, `:=`(new_year = 2000L, base_year = 1995L)]
irs[year %in% 2005:2006, `:=`(new_year = 2007L, base_year = 2000L)]
irs[year %in% 2011:2012, `:=`(new_year = 2013L, base_year = 2007L)]

irs <- irs[!is.na(new_year)]
irs[, year := new_year]

irs <- irs |> 
  fgroup_by(new_year, base_year, year, area_fips) |> 
  fsum()

# Base-period population
irs <- merge(
  irs, acs,
  by.x = c("area_fips", "base_year"),
  by.y = c("area_fips", "year"),
  all.x = TRUE
)


setnames(irs, "population", "population_base_year")
irs[, population_2000 := NULL]

# IRS outcomes -----------------------------------------------------------------
irs[, inflow_share_population_t0 := exemptions_1_inflow / population_base_year * 100]
irs[, outflow_share_population_t0 := exemptions_1_outflow / population_base_year * 100]

irs[, avg_hh_inflow := exemptions_3_inflow / returns_3_inflow]
irs[, avg_hh_outflow := exemptions_3_outflow / returns_3_outflow]
irs[, avg_hh_diff := avg_hh_inflow - avg_hh_outflow]

irs[, pci_in := agi_3_inflow / exemptions_3_inflow]
irs[, pci_out := agi_3_outflow / exemptions_3_outflow]
irs[, pci_diff := pci_in - pci_out]

irs[, net_outmigration := returns_3_outflow / (returns_3_outflow + returns_3_inflow) * 100]
irs[, net_inmigration := returns_3_inflow / (returns_3_outflow + returns_3_inflow) * 100]

# Differences
setorder(irs, area_fips, year)

diff_vars <- c(
  "avg_hh_inflow", "avg_hh_outflow", "avg_hh_diff",
  "pci_in", "pci_out", "pci_diff",
  "net_outmigration", "net_inmigration"
)

irs[, paste0("d_", diff_vars) := lapply(.SD, \(x) x - shift(x)),
    by = area_fips, .SDcols = diff_vars]

# Winsorize
winsor_vars <- c(
  "inflow_share_population_t0",
  "outflow_share_population_t0",
  "avg_hh_inflow",
  "avg_hh_outflow",
  "pci_in",
  "pci_out",
  "pci_diff",
  "d_net_outmigration"
)

for (v in winsor_vars) winsor(irs, v)


################################################################################
# Merge datasets
################################################################################

reg <- merge(
  qcew,
  lodes,
  by = c("area_fips", "year"),
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

reg <- merge(reg, census, 
             by = "area_fips", 
             all.x = T)

# Regression vars --------------------------------------------------------------
# State FIPS for clustering
reg[, statefip := floor(as.integer(area_fips) / 1000)]

# Manufacturing share
# d_sh_empl_mfg already constructed in QCEW

# Labor force participation
make_share_diff(reg, "labor_force", "population")

# Unemployment rate
make_share_diff(reg, "unemployed", "labor_force")
make_diff_t0(reg, "unemployed", "labor_force")
# Migration
reg[, net_migration := exemptions_2_inflow - exemptions_2_outflow]
reg[, net_migration_share_population_t0 := net_migration / population_base_year * 100]
winsor(reg, "net_migration_share_population_t0")

# Trade exposure
winsor(reg, "IPW_US")
winsor(reg, "IPW_OTH")
winsor(reg, "IPW_US_10yr")
winsor(reg, "IPW_OTH_10yr")
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

county_conditions <- reg[, .(
  area_fips,
  year,
  IPW_US = w_IPW_US,
  unemployment_rate,
  sh_empl_mfg,
  labor_force_share_population
)]
