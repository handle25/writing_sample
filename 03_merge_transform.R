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
  make_share_diff(dt, var, "outside_jobs")
  make_share_diff(dt, var, "total_servc_jobs")
  make_share_diff(dt, var, "total_goods_jobs")
  make_share_diff(dt, var, "population")
  make_share_diff(dt, var, "labor_force")
  make_share_diff(dt, var, "l_labor_force")

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

census_lf <- fread(
  paste0(
    path, 
    "/census/DECENNIALSF12000.P012_2026-09-13T135137/DECENNIALSF12000.P012-Data.csv"), 
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
  fmutate(sh_empl_f = total_female_in_labor_force_civilian_employed /
            (total_male_in_labor_force_civilian_employed + total_female_in_labor_force_civilian_employed)*100)

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
lodes <- lodes[year %in% c(1990, 2000, 2002, 2007, 2013, 2019)]
setorder(lodes, area_fips, year)
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

# make_diff_t0(lodes, "outside_jobs", "labor_force")
make_diff_t0(lodes, "outside_earn3333_jobs", "labor_force")
make_diff_t0(lodes, "outside_age30_54_jobs", "labor_force")

# Remove LAUS variables after constructing labor-force measures
laus_vars <- setdiff(names(laus), c("area_fips", "year"))


lodes[, (laus_vars) := NULL]
lodes <- lodes[year != 2002]
lodes[year == 1990, year := 1995]
# IRS --------------------------------------------------------------------------
irs <- fread(paste0(path, "/irs/new_lp_irs_migration_full.csv"))

## immigration destination measures 
irs_detail <- fread(paste0(path, "/irs/lp_irs_destination_conditions_full.csv"))
irs <- irs[year %in% 1990:2019]

# county AGI
irs_agi <- fread(paste0(path, "/irs/lp_irs_agi_full.csv")) |> 
  fmutate(area_fips=as.integer(area_fips),
          ln_agi_per_return=log(agi_per_return))

irs_agi <- irs_agi[year %in% c(1995, 2000, 2007, 2013, 2019)]
setorder(irs_agi, area_fips, year)

irs_agi[, d_ln_agi_per_return := ln_agi_per_return - shift(ln_agi_per_return), by=area_fips]

cols <- setdiff(names(irs), c("area_fips", "year"))
irs[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

irs[year %in% 1993:1994, `:=`(new_year = 1995L, base_year = 1990L)]
irs[year %in% 1998:1999, `:=`(new_year = 2000L, base_year = 1995L)]
irs[year %in% 2005:2006, `:=`(new_year = 2007L, base_year = 2000L)]
irs[year %in% 2011:2012, `:=`(new_year = 2013L, base_year = 2007L)]
irs[year %in% 2017:2018, `:=`(new_year = 2019L, base_year = 2013L)]

irs_detail[year %in% 1993:1994, `:=`(new_year = 1995L, base_year = 1990L)]
irs_detail[year %in% 1998:1999, `:=`(new_year = 2000L, base_year = 1995L)]
irs_detail[year %in% 2005:2006, `:=`(new_year = 2007L, base_year = 2000L)]
irs_detail[year %in% 2011:2012, `:=`(new_year = 2013L, base_year = 2007L)]
irs_detail[year %in% 2017:2018, `:=`(new_year = 2019L, base_year = 2013L)]

irs <- irs[!is.na(new_year)]
irs_detail <- irs_detail[!is.na(new_year)]

irs[, year := new_year]
irs[, new_year := NULL]
irs[, base_year := NULL]
irs_detail[, year := new_year]
irs_detail[, new_year := NULL]
irs_detail[, base_year := NULL]

irs <- irs |> fgroup_by(year, area_fips) |> fsum()
irs_detail <- irs_detail |> fgroup_by(year, area_fips) |> fmean()

# Base-period population
irs <- merge(irs, acs,
             by.x=c("area_fips", "year"),
             by.y=c("area_fips", "year"),
             all.x=T)

irs <- merge(irs, irs_detail,
             by=c("area_fips", "year"),
             all.x=T)

setnames(irs, "population", "population_base_year")
irs[, population_2000 := NULL]


# Merge datasets ---------------------------------------------------------------

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


reg <- merge(reg, irs_agi,
             by=c("area_fips", "year"),
             all.x=T)


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
reg[, exemptions_net_migration := exemptions_3_inflow - exemptions_3_outflow]
reg[, returns_net_migration := returns_3_inflow - returns_3_outflow]

# Trade exposure
winsor(reg, "IPW_US")
winsor(reg, "IPW_OTH")
winsor(reg, "IPW_US_wap")
winsor(reg, "IPW_OTH_wap")
winsor(reg, "IPW_US_10yr")
winsor(reg, "IPW_OTH_10yr")

# Migration / remaining population ---------------------------------------------
reg[, avg_hh_inflow := exemptions_3_inflow / returns_3_inflow]
reg[, avg_hh_outflow := exemptions_3_outflow / returns_3_outflow]
reg[, avg_hh_diff := avg_hh_inflow - avg_hh_outflow]

reg[, pci_in := agi_3_inflow / exemptions_3_inflow]
reg[, pci_out := agi_3_outflow / exemptions_3_outflow]
reg[, pci_diff := pci_in - pci_out]

reg[, returns_migration := returns_3_outflow + returns_3_inflow]
reg[, exemptions_migration := exemptions_3_outflow + exemptions_3_inflow]

setorder(reg, area_fips, year)

# Differences
make_share_diff(reg, "exemptions_net_migration", "exemptions")
make_share_diff(reg, "exemptions_net_migration", "population")
make_share_diff(reg, "returns_net_migration", "population")
make_share_diff(reg, "returns_3_outflow", "returns")
make_share_diff(reg, "returns_net_migration", "population")
make_share_diff(reg, "exemptions_net_migration", "population")
make_share_diff(reg, "returns_3_outflow", "returns_migration")
make_share_diff(reg, "exemptions_3_outflow", "exemptions")
make_share_diff(reg, "exemptions_3_outflow", "population")
make_share_diff(reg, "exemptions_3_outflow", "exemptions_migration")
reg[, d_pci_in := pci_in - shift(pci_in), by = area_fips]
winsor(reg, "d_pci_in")

# Labor force / non-migrants ----------------------------------------------------
make_share_diff(reg, "labor_force", "returns")
make_share_diff(reg, "labor_force", "exemptions")

reg[, net_outmigration_share_exemptions := (exemptions_3_outflow - exemptions_3_inflow) / exemptions * 100]
reg[, d_net_outmigration_share_exemptions := net_outmigration_share_exemptions - shift(net_outmigration_share_exemptions), by=area_fips]
winsor(reg, "d_net_outmigration_share_exemptions")
# Net outmigration / population ------------------------------------------------
reg[, net_outmigration_share_population := (exemptions_3_outflow - exemptions_3_inflow) / population * 100]
reg[, d_net_outmigration_share_population := net_outmigration_share_population - shift(net_outmigration_share_population), by=area_fips]
winsor(reg, "d_net_outmigration_share_population")
# Labor force growth -----------------------------------------------------------
reg[, ln_labor_force := log(labor_force)]
reg[, d_ln_labor_force := ln_labor_force - shift(ln_labor_force), by=area_fips]
winsor(reg, "d_ln_labor_force")
reg[, ln_population := log(population)]
reg[, d_ln_population := ln_population - shift(ln_population), by=area_fips]
winsor(reg, "d_ln_population")
reg[, l_labor_force := shift(labor_force), by = area_fips]

# LODES composition outcomes ---------------------------------------------------

# Services
diff_denom_all(reg, "outside_servc_jobs")
diff_denom_all(reg, "total_servc_jobs")

# Goods
diff_denom_all(reg, "outside_goods_jobs")
diff_denom_all(reg, "total_goods_jobs")

# Overall commuting
# diff_denom_all(reg, "outside_jobs")

# Earnings
diff_denom_all(reg, "outside_earn1250_jobs")
diff_denom_all(reg, "outside_earn1251_3333_jobs")
diff_denom_all(reg, "outside_earn3333_jobs")
winsor(reg, "outside_jobs_share_labor_force")
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

# county conditions ------------------------------------------------------------
county_conditions <- reg[year %in% c(2007, 2013), .(
  IPW_US=mean(w_IPW_US, na.rm=T),
  unemployed_share_labor_force=mean(unemployed_share_labor_force, na.rm=T),
  sh_empl_mfg=mean(sh_empl_mfg, na.rm=T),
  labor_force_share_population=mean(labor_force_share_population, na.rm=T)
), by=area_fips]

fwrite(county_conditions, paste0(path, "/output/shock_exposure.csv"))