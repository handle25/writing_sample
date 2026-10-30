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

make_share_diff <- function(dt, num, denom, id = "commuting_zone_id_2000") {
  share_var <- paste0(num, "_share_", denom)
  diff_var <- paste0("d_", share_var)
  dt[, (share_var) := get(num) / get(denom) * 100]
  dt[, (diff_var) := get(share_var) - shift(get(share_var)), by = id]
  winsor(dt, diff_var)
}

make_diff_t0 <- function(dt, num, denom, id = "commuting_zone_id_2000") {
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
  dt[, (newvar) := get(var)[match(base_year, year)], by = commuting_zone_id_2000]
}

recode_fips_2000 <- function(dt, var = "area_fips") {
  dt[get(var) == "02063", (var) := "02261"] # Chugach -> Valdez-Cordova
  dt[get(var) == "02066", (var) := "02261"] # Copper River -> Valdez-Cordova
  dt[get(var) == "02105", (var) := "02232"] # Hoonah-Angoon -> Skagway-Hoonah-Angoon
  dt[get(var) == "02158", (var) := "02270"] # Kusilvak -> Wade Hampton
  dt[get(var) == "02195", (var) := "02280"] # Petersburg -> Wrangell-Petersburg
  dt[get(var) == "02198", (var) := "02201"] # Prince of Wales-Hyder -> Prince of Wales-Outer Ketchikan
  dt[get(var) == "02230", (var) := "02232"] # Skagway -> old combined area
  dt[get(var) == "02275", (var) := "02280"] # Wrangell -> Wrangell-Petersburg
  dt[get(var) == "46102", (var) := "46113"] # Oglala Lakota -> Shannon
}

# Population -------------------------------------------------------------------

acs <- fread(paste0(path, "/output/population_1995_2023_CZ.csv"))

crosswalk <- read_excel("cz00eqvv1.xls") |> 
  data.table() |> 
  clean_names() |> 
  fselect(fips, commuting_zone_id_2000) |> 
  fmutate(
    fips = sprintf("%05d", as.integer(fips)),
    state = as.integer(substr(fips, 1, 2))
  ) |>
  fgroup_by(fips, commuting_zone_id_2000) |>
  fsummarize(state_cz = flast(state))

# census controls --------------------------------------------------------------
census <- fread(
  paste0(
    path, 
    "/census/DECENNIALDPSF42000.DP2_2026-09-09T191530/DECENNIALDPSF42000.DP2-Data.csv"), 
  skip = 1) |> clean_names() 


census[, area_fips := sprintf("%05d", (as.integer(substr(geography, nchar(geography) - 4, nchar(geography)))))]

census <- merge(census, crosswalk, 
                by.x = "area_fips", by.y = "fips", all.x = T)


census <- census |> 
  fmutate(
    foreign_born = as.numeric(number_nativity_and_place_of_birth_total_population_foreign_born),
    population = as.numeric(number_nativity_and_place_of_birth_total_population),
    college = as.numeric(number_educational_attainment_population_25_years_and_over_percent_bachelors_degree_or_higher),
    pop_25 = as.numeric(number_educational_attainment_population_25_years_and_over)
  ) |> 
  fgroup_by(commuting_zone_id_2000) |> 
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
  fselect(commuting_zone_id_2000, sh_popfborn, sh_popedu_c)

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

qcew[, area_fips := sprintf("%05d", area_fips)]

#filter statewide aggregates 
qcew <- qcew[
  substr(area_fips, 3, 5) != "999" &
    !substr(area_fips, 1, 2) %in% c("72", "78")
]
recode_fips_2000(qcew)

qcew <- merge(qcew, crosswalk, 
              by.x = "area_fips",
              by.y = "fips",
              all.x = T)

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
  fmutate(area_fips = sprintf("%05d",as.integer(
    paste0(state_fips_code, county_fips_code))), 
    year = as.integer(year))

recode_fips_2000(laus) 

laus <- merge(laus, crosswalk, 
              by.x = "area_fips",
              by.y = "fips", 
              all.x=T)

laus <- laus |> fgroup_by(commuting_zone_id_2000, year) |> 
  fsummarize(employed = fsum(employed), 
             unemployed = fsum(unemployed), 
             labor_force = fsum(labor_force))|> 
  fmutate(unemployment_rate_percent = unemployed / labor_force) 

# LODES ------------------------------------------------------------------------
lodes <- fread(
  paste0(path,"/output/lp_new_lodes_collapsed_all_no_crosswalk.csv"))

setnames(lodes, "county", "area_fips")
lodes[, area_fips := sprintf("%05d", area_fips)]
recode_fips_2000(lodes)
lodes[area_fips == "02231", area_fips := "02232"] # Skagway-Yakutat-Angoon -> Skagway-Hoonah-Angoon

lodes <- merge(lodes, crosswalk, 
               by.x = "area_fips", 
               by.y = "fips", all.x = T)

# Collapse LODES to CZ
sum_vars <- names(lodes)[
  !names(lodes) %in% c("area_fips", "year", "state_str", "commuting_zone_id_2000", "state_cz") &
    !grepl("^outside_d_", names(lodes))
]

lodes <- lodes[, lapply(.SD, sum, na.rm = TRUE),
               by = .(commuting_zone_id_2000, year),
               .SDcols = sum_vars]

make_base_year(lodes, "outside_jobs", 2000)


# Long-difference years only
lodes <- lodes[year %in% c(1990, 2002, 2007, 2013)]
setnames(lodes, "total_jobs", "resident_emp")

# Merge population
lodes <- merge(
  lodes,
  acs,
  by = c("commuting_zone_id_2000", "year"),
  all.x = TRUE
)

make_share_diff(lodes, "outside_jobs", "population")
make_diff_t0(lodes, "outside_jobs", "population")
make_base_year(lodes, "population")
lodes[, outside_jobs_population_2000 := outside_jobs_2000 /population_2000]
setorder(lodes, commuting_zone_id_2000, year)
# Remove population variables before later merge
lodes[, c("population", "population_2000") := NULL]

# Labor force denominator -------------------------------------------------------
lodes <- merge(
  lodes,
  laus,
  by = c("commuting_zone_id_2000", "year"),
  all.x = TRUE
)

setorder(lodes, commuting_zone_id_2000, year)

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
laus_vars <- setdiff(names(laus), c("commuting_zone_id_2000", "year"))


lodes[, (laus_vars) := NULL]
lodes[year==2002, year:= 2000]

# IRS --------------------------------------------------------------------------
irs <- fread(paste0(path, "/irs/lp_irs_migration_full.csv"))
irs <- irs[year %in% 1990:2012]
irs[, area_fips := sprintf("%05d", area_fips)]
irs <- merge(irs, crosswalk, 
             by.x = "area_fips", 
             by.y = "fips", 
             all.x = T)

irs[, state_cz := NULL]
#convert to numeric 
cols <- setdiff(names(irs), c("commuting_zone_id_2000", "year"))
irs[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

# Collapse IRS migration data to CZ
sum_vars <- names(irs)[
  !names(irs) %in% c("area_fips", "year", "state_str", "state_cz", "commuting_zone_id_2000") 
]

irs <- irs[, lapply(.SD, sum, na.rm = TRUE),
               by = .(commuting_zone_id_2000, year),
               .SDcols = sum_vars]

irs[year %in% 1993:1994, `:=`(new_year = 1995L, base_year = 1990L)]
irs[year %in% 1998:1999, `:=`(new_year = 2000L, base_year = 1995L)]
irs[year %in% 2005:2006, `:=`(new_year = 2007L, base_year = 2000L)]
irs[year %in% 2011:2012, `:=`(new_year = 2013L, base_year = 2007L)]

irs <- irs[!is.na(new_year)]
irs[, year := new_year]

irs <- irs |> 
  fgroup_by(new_year, base_year, year, commuting_zone_id_2000) |> 
  fsum()

# Base-period population
irs <- merge(
  irs, acs,
  by.x = c("commuting_zone_id_2000", "base_year"),
  by.y = c("commuting_zone_id_2000", "year"),
  all.x = TRUE
)

setnames(irs, "population", "population_base_year")

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
setorder(irs, commuting_zone_id_2000, year)

diff_vars <- c(
  "avg_hh_inflow", "avg_hh_outflow", "avg_hh_diff",
  "pci_in", "pci_out", "pci_diff",
  "net_outmigration", "net_inmigration"
)

irs[, paste0("d_", diff_vars) := lapply(.SD, \(x) x - shift(x)),
    by = commuting_zone_id_2000, .SDcols = diff_vars]

# Winsorize
winsor_vars <- c(
  "inflow_share_population_t0",
  "outflow_share_population_t0",
  "avg_hh_inflow",
  "avg_hh_outflow",
  "pci_in",
  "pci_out",
  "pci_diff"
)

for (v in winsor_vars) winsor(irs, v)


################################################################################
# Merge datasets
################################################################################

reg <- merge(
  qcew,
  lodes,
  by = c("commuting_zone_id_2000", "year"),
  all = TRUE
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
  laus,
  by = c("commuting_zone_id_2000", "year"),
  all.x = TRUE
)

reg <- merge(
  reg,
  acs,
  by = c("commuting_zone_id_2000", "year"),
  all.x = TRUE
)

setorder(
  reg,
  commuting_zone_id_2000,
  year
)

reg <- merge(reg, census, 
             by = "commuting_zone_id_2000", 
             all.x = T)

# Regression vars --------------------------------------------------------------
# State FIPS for clustering
reg[, statefip := state_cz]

# Manufacturing share
# d_sh_empl_mfg already constructed in QCEW

# Labor force participation
make_share_diff(reg, "labor_force", "population")

# Unemployment rate
make_share_diff(reg, "unemployed", "labor_force")

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
    "/output/transformed_reg_CZ.csv"
  )
)

fwrite(reg, paste0(local, "/output/transformed_reg_CZ.csv"))