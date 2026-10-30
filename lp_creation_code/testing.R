################################################################################
# Created 8.15.2026 
# Author: Sophie Handley 
# Purpose: Run LP regressions
################################################################################


rm(list = ls())
figure_1 <- T
figure_2 <- T
figure_3 <- T
figure_4 <- T
date <- Sys.Date()

# qcewdata 
# read in data -----------------------------------------------------------------
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")
run_date <- date
reg <- fread(paste0(path, "/output/lp_transformed_reg_", run_date, ".csv"))
# reg <- reg[year != 2009]
reg <- reg[year<2020]
reg <- reg[year>1994]

setorder(reg, area_fips, year)
reg[, baseline_resident_emp := resident_emp[year == 2000][1], by=area_fips]

reg[, l_sh_empl_mfg := shift(sh_empl_mfg), by = area_fips]
reg[, total_migration := returns_3_inflow + returns_3_outflow]
reg[, net_migration := returns_3_inflow - returns_3_outflow]
reg[, net_migration_share_population := net_migration / total_migration]
reg[, l2_ex_mean_work_IPW_US := shift(ex_mean_work_IPW_US, n = 2), by = area_fips]
reg[, l1_w_ex_mean_work_IPW_US := shift(w_ex_mean_work_IPW_US, n = 1), by = area_fips]


shocks <- grep("IPW", names(reg), value=TRUE)
for (v in shocks) reg[, (paste0("l1_", v)) := shift(get(v), 1), by=area_fips]

shocks <- grep("empw", names(reg), value=TRUE)
for (v in shocks) winsor(reg, v, p = .02)
reg[, unemployed := as.double(unemployed)]

reg[, state_fips := substr(sprintf("%05d", as.integer(as.character(area_fips))), 1, 2)]

coastal_fips <- c("01", "02", "06", "09", "10", "12", "13", "15",
                  "22", "23", "24", "25", "28", "33", "34", "36",
                  "37", "41", "44", "45", "48", "51", "53")

reg[, coastal_group := fifelse(
  is.na(state_fips), NA_character_,
  fifelse(state_fips %in% coastal_fips,
          "A. Coastal states", "B. Other states")
)]

open_var <- "outside_jobs_share_resident_emp"

base_open <- reg[
  year == 2000 & is.finite(get(open_var)),
  .(openness_2000 = mean(get(open_var))),
  by = area_fips
]

median_open <- median(base_open$openness_2000)

base_open[, openness_group := fifelse(
  openness_2000 > median_open,
  "High openness", "Low openness"
)]

reg[, c("openness_2000", "openness_group") := NULL]

reg[base_open, on = "area_fips", `:=`(
  openness_2000 = i.openness_2000,
  openness_group = i.openness_group
)]


setorder(reg, area_fips, year)

# Fill missing 2001 commuting values using each county's 2000 value.
reg[reg[year == 2000, .(area_fips, value_2000 = com_outside_jobs)],
    on = .(area_fips),
    com_outside_jobs := fifelse(
      year == 2001 & is.na(com_outside_jobs),
      i.value_2000,
      com_outside_jobs
    )]

commuting_two_shock <- run_lp_multiple_shocks(
  reg,
  outcome = "com_outside_jobs",
  denominator = "labor_force",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock_1+population",
  start_year = 2000,
  end_year = 2015,
  horizons = 0:7,
  table_horizons = 0:7,
  height = 4,
  width = 7
)


