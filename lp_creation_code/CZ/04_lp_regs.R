# Setup ------------------------------------------------------------------------
rm(list=ls())
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")
date <- Sys.Date()

# Read CZ panel ----------------------------------------------------------------
reg <- fread(paste0(path, "/output/lp_transformed_reg_CZ.csv"))[year < 2017]
reg[, test := log(resident_emp)]
reg[, total_jobs := total_goods_jobs + total_servc_jobs + total_trade_jobs]
unit <- "commuting_zone_id_2000"

# CZ local projections ---------------------------------------------------------
# run_lp_ratio is for outcomes already expressed as shares or ratios.
# Note: unlike the old standalone run_lp, this utility winsorizes each
# horizon's outcome and does not automatically include lagged import exposure.
results <- run_lp_ratio(reg, "w_manuf_share_emp", unit=unit,
                        start_year=2000, end_year=2007)

# Example: two endogenous exposures and two instruments
# Use shock names that actually exist in your CZ regression panel.
results_two <- run_lp_ratio_multiple_shocks(
  reg, "w_manuf_share_emp", unit=unit,
  shock_1="w_IPW_US", shock_2="empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH", instrument_2="empw_neighbor_IPW_OTH",
  start_year=2000, end_year=2015)

# Additional outcomes (uncomment as needed) -----------------------------------
run_lp_ratio(reg, "w_outside_jobs_share_resident_emp", unit=unit,
             start_year=2002, end_year=2015)
run_lp_ratio(reg, "w_net_migration_share_population", unit=unit,
             start_year=2000, end_year=2015)
# run_lp_ratio(reg, "w_manuf_emp_share_pop", unit=unit,
#              start_year=2000, end_year=2007)

# Optional CZ maps -------------------------------------------------------------
# Retain your original mapping section separately; it is visualization,
# not an LP function and requires cz_map.rds and mapped outcome columns.
