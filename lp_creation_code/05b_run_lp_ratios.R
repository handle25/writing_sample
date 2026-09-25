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
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")

# qcewdata 
date <- Sys.Date()
reg <- fread(paste0(path, "/output/lp_transformed_reg.csv"))

reg[, log_exemptions_total_migration := log(exemptions_3_inflow+exemptions_3_outflow)]
reg[, log_exemptions_inflow := log(exemptions_3_inflow)]

reg[, log_exemptions_inflow := log1p(exemptions_3_inflow)]
reg[, log_exemptions_outflow := log1p(exemptions_3_outflow)]
reg[, log_exemptions_total_migration := log1p(exemptions_3_inflow + exemptions_3_outflow)]

run_lp_ratio(reg, "log_exemptions_inflow", start_year=1998, end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")

reg[, z_IPW_US := w_IPW_US / sd(w_IPW_US,na.rm=T)]
reg[, z_IPW_OTH := w_IPW_OTH / sd(w_IPW_OTH,na.rm=T)]

reg[, z_ex_mean_work_IPW_US :=
      w_ex_mean_work_IPW_US / sd(w_ex_mean_work_IPW_US,na.rm=T)]

reg[, z_ex_mean_work_IPW_OTH :=
      w_ex_mean_work_IPW_OTH / sd(w_ex_mean_work_IPW_OTH,na.rm=T)]

shocks <- grep("IPW", names(reg), value = T) 
for (v in shocks) reg[, (paste0("l1_", v)) := shift(get(v), 1), by=area_fips]

#destination-condition LPs ----------------------------------------------------
reg[, outside_jobs_share_returns := outside_jobs / returns]
winsor(reg, "outside_jobs_share_returns")

run_lp_ratio(reg, "ew_share_into_less_unemp", start_year=1998, end_year=2007)
run_lp_ratio(reg, "ew_share_into_less_unemp", start_year=1998, end_year=2015, 
       shock = "w_ex_mean_work_IPW_US", 
       instrument ="w_ex_mean_work_IPW_OTH")

run_lp_ratio(reg, "ew_share_into_less_exposed", start_year=1998, end_year=2015,
             controls = "+ex_mean_work_IPW_US",
             shock = "w_fn_mean_work_IPW_US", 
             instrument ="w_fn_mean_work_IPW_OTH")

run_lp_ratio(reg, "ew_share_into_less_exposed", start_year=1998, end_year=2015,
             controls = "+ex_mean_work_IPW_US",
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")


run_lp_ratio_multiple_shocks(reg, "ew_share_into_less_unemp", start_year=1998, end_year=2015,
             shock_1="z_IPW_US",
             shock_2="l1_z_ex_mean_work_IPW_US",
             instrument_1="z_IPW_OTH",
             instrument_2="l1_z_ex_mean_work_IPW_OTH")


# main outcomes ----------------------------------------------------------------
reg[, outside_jobs_share_returns := outside_jobs / returns ]
## Labor market / population ---------------------------------------------------
################################################################################
run_lp_ratio(reg, "labor_force_share_population", start_year=1998, end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")
################################################################################
run_lp_ratio_multiple_shocks(reg, "labor_force_share_population", start_year=1998, end_year=2015,
                             shock_1="w_IPW_US",
                             shock_2="w_ex_mean_neighbor_IPW_US",
                             instrument_1="w_IPW_OTH",
                             instrument_2="w_ex_mean_neighbor_IPW_OTH")

# run_lp_ratio(reg, "labor_force_share_population", start_year=1998, end_year=2007,
#        controls = "+ex_mean_work_IPW_US")
# 
# 
# run_lp_ratio(reg, "labor_force_share_population", start_year=1998, end_year=2015,
#              shock = "w_fn_mean_work_IPW_US", 
#              instrument ="w_fn_mean_work_IPW_OTH")
# 
# 
# run_lp_ratio(reg, "labor_force_share_population", start_year=1998, end_year=2015,
#              shock = "w_fn_mean_neighbor_IPW_US", 
#              instrument ="w_fn_mean_neighbor_IPW_OTH")
# 
# run_lp_ratio(reg, "labor_force_share_population", start_year=1998, end_year=2015,
#              shock = "w_ex_mean_work_IPW_US", 
#              instrument ="w_ex_mean_work_IPW_OTH")
# 
# run_lp_ratio(reg, "labor_force_share_population", start_year=1998, end_year=2015,
#              shock = "w_ex_mean_neighbor_IPW_US", 
#              instrument ="w_ex_mean_neighbor_IPW_OTH")
# 
# run_lp_ratio_multiple_shocks(reg, "labor_force_share_population", start_year=1998, end_year=2015,
#                              shock_1="z_IPW_US",
#                              shock_2="z_ex_mean_work_IPW_US",
#                              instrument_1="z_IPW_OTH",
#                              instrument_2="z_ex_mean_work_IPW_OTH")
# 
# reg[, labor_force_share_exemptions := labor_force / exemptions]
# winsor(reg, "labor_force_share_exemptions")
# run_lp_ratio_multiple_shocks(reg, "w_labor_force_share_exemptions", start_year=1998, end_year=2015,
#                              shock_1="z_IPW_US",
#                              shock_2="z_ex_mean_work_IPW_US",
#                              instrument_1="z_IPW_OTH",
#                              instrument_2="z_ex_mean_work_IPW_OTH")

## Unemployment rates ----------------------------------------------------------
# baseline 
run_lp_ratio(reg, "unemployed_share_labor_force", start_year=1990, end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")
# # full network shock 
# run_lp_ratio(reg, "unemployed_share_labor_force", start_year=1998, end_year=2015,
#              shock = "w_fn_mean_work_IPW_US", 
#              instrument ="w_fn_mean_work_IPW_OTH")
# 
# run_lp_ratio(reg, "unemployed_share_labor_force", start_year=1998, end_year=2015,
#              shock = "w_fn_mean_neighbor_IPW_US", 
#              instrument ="w_fn_mean_neighbor_IPW_OTH")
# 
# # external county shock  
# run_lp_ratio(reg, "unemployed_share_labor_force", start_year=1998, end_year=2015,
#              shock = "w_ex_mean_work_IPW_US", 
#              instrument ="w_ex_mean_work_IPW_OTH")
# run_lp_ratio(reg, "unemployed_share_labor_force", start_year=1998, end_year=2015,
#              shock = "w_ex_mean_neighbor_IPW_US", 
#              instrument ="w_ex_mean_neighbor_IPW_OTH")

run_lp_ratio_multiple_shocks(reg, "w_unemployed_share_labor_force", start_year=1990, end_year=2015,
                             shock_1="z_IPW_US",
                             shock_2="empw_neighbor_IPW_US",
                             instrument_1="z_IPW_OTH",
                             instrument_2="empw_neighbor_IPW_OTH")

run_lp_ratio_multiple_shocks(reg, "w_unemployed_share_labor_force", start_year=1990, end_year=2015,
                             shock_1="IPW_US",
                             shock_2="ex_mean_neighbor_IPW_US",
                             instrument_1="IPW_OTH",
                             instrument_2="ex_mean_neighbor_IPW_OTH")

run_lp_ratio_multiple_shocks(reg, "w_unemployed_share_labor_force", start_year=1990, end_year=2015,
                             shock_1="w_IPW_US",
                             shock_2="w_ex_mean_neighbor_IPW_US",
                             instrument_1="w_IPW_OTH",
                             instrument_2="w_ex_mean_neighbor_IPW_OTH")


## Commuting patterns ----------------------------------------------------------
# baseline 
run_lp_ratio(reg, "w_outside_jobs_share_labor_force", start_year=1998, end_year=2010,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")

# full network shock 
run_lp_ratio(reg, "w_outside_jobs_share_labor_force", start_year=1990, end_year=2015,
             shock = "mean_neighbor_IPW_US", 
             instrument ="mean_neighbor_IPW_OTH")

# external county shock  
run_lp_ratio(reg, "w_outside_jobs_share_labor_force", start_year=1998, end_year=2015,
             shock = "w_ex_mean_work_IPW_US", 
             instrument ="w_ex_mean_work_IPW_OTH")

run_lp_ratio_multiple_shocks(reg, "w_outside_jobs_share_resident_emp", start_year=1998, end_year=2015,
                             shock_1="z_IPW_US",
                             shock_2="z_ex_mean_work_IPW_US",
                             instrument_1="z_IPW_OTH",
                             instrument_2="z_ex_mean_work_IPW_OTH")

run_lp_ratio_multiple_shocks(reg, "w_outside_jobs_share_resident_emp", start_year=1998, end_year=2015,
                             shock_1="w_IPW_US",
                             shock_2="w_ex_mean_neighbor_IPW_US",
                             instrument_1="w_IPW_OTH",
                             instrument_2="w_ex_mean_neighbor_IPW_OTH")

run_lp_ratio_multiple_shocks(reg, "com_nonadjacent_share_external", start_year=1990, end_year=2015,
                             shock_1="w_IPW_US",
                             shock_2="mean_neighbor_IPW_US",
                             instrument_1="w_IPW_OTH",
                             instrument_2="mean_neighbor_IPW_OTH")

## Migration patterns ----------------------------------------------------------
# baseline 
run_lp_ratio(reg, "exemptions_3_outflow_share_exemptions_total_migration", start_year=1998, end_year=2010,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")
# full network shock 
run_lp_ratio(reg, "w_exemptions_net_migration_share_population", start_year=1998, end_year=2015,
             shock = "w_fn_mean_work_IPW_US", 
             instrument ="w_fn_mean_work_IPW_OTH")
# external county shock  
run_lp_ratio(reg, "w_exemptions_net_migration_share_population", start_year=1998, end_year=2015,
             shock = "w_ex_mean_work_IPW_US", 
             instrument ="w_ex_mean_work_IPW_OTH")

reg[, test := returns_3_outflow/population]
run_lp_ratio_multiple_shocks(reg, "test", start_year=1990, end_year=2015,
                             shock_1="IPW_US",
                             shock_2="mean_neighbor_IPW_US",
                             instrument_1="IPW_OTH",
                             instrument_2="mean_neighbor_IPW_OTH")



# Migration
run_lp_ratio(reg, "exemptions_3_outflow_share_exemptions_net_migration", start_year=1990, end_year=2015)
run_lp_ratio(reg, "exemptions_3_outflow_share_exemptions_total_migration", start_year=1990, end_year=2015)

# Destination of migration
run_lp_ratio(reg, "ew_share_into_less_unemp", start_year=1998, end_year=2007)
run_lp_ratio(reg, "ew_share_into_less_exposed", start_year=1998, end_year=2007)

# Income
run_lp_ratio(reg, "ln_agi_per_return", start_year=1998, end_year=2007)
