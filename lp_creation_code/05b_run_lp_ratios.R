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

# qcewdata 

date <- Sys.Date()
path <- "D:/writing_sample/data"
figs <- "D:/writing_sample/figures"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"

reg <- fread(paste0(path, "/output/lp_transformed_reg.csv"))
source(paste0(local, "/../code/writing_sample/utilities.R"))
source(paste0(local, "/../code/writing_sample/00_load_workspace.R"))
setwd(path)
# function definition ----------------------------------------------------------
# function definition ----------------------------------------------------------
reg[, z_IPW_US := w_IPW_US / sd(w_IPW_US,na.rm=T)]
reg[, z_IPW_OTH := w_IPW_OTH / sd(w_IPW_OTH,na.rm=T)]

reg[, z_ex_mean_work_IPW_US :=
      w_ex_mean_work_IPW_US / sd(w_ex_mean_work_IPW_US,na.rm=T)]

reg[, z_ex_mean_work_IPW_OTH :=
      w_ex_mean_work_IPW_OTH / sd(w_ex_mean_work_IPW_OTH,na.rm=T)]

state_crosswalk <- data.table(
  state = c(
    1,2,4,5,6,8,9,10,12,13,15,16,17,18,19,20,21,22,23,24,
    25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,
    44,45,46,47,48,49,50,51,53,54,55,56
  ),
  state_str = state.abb,
  region = state.region,
  division = state.division
)

#destination-condition LPs ----------------------------------------------------
reg[, outside_jobs_share_returns := outside_jobs / returns]
winsor(reg, "outside_jobs_share_returns")

run_lp_ratio(reg, "ew_share_into_less_unemp", start_year=2000, end_year=2007)
run_lp_ratio(reg, "ew_share_into_less_unemp", start_year=2000, end_year=2015, 
       shock = "w_ex_mean_work_IPW_US", 
       instrument ="w_ex_mean_work_IPW_OTH")

run_lp_ratio(reg, "ew_share_into_less_exposed", start_year=2000, end_year=2015,
             controls = "+ex_mean_work_IPW_US",
             shock = "w_fn_mean_work_IPW_US", 
             instrument ="w_fn_mean_work_IPW_OTH")

run_lp_ratio(reg, "ew_share_into_less_exposed", start_year=2000, end_year=2015,
             controls = "+ex_mean_work_IPW_US",
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")


run_lp_ratio_multiple_shocks(reg, "ew_share_into_less_unemp", start_year=2000, end_year=2015,
             shock_1="z_IPW_US",
             shock_2="z_ex_mean_work_IPW_US",
             instrument_1="z_IPW_OTH",
             instrument_2="z_ex_mean_work_IPW_OTH")


# main outcomes ----------------------------------------------------------------
reg[, outside_jobs_share_returns := outside_jobs / returns ]
## Labor market / population ---------------------------------------------------
################################################################################
run_lp_ratio(reg, "labor_force_share_population", start_year=2000, end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")
################################################################################

# run_lp_ratio(reg, "labor_force_share_population", start_year=2000, end_year=2007,
#        controls = "+ex_mean_work_IPW_US")
# 
# 
# run_lp_ratio(reg, "labor_force_share_population", start_year=2000, end_year=2015,
#              shock = "w_fn_mean_work_IPW_US", 
#              instrument ="w_fn_mean_work_IPW_OTH")
# 
# 
# run_lp_ratio(reg, "labor_force_share_population", start_year=2000, end_year=2015,
#              shock = "w_fn_mean_neighbor_IPW_US", 
#              instrument ="w_fn_mean_neighbor_IPW_OTH")
# 
# run_lp_ratio(reg, "labor_force_share_population", start_year=2000, end_year=2015,
#              shock = "w_ex_mean_work_IPW_US", 
#              instrument ="w_ex_mean_work_IPW_OTH")
# 
# run_lp_ratio(reg, "labor_force_share_population", start_year=2000, end_year=2015,
#              shock = "w_ex_mean_neighbor_IPW_US", 
#              instrument ="w_ex_mean_neighbor_IPW_OTH")
# 
# run_lp_ratio_multiple_shocks(reg, "labor_force_share_population", start_year=2000, end_year=2015,
#                              shock_1="z_IPW_US",
#                              shock_2="z_ex_mean_work_IPW_US",
#                              instrument_1="z_IPW_OTH",
#                              instrument_2="z_ex_mean_work_IPW_OTH")
# 
# reg[, labor_force_share_exemptions := labor_force / exemptions]
# winsor(reg, "labor_force_share_exemptions")
# run_lp_ratio_multiple_shocks(reg, "w_labor_force_share_exemptions", start_year=2000, end_year=2015,
#                              shock_1="z_IPW_US",
#                              shock_2="z_ex_mean_work_IPW_US",
#                              instrument_1="z_IPW_OTH",
#                              instrument_2="z_ex_mean_work_IPW_OTH")

## Unemployment rates ----------------------------------------------------------
# baseline 
run_lp_ratio(reg, "unemployed_share_labor_force", start_year=2000, end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")
# full network shock 
run_lp_ratio(reg, "unemployed_share_labor_force", start_year=2000, end_year=2015,
             shock = "w_fn_mean_work_IPW_US", 
             instrument ="w_fn_mean_work_IPW_OTH")
# external county shock  
run_lp_ratio(reg, "unemployed_share_labor_force", start_year=2000, end_year=2015,
             shock = "w_ex_mean_work_IPW_US", 
             instrument ="w_ex_mean_work_IPW_OTH")

run_lp_ratio_multiple_shocks(reg, "w_unemployed_share_labor_force", start_year=2000, end_year=2015,
                             shock_1="z_IPW_US",
                             shock_2="z_ex_mean_work_IPW_US",
                             instrument_1="z_IPW_OTH",
                             instrument_2="z_ex_mean_work_IPW_OTH")


## Commuting patterns ----------------------------------------------------------
# baseline 
run_lp_ratio(reg, "w_outside_jobs_share_labor_force", start_year=2000, end_year=2010,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")
# full network shock 
run_lp_ratio(reg, "w_outside_jobs_share_labor_force", start_year=2000, end_year=2015,
             shock = "w_fn_mean_work_IPW_US", 
             instrument ="w_fn_mean_work_IPW_OTH")
# external county shock  
run_lp_ratio(reg, "w_outside_jobs_share_labor_force", start_year=2000, end_year=2015,
             shock = "w_ex_mean_work_IPW_US", 
             instrument ="w_ex_mean_work_IPW_OTH")

run_lp_ratio_multiple_shocks(reg, "w_outside_jobs_share_resident_emp", start_year=2000, end_year=2015,
                             shock_1="z_IPW_US",
                             shock_2="z_ex_mean_work_IPW_US",
                             instrument_1="z_IPW_OTH",
                             instrument_2="z_ex_mean_work_IPW_OTH")

## Migration patterns ----------------------------------------------------------
# baseline 
run_lp_ratio(reg, "exemptions_3_outflow_share_exemptions_total_migration", start_year=2000, end_year=2010,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")
# full network shock 
run_lp_ratio(reg, "w_exemptions_net_migration_share_population", start_year=2000, end_year=2015,
             shock = "w_fn_mean_work_IPW_US", 
             instrument ="w_fn_mean_work_IPW_OTH")
# external county shock  
run_lp_ratio(reg, "w_exemptions_net_migration_share_population", start_year=2000, end_year=2015,
             shock = "w_ex_mean_work_IPW_US", 
             instrument ="w_ex_mean_work_IPW_OTH")

run_lp_ratio_multiple_shocks(reg, "exemptions_3_outflow_share_exemptions_total_migration", start_year=2000, end_year=2015,
                             shock_1="z_IPW_US",
                             shock_2="z_ex_mean_work_IPW_US",
                             instrument_1="z_IPW_OTH",
                             instrument_2="z_ex_mean_work_IPW_OTH")



# Migration
run_lp_ratio(reg, "exemptions_net_migration_share_population", start_year=2000, end_year=2007)
run_lp_ratio(reg, "exemptions_3_outflow_share_exemptions_total_migration", start_year=2000, end_year=2007)

# Destination of migration
run_lp_ratio(reg, "ew_share_into_less_unemp", start_year=2000, end_year=2007)
run_lp_ratio(reg, "ew_share_into_less_exposed", start_year=2000, end_year=2007)

# Income
run_lp_ratio(reg, "ln_agi_per_return", start_year=2000, end_year=2007)
