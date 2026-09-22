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
path <- "D:/writing_sample/data"
figs <- "D:/writing_sample/figures"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
setwd(path)
source(paste0(local, "/../code/writing_sample/utilities.R"))
# read in data -----------------------------------------------------------------
reg <- fread(paste0(path, "/output/lp_transformed_reg.csv"))
setorder(reg, area_fips, year)
# reg <- reg[year != 2009]
reg <- reg[year<2020]

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

# run regressions --------------------------------------------------------------
run_lp_lagged_denom(reg, outcome="outside_jobs", 
                    denominator="resident_emp",
                    controls="+l1_own_shock+w_ex_mean_work_IPW_US",
                    start_year = 1998, 
                    end_year = 2008)

run_lp_lagged_denom(reg, outcome="exemptions_3_outflow", 
                    denominator="exemptions",
                    controls="+l1_own_shock+w_ex_mean_work_IPW_US",
                    start_year = 1997, 
                    end_year = 2020)

## unemployment ----------------------------------------------------------------
run_lp_lagged_denom(reg, outcome="unemployed", 
                    denominator="labor_force",
                    controls="+l1_own_shock+w_ex_mean_work_IPW_US",
                    start_year = 1998, 
                    end_year = 2015)


lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="unemployed",
  denominator="labor_force", # or resident_emp
  shock_1="w_IPW_US",
  shock_2="w_l1_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l1_empw_neighbor_IPW_OTH",
  controls="+l1_own_shock",
  start_year=2000,
  end_year=2015
)

lf_multi <- run_lp_ratio_multiple_shocks(
  reg,
  outcome="unemployed_share_labor_force",
  shock_1="w_IPW_US",
  shock_2="w_l1_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l1_empw_neighbor_IPW_OTH",
  start_year=2000,
  end_year=2015
)

## Labor force -----------------------------------------------------------------
run_lp_ratio(reg, "labor_force_share_population", start_year=1998, end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")

lf_multi <- run_lp_ratio_multiple_shocks(
  reg,
  outcome="labor_force_share_population",
  shock_1="w_IPW_US",
  shock_2="w_l1_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l1_empw_neighbor_IPW_OTH",
  start_year=2000,
  end_year=2015
)

# run_lp_lagged_denom(reg, outcome="labor_force", 
#                     denominator="population",
#                     controls="+l1_own_shock+w_ex_mean_work_IPW_US",
#                     start_year = 1998, 
#                     end_year = 2015)
# 
# lf_multi <- run_lp_multiple_shocks(
#   reg,
#   outcome="labor_force",
#   denominator="population", # or resident_emp
#   shock_1="w_IPW_US",
#   shock_2="w_l1_empw_neighbor_IPW_US",
#   instrument_1="w_IPW_OTH",
#   instrument_2="w_l1_empw_neighbor_IPW_OTH",
#   controls="+l1_own_shock",
#   start_year=2000,
#   end_year=2015
# )

## Commuting -------------------------------------------------------------------
run_lp_ratio(reg, "w_outside_jobs_share_labor_force", 
             start_year=2000, 
             end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="outside_jobs",
  denominator="resident_emp", # or resident_emp
  shock_1="w_IPW_US",
  shock_2="w_l1_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l1_empw_neighbor_IPW_OTH",
  controls="+l1_own_shock_1+l1_own_shock_2",
  start_year=2000,
  end_year=2015
)

run_lp_lagged_denom(reg, outcome="outside_jobs", 
                    denominator="resident_emp",
                    controls="+l1_own_shock", # removed w_ex_mean_work_IPW_US 
                    start_year = 2000, 
                    end_year = 2015)

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="outside_jobs",
  denominator="resident_emp", # or resident_emp
  shock_1="w_IPW_US",
  shock_2="w_l1_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l1_empw_neighbor_IPW_OTH",
  controls="+l1_own_shock_1+l1_own_shock_2",
  start_year=2000,
  end_year=2015
)

## Migration -------------------------------------------------------------------
run_lp_ratio(reg, "w_outside_jobs_share_labor_force", 
             start_year=2000, 
             end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="outside_jobs",
  denominator="resident_emp", # or resident_emp
  shock_1="w_IPW_US",
  shock_2="w_l1_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l1_empw_neighbor_IPW_OTH",
  controls="+l1_own_shock_1+l1_own_shock_2",
  start_year=2000,
  end_year=2015
)

run_lp_lagged_denom(reg, outcome="outside_jobs", 
                    denominator="resident_emp",
                    controls="+l1_own_shock", # removed w_ex_mean_work_IPW_US 
                    start_year = 2000, 
                    end_year = 2015)

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="outside_jobs",
  denominator="resident_emp", # or resident_emp
  shock_1="w_IPW_US",
  shock_2="w_l1_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l1_empw_neighbor_IPW_OTH",
  controls="+l1_own_shock_1+l1_own_shock_2",
  start_year=2000,
  end_year=2015
)
