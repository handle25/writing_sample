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
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")
# read in data -----------------------------------------------------------------
reg <- fread(paste0(path, "/output/lp_transformed_reg.csv"))
setorder(reg, area_fips, year)
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
stop()

# run regressions --------------------------------------------------------------
run_lp_lagged_denom(reg, outcome="outside_jobs", 
                    denominator="resident_emp",
                    controls="+l1_own_shock+w_ex_mean_work_IPW_US",
                    start_year = 1998, 
                    end_year = 2008)

run_lp_lagged_denom(reg, outcome="exemptions_3_inflow", 
                    denominator="exemptions",
                    controls="+l1_own_shock+w_ex_mean_work_IPW_US",
                    start_year = 1997, 
                    end_year = 2020)

## unemployment ----------------------------------------------------------------
run_lp_lagged_denom(reg, outcome="unemployed", 
                    denominator="labor_force",
                    controls="+l1_own_shock",
                    start_year = 2000, 
                    end_year = 2015)

run_lp_ratio(reg, "w_unemployed_share_labor_force", start_year=1998, end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")


lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="unemployed",
  denominator="labor_force", # or resident_emp
  shock_1="w_IPW_US",
  shock_2="w_l1_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l1_empw_neighbor_IPW_OTH",
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
  denominator="labor_force", # or resident_emp
  shock_1="w_IPW_US",
  shock_2="w_l1_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l1_empw_neighbor_IPW_OTH",
  controls="+l1_own_shock_1",
  start_year=2000,
  end_year=2015
)

run_lp_lagged_denom(reg, outcome="outside_jobs", 
                    denominator="labor_force",
                    controls="+l1_own_shock", # removed w_ex_mean_work_IPW_US 
                    start_year = 2000, 
                    end_year = 2015)

run_lp_ratio_multiple_shocks(
  reg,
  outcome = "outside_jobs_share_labor_force", 
  shock_1 = "w_IPW_US",
  shock_2 = "w_l1_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_l1_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 2000,
  end_year = 2015
)


## Migration -------------------------------------------------------------------
n <- 0
reg[, net_full_migration := exemptions_3_inflow  - exemptions_3_outflow ]
reg[, net_domestic_migration := exemptions_3_inflow - exemptions_3_outflow]
# Same-state migration -------------------------------------------------------
reg[, total_domestic_migration := exemptions_3_inflow + exemptions_3_outflow]
# Net migration

# Cumulative net migration
setorder(reg, area_fips, year)

reg[year >= 1995, cum_net_full_mig := cumsum(net_full_migration), by = area_fips]
reg[year >= 1995, cum_net_domestic_mig := cumsum(net_domestic_migration), by = area_fips]
reg[year >= 1995, cum_domestic_mig := cumsum(total_domestic_migration), by = area_fips]
reg[year >= 1995, cum_domestic_inflow := cumsum(exemptions_3_inflow), by = area_fips]
reg[year >= 1995, cum_domestic_outflow := cumsum(exemptions_3_outflow), by = area_fips]
reg[year >= 1995, cum_same_inflow := cumsum(exemptions_same_state_inflow), by = area_fips]
reg[year >= 1995, cum_same_outflow := cumsum(exemptions_same_state_outflow), by = area_fips]
reg[year >= 1998, cum_exemptions := cumsum(exemptions), by = area_fips]

reg[, cum_total_domestic_migration := cum_domestic_inflow + cum_domestic_outflow]
reg[, cum_net_full_mig_share_population := cum_net_full_mig / population]
reg[, cum_net_domestic_mig_share_population := cum_net_domestic_mig / population]
reg[, cum_domestic_inflow_share_population := cum_domestic_inflow / population]
reg[, cum_domestic_outflow_share_population := cum_domestic_outflow / population]
reg[, cum_same_inflow_share_population := cum_same_inflow / population]
reg[, cum_same_outflow_share_population := cum_same_outflow / population]
reg[, cum_net_full_mig_share_migration := cum_net_full_mig / cum_total_domestic_migration]
reg[, cum_net_domestic_mig_share_migration := cum_net_domestic_mig / cum_total_domestic_migration]
reg[, cum_domestic_inflow_share_migration := cum_domestic_inflow / cum_total_domestic_migration]
reg[, cum_domestic_outflow_share_migration := cum_domestic_outflow / cum_total_domestic_migration]
reg[, cum_same_inflow_share_migration := cum_same_inflow / cum_total_domestic_migration]
reg[, cum_same_outflow_share_migration := cum_same_outflow / cum_total_domestic_migration]
reg[, cum_net_full_mig_share_nonmigration := cum_net_full_mig / exemptions]
reg[, cum_net_domestic_mig_share_nonmigration := cum_net_domestic_mig / exemptions]
reg[, cum_domestic_inflow_share_nonmigration := cum_domestic_inflow / exemptions]
reg[, cum_domestic_outflow_share_nonmigration := cum_domestic_outflow / exemptions]
reg[, cum_same_inflow_share_nonmigration := cum_same_inflow / exemptions]
reg[, cum_same_outflow_share_nonmigration := cum_same_outflow / exemptions]

winsor(reg, "cum_net_full_mig_share_population", p = .01)
winsor(reg, "cum_net_domestic_mig_share_population", p = .01)
winsor(reg, "cum_domestic_inflow_share_population", p = .01)
winsor(reg, "cum_domestic_outflow_share_population", p = .01)
winsor(reg, "cum_same_inflow_share_population", p = .01)
winsor(reg, "cum_same_outflow_share_population", p = .01)
winsor(reg, "cum_net_full_mig_share_migration", p = .01)
winsor(reg, "cum_net_domestic_mig_share_migration", p = .01)
winsor(reg, "cum_domestic_inflow_share_migration", p = .01)
winsor(reg, "cum_domestic_outflow_share_migration", p = .01)
winsor(reg, "cum_same_inflow_share_migration", p = .01)
winsor(reg, "cum_same_outflow_share_migration", p = .01)
winsor(reg, "cum_net_full_mig_share_nonmigration", p = .01)
winsor(reg, "cum_net_domestic_mig_share_nonmigration", p = .01)
winsor(reg, "cum_domestic_inflow_share_nonmigration", p = .01)
winsor(reg, "cum_domestic_outflow_share_nonmigration", p = .01)
winsor(reg, "cum_same_inflow_share_nonmigration", p = .01)
winsor(reg, "cum_same_outflow_share_nonmigration", p = .01)
winsor(reg, "cum_net_domestic_mig")
winsor(reg, "cum_net_full_mig", p = .01)

# Full
lp_full_mig <- run_lp_multiple_shocks(
  reg,
  outcome = "w_cum_net_full_mig",
  denominator = "population",
  shock_1 = "w_IPW_US",
  shock_2 = "w_l1_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_l1_empw_neighbor_IPW_OTH",
  start_year = 1995,
  end_year = 2015
)



lp_full_mig <- run_lp_ratio_multiple_shocks(
  reg,
  outcome = "w_cum_domestic_outflow_share_migration",
  shock_1 = "w_IPW_US",
  shock_2 = "w_l1_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_l1_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015
)

lp_full_mig <- run_lp_multiple_shocks(
  reg,
  outcome = "cum_domestic_outflow",
  denominator = "exemptions", 
  shock_1 = "w_IPW_US",
  shock_2 = "w_l1_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_l1_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015
)

