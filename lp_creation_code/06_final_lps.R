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
  controls="+l1_own_shock_1+l1_own_shock_2",
  start_year=2000,
  end_year=2015
)

run_lp_lagged_denom(reg, outcome="outside_jobs", 
                    denominator="labor_force",
                    controls="+l1_own_shock", # removed w_ex_mean_work_IPW_US 
                    start_year = 2000, 
                    end_year = 2015)

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="outside_jobs",
  denominator="labor_force", # or resident_emp
  shock_1="w_IPW_US",
  shock_2="w_l1_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l1_empw_neighbor_IPW_OTH",
  controls="+l1_own_shock_1+l1_own_shock_2",
  start_year=2000,
  end_year=2015
)

## Migration -------------------------------------------------------------------
n <- 0
reg[, lshock_us := shift(w_IPW_US, n = n) , by = area_fips]
reg[, lshock_oth := shift(w_IPW_OTH, n = n) , by = area_fips]
reg[, lshock_nus := shift(w_empw_neighbor_IPW_US, n = n) , by = area_fips]
reg[, lshock_noth := shift(w_empw_neighbor_IPW_OTH, n = n) , by = area_fips]

reg[, exemptions_3_outflow_share_population_2000 :=exemptions_3_outflow/population_2000 ]
setorder(reg, area_fips, year)
reg[, cumulative_outflows := cumsum(exemptions_3_outflow), by = area_fips]
reg[, cumulative_inflows := cumsum(exemptions_3_inflow), by = area_fips]

reg[, cumulative_net_migration := cumsum(returns_net_migration), by = area_fips]
reg[, cumulative_total_migration := cumsum(exemptions_total_migration), by = area_fips]
reg[, coverage_gap := (population - exemptions)/population]

reg[, exemptions_y1 := shift(exemptions, n = 1 , type = "lead"), by = area_fips]
setorder(reg, area_fips, year)

# Shift IRS migration measures forward one year
# New year t contains the IRS measure previously labeled t-1
irs_vars <- c(
  "exemptions",
  "exemptions_3_inflow",
  "exemptions_3_outflow",
  "returns",
  "returns_3_inflow",
  "returns_3_outflow"
)

setorder(reg, area_fips, year)

# First construct cumulative series using original IRS labeling
reg[, cumulative_outflows :=
      cumsum(exemptions_3_outflow),
    by = area_fips]

reg[, cumulative_inflows :=
      cumsum(exemptions_3_inflow),
    by = area_fips]

reg[, cumulative_net_migration :=
      cumsum(returns_3_inflow - returns_3_outflow),
    by = area_fips]

reg[, cumulative_total_migration :=
      cumsum(exemptions_same_state_outflow + exemptions_same_state_inflow),
    by = area_fips]
setorder(reg, area_fips, year)

# Same-state migration ---------------------------------------------------------

reg[
  year >= 1995,
  cumulative_same_state_outflows :=
    cumsum(exemptions_same_state_outflow),
  by = area_fips
]

reg[
  year >= 1995,
  cumulative_same_state_inflows :=
    cumsum(exemptions_same_state_inflow),
  by = area_fips
]

reg[
  year >= 1995,
  cumulative_same_state_migration :=
    cumsum(
      exemptions_same_state_outflow +
        exemptions_same_state_inflow
    ),
  by = area_fips
]


# Different-state migration ----------------------------------------------------
run_lp_ratio(reg, "returns_3_outflow_share_returns_total_migration")
setorder(reg, area_fips, year)
reg[, cumulative_exemptions_outflow := cumsum(exemptions_3_outflow), by = area_fips]
reg[, cum_ex_net_migration := cumsum(exemptions_net_migration), by = area_fips]
run_lp_lagged_denom(reg, "total_migration", 
                    denom = "population", 
                    start_year = 1998, 
                    end_year = 2015, horizons = 0:10)



reg[, exemptions_share_population := exemptions / population]


run_lp_ratio(reg, "exemptions_share_population", end_year = 2015)
lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="exemptions_total_migration",
  denominator =  "population", 
  shock_1="w_IPW_US",
  shock_2="w_l1_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l1_empw_neighbor_IPW_OTH",
  start_year=1995,
  end_year=2015
)

n <- 5
reg[, l_shock_us := shift(w_IPW_US, n = n), by = area_fips]
reg[, l_shock_ot := shift(w_IPW_OTH, n = n), by = area_fips]

reg[, l_nshock_us := shift(w_l1_empw_neighbor_IPW_US, n = n), by = area_fips]
reg[, l_nshock_ot := shift(w_l1_empw_neighbor_IPW_OTH, n = n), by = area_fips]

lf_multi <- run_lp_ratio_multiple_shocks(
  reg,
  outcome="ew_share_into_less_unemp",
  shock_1="l_shock_us",
  shock_2="l_nshock_us",
  instrument_1="l_shock_ot",
  instrument_2="l_nshock_ot",
  start_year=1995,
  end_year=2015
)







exit 
reg[
  year >= 1995,
  cumulative_diff_state_outflows :=
    cumsum(exemptions_diff_state_outflow),
  by = area_fips
]

reg[
  year >= 1995,
  cumulative_diff_state_inflows :=
    cumsum(exemptions_diff_state_inflow),
  by = area_fips
]

reg[
  year >= 1995,
  cumulative_diff_state_migration :=
    cumsum(
      exemptions_diff_state_outflow +
        exemptions_diff_state_inflow
    ),
  by = area_fips
]
# Then relabel those cumulative series one year forward
cum_vars <- c(
  "cumulative_outflows",
  "cumulative_inflows",
  "cumulative_net_migration",
  "cumulative_total_migration"
)

reg[, paste0(cum_vars, "_y1") :=
      lapply(.SD, shift, n = 1),
    by = area_fips,
    .SDcols = cum_vars]

winsor(reg, "cumulative_outflows_y1")
run_lp_lagged_denom(
  reg,
  outcome = "exemptions_same_state_outflow",
  denominator = "population",
  controls = "+l1_own_shock",
  start_year = 1995,
  end_year = 2015
)

winsor(reg, "exemptions_3_outflow_share_population_2000")
lf_multi <- run_lp_ratio_multiple_shocks(
  reg,
  outcome="cumulative_total_migration",
  shock_1="lshock_us",
  shock_2="lshock_nus",
  instrument_1="lshock_oth",
  instrument_2="lshock_noth",
  start_year=1998,
  end_year=2007, 
  horizons = 0:7
)

run_lp_lagged_denom(reg, outcome="cumulative_outflows", 
                    denominator="population",
                    controls="", # removed w_ex_mean_work_IPW_US 
                    start_year = 1998, 
                    end_year = 2015)


lf_multi <- run_lp_ratio_multiple_shocks(
  reg,
  outcome="coverage_gap",
  shock_1="w_IPW_US",
  shock_2="w_l1_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l1_empw_neighbor_IPW_OTH",
  start_year=2000,
  end_year=2015
)

winsor(reg, "coverage_gap")
run_lp_ratio(reg, "cumulative_outflows_y1", 
             start_year=2000, 
             end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")

run_lp_lagged_denom(reg, outcome="outside_jobs", 
                    denominator="resident_emp",
                    controls="+l1_own_shock", # removed w_ex_mean_work_IPW_US 
                    start_year = 2000, 
                    end_year = 2015)

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="cumulative_total_migration",
  denominator="population", # or resident_emp
  shock_1="w_IPW_US",
  shock_2="w_l1_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l1_empw_neighbor_IPW_OTH",
  controls="",
  start_year=1995,
  end_year=2015
)

# cumulative SAME-STATE outflows
out_same <- run_lp_multiple_shocks(
  reg,
  outcome = "cumulative_same_state_outflows",
  denominator = "population",
  shock_1 = "w_IPW_US",
  shock_2 = "w_l1_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_l1_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015
)

# cumulative DIFFERENT-STATE outflows
out_diff <- run_lp_multiple_shocks(
  reg,
  outcome = "cumulative_diff_state_outflows",
  denominator = "population",
  shock_1 = "w_IPW_US",
  shock_2 = "w_l1_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_l1_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015
)

