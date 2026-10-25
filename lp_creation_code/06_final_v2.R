################################################################################
# Final county LP specifications, v2. Original testing script remains unchanged.
# Five outcomes, each with single-shock and two-shock specifications (10 calls).
################################################################################
rm(list = ls())
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")
date <- Sys.Date()
input_date <- "2026-10-25"
reg <- fread(paste0(path, "/output/lp_transformed_reg_", input_date, ".csv"))
reg <- reg[year > 1994 & year < 2020]
setorder(reg, area_fips, year)
reg[, l_sh_empl_mfg := shift(sh_empl_mfg), by = area_fips]
reg[, unemployed := as.double(unemployed)]

# Preserve the existing 1% preprocessing of network exposure variables.
shocks <- grep("empw|mean", names(reg), value = TRUE)
for (v in shocks) winsor(reg, v, p = .01)

# Fill only missing 2001 full-flow counts; leave all other years unchanged.
reg[reg[year == 2000, .(area_fips, value_2000 = aux_outside_jobs)],
    on = .(area_fips),
    aux_outside_jobs := fifelse(
      year == 2001 & is.na(aux_outside_jobs), i.value_2000, aux_outside_jobs
    )]

run_dir <- file.path(figs, paste0("final_v2_", format(Sys.time(), "%Y%m%d_%H%M%S")))
figs <- file.path(run_dir, "figures")
tables_dir <- file.path(run_dir, "tables")
dir.create(figs, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# Auxiliary commuting: count-change LPs -----------------------------------------
commuting_full_resident_emp <- run_lp_multiple_shocks(
  reg,
  outcome = "aux_outside_jobs",
  denominator = "aux_resident_emp",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock_1+population",
  start_year = 2003,
  end_year = 2015,
  horizons = 0:6,
  table_horizons = 0:6,
  figure = TRUE,
  table = TRUE,
  table_output_dir = tables_dir,
  unit = "area_fips",
  height = 4,
  width = 7
)

commuting_full_resident_emp_single <- run_lp_lagged_denom(
  reg,
  outcome = "aux_outside_jobs",
  denominator = "aux_resident_emp",
  shock = "w_IPW_US",
  instrument = "w_IPW_OTH",
  controls = "+l1_own_shock+population",
  start_year = 2003,
  end_year = 2015,
  horizons = 0:6,
  table_horizons = 0:6,
  figure = TRUE,
  table = TRUE,
  table_output_dir = tables_dir,
  unit = "area_fips"
)

# Ratio outcomes below use percentage-point changes in both model versions.
# Unemployment and labor-force participation ----------------------------------
unemployment_ratio <- run_lp_ratio_multiple_shocks(
  reg,
  outcome = "unemployed_share_labor_force",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  table_horizons = 0:7,
  figure = TRUE,
  table = TRUE,
  table_output_dir = tables_dir,
  unit = "area_fips",
  height = 4,
  width = 7
)

unemployment_single <- run_lp_ratio(
  reg,
  outcome = "unemployed_share_labor_force",
  shock = "w_IPW_US",
  instrument = "w_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  table_horizons = 0:7,
  figure = TRUE,
  table = TRUE,
  table_output_dir = tables_dir,
  outcome_scale = 100,
  unit = "area_fips"
)

lfp_two_shock <- run_lp_ratio_multiple_shocks(
  reg,
  outcome = "labor_force_share_population",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  table_horizons = 0:7,
  figure = TRUE,
  table = TRUE,
  table_output_dir = tables_dir,
  unit = "area_fips",
  height = 4,
  width = 7
)

lfp_single <- run_lp_ratio(
  reg,
  outcome = "labor_force_share_population",
  shock = "w_IPW_US",
  instrument = "w_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  table_horizons = 0:7,
  figure = TRUE,
  table = TRUE,
  table_output_dir = tables_dir,
  outcome_scale = 100,
  unit = "area_fips"
)

# Manufacturing share: 1995-2008 -----------------------------------------------
winsor(reg, "sh_empl_mfg", p = .01)
manufacturing_two_shock <- run_lp_ratio_multiple_shocks(
  reg,
  outcome = "w_sh_empl_mfg",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2008,
  horizons = 0:7,
  table_horizons = 0:7,
  figure = TRUE,
  table = TRUE,
  table_output_dir = tables_dir,
  unit = "area_fips",
  height = 4,
  width = 7
)

manufacturing_single <- run_lp_ratio(
  reg,
  outcome = "w_sh_empl_mfg",
  shock = "w_IPW_US",
  instrument = "w_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2008,
  horizons = 0:7,
  table_horizons = 0:7,
  figure = TRUE,
  table = TRUE,
  table_output_dir = tables_dir,
  outcome_scale = 100,
  unit = "area_fips"
)

# Total migration: preserve the selected 2% employment-neighbor preprocessing.
winsor(reg, "empw_neighbor_IPW_US", p = .02)
winsor(reg, "empw_neighbor_IPW_OTH", p = .02)
reg[year >= 1995, cum_domestic_inflow := cumsum(exemptions_3_inflow), by = area_fips]
reg[year >= 1995, cum_domestic_outflow := cumsum(exemptions_3_outflow), by = area_fips]
reg[, cum_total_domestic_migration := cum_domestic_inflow + cum_domestic_outflow]

migration_total <- run_lp_multiple_shocks(
  reg,
  outcome = "cum_total_domestic_migration",
  denominator = "exemptions",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  table_horizons = 0:7,
  figure = TRUE,
  table = TRUE,
  table_output_dir = tables_dir,
  unit = "area_fips",
  height = 4,
  width = 7
)

migration_total_single <- run_lp_lagged_denom(
  reg,
  outcome = "cum_total_domestic_migration",
  denominator = "exemptions",
  shock = "w_IPW_US",
  instrument = "w_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  table_horizons = 0:7,
  figure = TRUE,
  table = TRUE,
  table_output_dir = tables_dir,
  unit = "area_fips"
)

final_results <- list(
  commuting_full_resident_emp = commuting_full_resident_emp,
  commuting_full_resident_emp_single = commuting_full_resident_emp_single,
  unemployment_ratio = unemployment_ratio,
  unemployment_single = unemployment_single,
  lfp_two_shock = lfp_two_shock,
  lfp_single = lfp_single,
  manufacturing_two_shock = manufacturing_two_shock,
  manufacturing_single = manufacturing_single,
  migration_total = migration_total,
  migration_total_single = migration_total_single
)
saveRDS(final_results, file.path(run_dir, paste0("final_results_", date, ".rds")))
message("Figures: ", figs, "\nTables: ", tables_dir)
