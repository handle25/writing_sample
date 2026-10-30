# Final county regressions v5: explicit multiple-shock specifications only.
# Short/long sample windows are distinct from the response horizon (0:7).
rm(list = ls())
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")
date <- Sys.Date()
input_date <- "2026-10-26"
reg <- read_lp_panel(input_date)
reg <- reg[year > 1994 & year <= 2020]
setorder(reg, area_fips, year)
stopifnot(!anyDuplicated(reg[, .(area_fips, year)]))
reg[, l_sh_empl_mfg := shift(sh_empl_mfg), by = area_fips]
reg[, unemployed := as.double(unemployed)]
if (max(reg$year) < 2022L)
  warning("The panel ends in ", max(reg$year),
    "; late shock years in the 1995-2015 models cannot contribute to all horizons through 7.")

# Preserve v2's baseline handling; this is a carried-forward Census baseline.
reg[reg[year == 2000, .(area_fips, value_2000 = aux_outside_jobs)],
  on = .(area_fips), aux_outside_jobs := fifelse(
    year == 2001 & is.na(aux_outside_jobs), i.value_2000, aux_outside_jobs)]

# Preserve v2 shock/IV preprocessing: pooled 1995-2019 1/99 caps.
# Later outcome years do not change the shock cap reference sample.
shock_winsor_p <- 0.01
# reg[year > 2008, IPW_US := NA_real_]
# reg[year > 2008, IPW_OTH := NA_real_]
# reg[year > 2008, empw_neighbor_IPW_US := NA_real_]
# reg[year > 2008, empw_neighbor_IPW_OTH := NA_real_]

for (v in c("IPW_US", "IPW_OTH", "empw_neighbor_IPW_US", "empw_neighbor_IPW_OTH")) {
  x <- reg[year < 2020, get(v)]
  bounds <- quantile(x[is.finite(x)], c(shock_winsor_p, 1-shock_winsor_p), names = FALSE)
  reg[, (paste0("w_", v)) := pmin(pmax(get(v), bounds[1L]), bounds[2L])]
}
reg[, l1_own_shock := shift(w_IPW_US), by = area_fips]

# Reconstruct cumulative migration as in v2; no new outcome caps.
reg[, cum_domestic_inflow := cumsum(exemptions_3_inflow), by = area_fips]
reg[, cum_domestic_outflow := cumsum(exemptions_3_outflow), by = area_fips]
reg[, cum_total_domestic_migration := cum_domestic_inflow + cum_domestic_outflow]

# Stable figure copies, in addition to all timestamped run outputs.
options(lp.final_figure_dir = file.path(figs, "final_lps"))
run_dir <- file.path(figs, paste0("final_regs_v5_", format(Sys.time(), "%Y%m%d_%H%M%S")))
figs <- file.path(run_dir, "figures")
tables_dir <- file.path(run_dir, "tables")
lp_results_dir <- file.path(run_dir, "results")
if (lp_save_outputs()) {
  dir.create(figs, recursive = TRUE, showWarnings = FALSE)
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(lp_results_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(tables_dir, "hhi_1995_2008"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(tables_dir, "hhi_1995_2015"), recursive = TRUE, showWarnings = FALSE)
}

state_lookup <- data.table(
  state_fips = c(
    1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18,
    19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
    33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47,
    48, 49, 50, 51, 53, 54, 55, 56, 72, 78
  ),
  state_str = c(
    "al", "ak", "az", "ar", "ca", "co", "ct", "de", "dc", "fl",
    "ga", "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me",
    "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh",
    "nj", "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri",
    "sc", "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi",
    "wy", "pr", "vi"
  )
)

reg[, state_fips := as.integer(as.character(area_fips)) %/% 1000L]
reg[, state_str := state_lookup$state_str[
  match(state_fips, state_lookup$state_fips)
]]


# Mark only the commuting counts missing; retain calendar rows for leads/lags.
# commuting shocks -------------------------------------------------------------
reg[
  (state_str %chin% c("ar", "nh") & year == 2002) |
    (state_str %chin% c("az", "ms") & year %in% 2002:2003) |
    (state_str == "ma" & year %in% 2002:2010) |
    (state_str == "ct" & year %in% 2009:2010),
  c("aux_outside_jobs", "aux_resident_emp") := NA_real_
]
winsor(reg, "aux_outside_jobs")
winsor(reg, "aux_resident_emp")
commuting_full_resident_emp <- run_lp_multiple_shocks(
  reg,
  results_output_dir = lp_results_dir,
  outcome = "w_aux_outside_jobs",
  denominator = "w_aux_resident_emp",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock",
  start_year = 2002,
  end_year = 2008,
  horizons = 0:6,
  table_horizons = 0:6,
  figure = TRUE,
  table = TRUE,
  table_output_dir = tables_dir,
  unit = "area_fips",
  height = 4,
  width = 7
)

# labor force shocks -----------------------------------------------------------
unemployment_ratio <- run_lp_ratio_multiple_shocks(
  reg,
  results_output_dir = lp_results_dir,
  outcome = "w_unemployed_share_labor_force",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock",
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


lfp_two_shock <- run_lp_ratio_multiple_shocks(
  reg,
  results_output_dir = lp_results_dir,
  outcome = "w_labor_force_share_population",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock",
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

# industry level shocks : recomposition 2000-2015 ------------------------------
manufacturing_two_shock <- run_lp_ratio_multiple_shocks(
  reg,
  results_output_dir = lp_results_dir,
  outcome = "sh_empl_mfg",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock",
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

figs <- file.path(run_dir, "figures", "hhi_1995_2008")
industry_hhi_two_shock <- run_lp_ratio_multiple_shocks(
  reg,
  results_output_dir = lp_results_dir,
  outcome = "industry_hhi",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock",
  start_year = 1995,
  end_year = 2008,
  horizons = 0:7,
  table_horizons = 0:7,
  figure = TRUE,
  table = TRUE,
  table_output_dir = file.path(tables_dir, "hhi_1995_2008"),
  unit = "area_fips",
  height = 4,
  width = 7
)

figs <- file.path(run_dir, "figures")

figs <- file.path(run_dir, "figures", "hhi_1995_2015")
industry_hhi_two_shock_long <- run_lp_ratio_multiple_shocks(
  reg,
  results_output_dir = lp_results_dir,
  outcome = "industry_hhi",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  table_horizons = 0:7,
  figure = TRUE,
  table = TRUE,
  table_output_dir = file.path(tables_dir, "hhi_1995_2015"),
  unit = "area_fips",
  height = 4,
  width = 7
)

figs <- file.path(run_dir, "figures")

# Migration regs: long term results. Goes to 2015 shocks. ----------------------
migration_total <- run_lp_multiple_shocks(
  reg,
  results_output_dir = lp_results_dir,
  outcome = "cum_total_domestic_migration",
  denominator = "exemptions",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock",
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

migration_outflow_multi <- run_lp_multiple_shocks(
  reg,
  results_output_dir = lp_results_dir,
  outcome = "exemptions_3_inflow",
  denominator = "exemptions",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock",
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

# Appendix composition models: 2003-2008 shocks, horizons 0:7.
# Keep exclusions specific to these commuting outcomes; preserve calendar rows.
composition_cols <- c("aux_com_low_earnings_share_outside_jobs",
  "aux_com_nongoods_inside_share_sector_jobs")
stopifnot(all(composition_cols %in% names(reg)))
reg[
  (state_str %chin% c("ar", "nh") & year == 2002) |
    (state_str %chin% c("az", "ms") & year %in% 2002:2003) |
    (state_str == "ma" & year %in% 2002:2010) |
    (state_str == "ct" & year %in% 2009:2010),
  (composition_cols) := NA_real_
]
for (v in composition_cols) {
  bounds <- quantile(reg[year %in% 2003:2015, get(v)],
    c(.01, .99), na.rm = TRUE, names = FALSE)
  reg[, (paste0("wpool_", v)) := pmin(pmax(get(v), bounds[1L]), bounds[2L])]
}

low_earnings_share_outside_jobs <- run_lp_ratio_multiple_shocks(
  reg,
  results_output_dir = lp_results_dir,
  outcome = "wpool_aux_com_low_earnings_share_outside_jobs",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock",
  start_year = 2003,
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

nongoods_inside_share_sector_jobs <- run_lp_ratio_multiple_shocks(
  reg,
  results_output_dir = lp_results_dir,
  outcome = "wpool_aux_com_nongoods_inside_share_sector_jobs",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock",
  start_year = 2003,
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

final_results <- list(
  commuting_full_resident_emp = commuting_full_resident_emp,
  unemployment_ratio = unemployment_ratio,
  industry_hhi_two_shock = industry_hhi_two_shock,
  industry_hhi_two_shock_long = industry_hhi_two_shock_long,
  lfp_two_shock = lfp_two_shock,
  manufacturing_two_shock = manufacturing_two_shock,
  migration_total = migration_total,
  migration_outflow_multi = migration_outflow_multi
)
final_results$low_earnings_share_outside_jobs <- low_earnings_share_outside_jobs
final_results$nongoods_inside_share_sector_jobs <- nongoods_inside_share_sector_jobs
if (lp_save_outputs()) {
  saveRDS(final_results, file.path(run_dir, paste0("final_results_", date, ".rds")))
  results_csv <- combine_lp_results_csv(lp_results_dir, file.path(run_dir, "results.csv"))
  message("Final v5 outputs: ", run_dir)
} else {
  message("Display-only run complete; results are in final_results. No files saved.")
}
