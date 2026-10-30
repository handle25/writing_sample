# Single-shock CZ LPs with full main + auxiliary LODES flows.
# Inside/outside refers to CZ boundaries, not county boundaries.
rm(list = ls())
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")
date <- Sys.Date()
start_year <- 2002L
end_year <- 2015L
horizons <- 0:7
unit <- "commuting_zone_id_2000"
reg <- fread(file.path(path, "output/lp_transformed_reg_CZ.csv"))[year > 1994 & year < 2020]
stopifnot(!anyDuplicated(reg[, .(commuting_zone_id_2000, year)]))
run_dir <- file.path(figs, paste0("testing_CZ_single_", format(Sys.time(), "%Y%m%d_%H%M%S")))
figs <- file.path(run_dir, "figures")
tables_dir <- file.path(run_dir, "tables")
lp_results_dir <- file.path(run_dir, "results")
for (d in c(figs, tables_dir, lp_results_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Fixed pooled 1995-2019 shock/IV caps, matching the county testing baseline.
for (v in c("IPW_US", "IPW_OTH")) {
  x <- reg[[v]]
  bounds <- quantile(x[is.finite(x)], c(.01, .99), names = FALSE)
  reg[, (paste0("w_", v)) := pmin(pmax(get(v), bounds[1L]), bounds[2L])]
}

# Existing aux_com columns preserve outside-COUNTY definitions. Build distinct CZ columns.
cw <- read_excel(file.path(path, "cz00eqvv1.xls")) |> data.table() |> clean_names()
cw <- unique(cw[!is.na(commuting_zone_id_2000),
  .(county = as.integer(fips), home_cz = as.integer(commuting_zone_id_2000))])
stopifnot(!anyDuplicated(cw$county))
flows <- fread(file.path(path, "lodes/raw/full_lodes_data_with_aux_1990-2025.csv"),
  select = c("county", "w_county", "year", "S000", "SI02", "SI03", "SE01"))
flows <- flows[year %in% reg$year]
stopifnot(!anyDuplicated(flows[, .(county, w_county, year)]))
flows <- merge(flows, cw, by = "county", all.x = TRUE)
work_cw <- copy(cw)
setnames(work_cw, c("county", "home_cz"), c("w_county", "work_cz"))
flows <- merge(flows, work_cw, by = "w_county", all.x = TRUE)
flows[, state_fips := as.integer(county) %/% 1000L]
flows[, bad_coverage :=
  (state_fips %in% c(5L, 33L) & year == 2002) |
  (state_fips %in% c(4L, 28L) & year %in% 2002:2003) |
  (state_fips == 25L & year %in% 2002:2010) |
  (state_fips == 9L & year %in% 2009:2010)]
divide <- function(x, y) if (is.finite(y) && y > 0) x / y else NA_real_
cz_measures <- flows[!is.na(home_cz), {
  mapped <- !is.na(work_cz)
  usable <- all(is.finite(S000) & S000 >= 0) && all(mapped) && !any(bad_coverage)
  inside <- mapped & work_cz == home_cz
  outside <- mapped & work_cz != home_cz
  total <- if (usable) sum(as.double(S000)) else NA_real_
  inside_n <- if (usable) sum(as.double(S000[which(inside)])) else NA_real_
  outside_n <- if (usable) sum(as.double(S000[which(outside)])) else NA_real_
  nongoods <- as.double(SI02) + as.double(SI03)
  ng_ok <- usable && all(is.finite(SI02) & SI02 >= 0 & is.finite(SI03) & SI03 >= 0)
  low_ok <- usable && all(is.finite(SE01) & SE01 >= 0)
  ng_inside <- if (ng_ok) sum(nongoods[which(inside)]) else NA_real_
  low_outside <- if (low_ok) sum(as.double(SE01[which(outside)])) else NA_real_
  list(cz_aux_resident_jobs = total, cz_aux_inside_jobs = inside_n,
    cz_aux_outside_jobs = outside_n,
    cz_aux_outside_share_total_jobs = divide(outside_n, total),
    cz_aux_nongoods_inside_share_total_jobs = divide(ng_inside, total),
    cz_aux_low_earnings_outside_share_outside_jobs = divide(low_outside, outside_n),
    cz_aux_bad_coverage = any(bad_coverage),
    cz_aux_unmapped_work_jobs = sum(as.double(S000[!mapped]), na.rm = TRUE))
}, by = .(commuting_zone_id_2000 = home_cz, year)]
ok <- is.finite(cz_measures$cz_aux_resident_jobs)
stopifnot(all(abs(cz_measures$cz_aux_inside_jobs[ok] + cz_measures$cz_aux_outside_jobs[ok] -
  cz_measures$cz_aux_resident_jobs[ok]) < 1e-8))
fwrite(cz_measures, file.path(run_dir, "CZ_aux_measures.csv"))
reg <- merge(reg, cz_measures, by = c(unit, "year"), all.x = TRUE)
rm(flows, cw, work_cw, cz_measures)
setorderv(reg, c(unit, "year"))
reg[, l1_own_shock := shift(w_IPW_US), by = unit]

# Shares: pooled 2003-2015 1/99 caps, applied to all years including baseline lags.
outcome_vars <- c("cz_aux_outside_share_total_jobs",
  "cz_aux_nongoods_inside_share_total_jobs",
  "cz_aux_low_earnings_outside_share_outside_jobs")
cutoffs <- rbindlist(lapply(outcome_vars, function(v) {
  x <- reg[[v]]
  ref <- x[reg$year %in% start_year:(end_year + max(horizons)) & is.finite(x)]
  if (!length(ref)) stop("No usable observations for ", v)
  bounds <- quantile(ref, c(.01, .99), names = FALSE)
  reg[, (paste0("wpool_", v)) := pmin(pmax(get(v), bounds[1L]), bounds[2L])]
  data.table(variable = v, lower = bounds[1L], upper = bounds[2L], n = length(ref))
}))
fwrite(cutoffs, file.path(run_dir, "pooled_winsor_cutoffs.csv"))

outside_CZ_share <- run_lp_ratio(
  reg, outcome = "wpool_cz_aux_outside_share_total_jobs", outcome_scale = 100,
  shock = "w_IPW_US", instrument = "w_IPW_OTH",
  controls = "+l1_own_shock", unit = unit,
  start_year = start_year, end_year = end_year,
  horizons = horizons, table_horizons = horizons,
  figure = TRUE, table = TRUE,
  table_output_dir = tables_dir, results_output_dir = lp_results_dir,
  lagged_shock_comparison = FALSE
)

inside_CZ_nongoods_share_total <- run_lp_ratio(
  reg, outcome = "wpool_cz_aux_nongoods_inside_share_total_jobs", outcome_scale = 100,
  shock = "w_IPW_US", instrument = "w_IPW_OTH",
  controls = "+l1_own_shock", unit = unit,
  start_year = start_year, end_year = end_year,
  horizons = horizons, table_horizons = horizons,
  figure = TRUE, table = TRUE,
  table_output_dir = tables_dir, results_output_dir = lp_results_dir,
  lagged_shock_comparison = FALSE
)

low_earnings_share_outside_CZ <- run_lp_ratio(
  reg, outcome = "wpool_cz_aux_low_earnings_outside_share_outside_jobs", outcome_scale = 100,
  shock = "w_IPW_US", instrument = "w_IPW_OTH",
  controls = "+l1_own_shock", unit = unit,
  start_year = start_year, end_year = end_year,
  horizons = horizons, table_horizons = horizons,
  figure = TRUE, table = TRUE,
  table_output_dir = tables_dir, results_output_dir = lp_results_dir,
  lagged_shock_comparison = FALSE
)

manufacturing_share <- run_lp_ratio(
  reg, outcome = "w_manuf_share_emp", outcome_scale = 100,
  shock = "w_IPW_US", instrument = "w_IPW_OTH",
  controls = "+l1_own_shock", unit = unit,
  start_year = start_year, end_year = end_year,
  horizons = horizons, table_horizons = horizons,
  figure = TRUE, table = TRUE,
  table_output_dir = tables_dir, results_output_dir = lp_results_dir,
  lagged_shock_comparison = FALSE
)

net_migration_share <- run_lp_ratio(
  reg, outcome = "w_net_migration_share_population", outcome_scale = 100,
  shock = "w_IPW_US", instrument = "w_IPW_OTH",
  controls = "+l1_own_shock", unit = unit,
  start_year = start_year, end_year = end_year,
  horizons = horizons, table_horizons = horizons,
  figure = TRUE, table = TRUE,
  table_output_dir = tables_dir, results_output_dir = lp_results_dir,
  lagged_shock_comparison = FALSE
)

testing_results <- list(
  outside_CZ_share = outside_CZ_share,
  inside_CZ_nongoods_share_total = inside_CZ_nongoods_share_total,
  low_earnings_share_outside_CZ = low_earnings_share_outside_CZ,
  manufacturing_share = manufacturing_share,
  net_migration_share = net_migration_share
)
saveRDS(testing_results, file.path(run_dir, "testing_results.rds"))
combine_lp_results_csv(lp_results_dir, file.path(run_dir, "results.csv"))
message("CZ single-shock outputs: ", run_dir)
