source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")
date <- Sys.Date()
# Match the data vintage used in 06_final_v2.R; edit this to select another build.
input_date <- "2026-10-25"
lp_input_file <- file.path(path, "output", paste0("lp_transformed_reg_", input_date, ".csv"))
figs <- "D:/writing_sample/figures"
# Overleaf bundle: all summary-statistics TeX files, with no data exports.
summary_stats_dir <- file.path(figs, paste0("summary_stats_v", date))
dir.create(summary_stats_dir, recursive = TRUE, showWarnings = FALSE)

# Archived long-difference section: disabled pending input review.
# # long differences -------------------------------------------------------------
# reg <- fread(paste0(path, "/output/transformed_reg.csv"))
# ## shock  ----------------------------------------------------------------------
# file <- paste0(figs, "/shock_distribution.tex")
# names_dict <- c(
#   "w_d_labor_force_share_population" = "$\\Delta$ Labor Force Participation",
#   "w_d_unemployed_share_labor_force" = "$\\Delta$ Unemployment Rate",
#   "w_d_outside_jobs_share_population" = "$\\Delta$ Outside-County Employment / Population",
#   "w_d_exemptions_net_migration_share_population" = "$\\Delta$ Net Migration / Population"
# )
# 
# to_summarize <- c(
#   "w_d_labor_force_share_population",
#   "w_d_unemployed_share_labor_force",
#   "w_d_outside_jobs_share_population",
#   "w_d_exemptions_net_migration_share_population"
# )
# p <- c(.01,.05,.25,.50,.75,.90,.99) 
# d <- 2 
# sum07 <- round(quantile(reg[year==2007, w_IPW_US], p = p, na.rm = T), d)
# sum13 <- round(quantile(reg[year==2013, w_IPW_US], p = p,na.rm = T), d)
# 
# mean07 <- round(weighted.mean(reg[year==2007, w_IPW_US], reg[year==2007, baseline_emp], na.rm=T), 3)
# mean13 <- round(weighted.mean(reg[year==2013, w_IPW_US], reg[year==2013, baseline_emp], na.rm=T), 3)
# 
# cat("\\begin{tabular}{l", rep("c", length(p)), "c} \n", file = file)
# cat("\\toprule \n & \\multicolumn{", length(p), "}{c}{Percentile}  \\\\ \n \\cmidrule{2-8}  ",  
#     file = file, append = T)
# 
# cat(" & 1 & 5 & 25 & 50 & 75 & 90 & 99 & Mean \\\\\n \\midrule ",  file = file, append = T)
# cat("2000-2007 Import Exposure ", sum07, mean07, sep = "&", file = file, append = T)
# cat("\\\\\n", file = file, append = T)
# cat("2007-2013 Import Exposure ",sum13, mean13, sep = "&", file = file, append = T)
# cat("\\\\\nTotal", rep("", length(p)), round(mean07 + mean13, 3), sep = "&", file = file, append = T)
# cat("\\\\\n \\bottomrule \n \\end{tabular}", file = file, append = T)
# 
# local projections 
reg <- fread(lp_input_file)

file <- file.path(summary_stats_dir, paste0("lp_shock_distribution_", date, ".tex"))
p <- c(.01,.05,.25,.50,.75,.90,.99) 
means <- 0 
cat("\\begin{tabular}{l", rep("c", length(p)), "c} \n", file = file)
cat("\\toprule \n & \\multicolumn{", length(p), "}{c}{Percentile}  \\\\ \n \\cmidrule{2-8}  ",  
    file = file, append = T)
cat(" & 1 & 5 & 25 & 50 & 75 & 90 & 99 & Mean \\\\\n \\midrule ",  file = file, append = T)

for (y in 2000:2013){
  sum <- round(quantile(reg[year == y, w_IPW_US], probs = p, na.rm = TRUE), 2)
  mean <- round(reg[year == y, weighted.mean(w_IPW_US,
                        w = as.double(baseline_emp), na.rm = TRUE)], 3)
  cat(paste0(y, " Import Exposure "), sum, mean, sep = "&", file = file, append = T)
  cat("\\\\\n", file = file, append = T)
  
  means <- means + mean 
}

cat("Total", rep("", length(p)), round(means, 3), sep="&", file=file, append=T)
cat("\\\\\n", file=file, append=T)
cat(" \\bottomrule \n \\end{tabular}", file = file, append = T)


# labor force participation 
laus <- read_excel(paste0(path, "/laus/laucnty90.xlsx"), skip = 1)
for (year in c(1991:2024)) {
  y <- sprintf("%02.f", as.integer(substr(as.character(year), 3,4)))
  laus <- rbind(laus, 
                read_excel(paste0(path, "/laus/laucnty", y, ".xlsx"),
                           skip = 1)
  )
}
laus <- laus |> 
  clean_names() |>
  data.table() |> 
  fmutate(area_fips = as.integer(
    paste0(state_fips_code, county_fips_code)), 
    year = as.integer(year))

acs <- fread(paste0(path, "/acs/population_1995_2023.csv"))

laus <- merge(laus, acs, by = c("year", "area_fips"))
laus <- laus[year %in% 2000:2013,]

laus <- laus[,.(
  labor_force = sum(labor_force, na.rm = T), 
  population = sum(population, na.rm = T)), 
  by = year]
laus[, lfp := labor_force / population ]
ggplot(laus, aes(
  x = year, 
  y = lfp
))+ 
  geom_line()

reg <- fread(lp_input_file)
winsor(reg, "mean_neighbor_IPW_US", p = .03)



var <- "aux_outside_jobs_share_aux_resident_emp"
reg[, diff_var := get(var) - shift(get(var)), by = area_fips]
winsor(reg, "diff_var") 

# reg <- fread(paste0(path, "/output/transformed_reg.csv"))
# Pull US county shapefile
counties <- counties(cb = TRUE, year = 2020, class = "sf")

# Drop AK, HI, PR, and territories
counties <- counties[!counties$STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"), ]

# Make FIPS compatible with reg
counties$area_fips <- as.integer(counties$GEOID)

# Pick year and variable to map
map_data <- reg[year == 2007, .(area_fips, value = w_diff_var)]

# Merge data onto geometry
map_sf <- merge(counties, map_data, by = "area_fips", all.x = TRUE)

# Plot
ggplot(map_sf) +
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c(option = "magma", na.value = "grey90") +
  coord_sf(datum = NA) +
  labs(
    fill = "Import exposure",
    title = "Import Exposure Across U.S. Counties"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )




commute <- reg[,.(mean = weighted.mean(aux_outside_jobs_share_population, na.rm = TRUE, w = baseline_emp)) , by = year]

# State snapshots: 2000 and 2019 ------------------------------------------------
# Baseline-employment-weighted county means; all entries are percentages.
snapshot <- fread(lp_input_file, select = c("area_fips", "year", "baseline_emp", "aux_outside_jobs",
  "aux_resident_emp", "population", "labor_force", "unemployed", "sh_empl_mfg"))
snapshot <- snapshot[year %in% c(2000L, 2019L)]
stopifnot(!anyDuplicated(snapshot[, .(area_fips, year)]))
snapshot[, state_fips := as.integer(area_fips) %/% 1000L]
state_lookup <- data.table(
  state_fips = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,
                 26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,
                 47,48,49,50,51,53,54,55,56),
  state = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
    "Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii",
    "Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine",
    "Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri",
    "Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
    "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
    "Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee",
    "Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")
)
# Fifty states and DC; territories excluded.
snapshot <- merge(snapshot, state_lookup, by = "state_fips", all = FALSE)
snapshot[, `:=`(
  commuting_out = fifelse(is.finite(aux_resident_emp) & aux_resident_emp > 0,
                          aux_outside_jobs / aux_resident_emp, NA_real_),
  lfp = fifelse(is.finite(population) & population > 0, labor_force / population, NA_real_),
  unemployment = fifelse(is.finite(labor_force) & labor_force > 0,
                          unemployed / labor_force, NA_real_),
  manufacturing = sh_empl_mfg
)]
snapshot_vars <- c("commuting_out", "lfp", "unemployment", "manufacturing")
snapshot_long <- melt(snapshot,
  id.vars = c("state", "year", "area_fips", "baseline_emp"),
  measure.vars = snapshot_vars, variable.name = "measure", value.name = "value")
snapshot_summary <- snapshot_long[, {
  ok <- is.finite(value) & is.finite(baseline_emp) & baseline_emp > 0
  .(mean_pct = if (any(ok)) 100 * weighted.mean(value[ok], baseline_emp[ok], na.rm = TRUE)
                else NA_real_, n_counties = sum(ok))
}, by = .(state, year, measure)]
snapshot_wide <- dcast(snapshot_summary, state ~ year + measure, value.var = "mean_pct")
# N counts unique counties with a usable weight; individual outcomes may have fewer observations.
snapshot_counts <- snapshot[is.finite(baseline_emp) & baseline_emp > 0,
  .(N = uniqueN(area_fips)), by = .(state, year)]
snapshot_counts <- dcast(snapshot_counts, state ~ year, value.var = "N", fill = 0L)
setnames(snapshot_counts, c("2000", "2019"), c("2000_N", "2019_N"))
snapshot_wide <- merge(snapshot_wide, snapshot_counts, by = "state", all.x = TRUE)
snapshot_columns <- c("state", paste0("2000_", c(snapshot_vars, "N")),
                      paste0("2019_", c(snapshot_vars, "N")))
setcolorder(snapshot_wide, snapshot_columns)
setorder(snapshot_wide, state)
dir.create(figs, recursive = TRUE, showWarnings = FALSE)
snapshot_tex <- file.path(summary_stats_dir, paste0("state_outcome_snapshot_2000_2019_", date, ".tex"))

# Tabular fragment only: place captions, labels, and table wrappers in the paper. Requires booktabs.
row_end <- paste0(" ", intToUtf8(c(92,92)))
header <- c("\\toprule",
  paste0("State & \\multicolumn{5}{c}{2000} & \\multicolumn{5}{c}{2019}", row_end),
  "\\cmidrule(lr){2-6} \\cmidrule(lr){7-11}",
  paste0(" & Comm. out & LFP & Unemp. & Mfg. & N & Comm. out & LFP & Unemp. & Mfg. & N", row_end),
  "\\midrule")
format_snapshot <- function(x) ifelse(is.finite(x), sprintf("%.2f", x), "--")
body <- vapply(seq_len(nrow(snapshot_wide)), function(i) {
  values <- unlist(snapshot_wide[i, snapshot_columns[-1L], with = FALSE], use.names = FALSE)
  cells <- format_snapshot(values)
  cells[c(5L, 10L)] <- sprintf("%d", as.integer(values[c(5L, 10L)]))
  paste0(paste(c(snapshot_wide$state[i], cells), collapse = " & "), row_end)
}, character(1))
writeLines(c(
  "% Outcome entries are baseline-employment-weighted percentages; N is the county count.",
  "\\begin{tabular}{lrrrrrrrrrr}",
  header, body, "\\bottomrule", "\\end{tabular}",
  "% Comm. out = full main-plus-auxiliary outside-county jobs / resident jobs;",
  "% LFP = labor force / population; Unemp. = unemployed / labor force; Mfg. = manufacturing employment share.",
  "% All means use baseline_emp weights; invalid values/weights are excluded separately for each measure.",
  "% N counts counties with a valid baseline_emp weight, regardless of outcome availability.",
  "% Counties observed in each year are used; the two snapshots need not be a balanced sample."
), snapshot_tex)
message("State snapshot saved: ", snapshot_tex)



# LP dependent-variable distributions: overall and baseline industry HHI ---------
# Descriptive levels, not long differences or horizon-specific LP responses.
distribution_years <- c(2003L, 2015L)
dep_data <- fread(lp_input_file, select = c("area_fips", "year", "baseline_emp",
  "industry_hhi", "aux_outside_jobs", "aux_resident_emp", "population",
  "labor_force", "unemployed", "sh_empl_mfg"))
dep_hhi <- dep_data[year == 2000 & is.finite(industry_hhi),
                   .(hhi_2000 = mean(industry_hhi)), by = area_fips]
stopifnot(nrow(dep_hhi) > 0L)
dep_hhi_median <- median(dep_hhi$hhi_2000, na.rm = TRUE)
message(sprintf("Overall county median industry HHI in 2000 (split cutoff, unweighted): %.6f", dep_hhi_median))
dep_hhi[, hhi_group := fifelse(hhi_2000 > dep_hhi_median, "Above-median HHI", "At-or-below-median HHI")]
# Employment-weighted baseline HHI means; count each county once.
dep_hhi_weights <- dep_data[year == 2000,
  .(hhi_weight = as.double(baseline_emp[1L])), by = area_fips]
dep_hhi_means <- merge(dep_hhi, dep_hhi_weights, by = "area_fips", all.x = TRUE)
dep_hhi_means <- dep_hhi_means[is.finite(hhi_2000) & is.finite(hhi_weight) & hhi_weight > 0,
  .(mean_hhi = weighted.mean(hhi_2000, w = hhi_weight, na.rm = TRUE)), by = hhi_group]
dep_data <- merge(dep_data, dep_hhi, by = "area_fips", all.x = TRUE)
dep_data <- dep_data[year %in% distribution_years]
dep_data[, `:=`(
  commuting_out = fifelse(is.finite(aux_resident_emp) & aux_resident_emp > 0,
    100 * aux_outside_jobs / aux_resident_emp, NA_real_),
  lfp = fifelse(is.finite(population) & population > 0,
    100 * labor_force / population, NA_real_),
  unemployment = fifelse(is.finite(labor_force) & labor_force > 0,
    100 * unemployed / labor_force, NA_real_),
  manufacturing = 100 * sh_empl_mfg
)]
dep_names <- c(commuting_out = "Outside-county jobs / resident jobs",
  lfp = "Labor force / population", unemployment = "Unemployed / labor force",
  manufacturing = "Manufacturing employment share")
dep_probs <- c(.01, .25, .50, .75, .99)
dep_groups <- c("Overall", "At-or-below-median HHI", "Above-median HHI")
dep_distribution <- list()
for (g in dep_groups) {
  for (yr in distribution_years) {
    panel <- if (g == "Overall") dep_data[year == yr] else dep_data[year == yr & hhi_group == g]
    for (v in names(dep_names)) {
      x <- panel[[v]]
      w <- as.double(panel$baseline_emp)
      ok <- is.finite(x) & is.finite(w) & w > 0
      q <- if (any(ok)) as.numeric(quantile(x[ok], probs = dep_probs, na.rm = TRUE))
           else rep(NA_real_, length(dep_probs))
      m <- if (any(ok)) weighted.mean(x[ok], w = w[ok], na.rm = TRUE) else NA_real_
      dep_distribution[[paste(g, yr, v)]] <- data.table(group = g, year = yr, measure = v,
        p01 = q[1], p25 = q[2], p50 = q[3], p75 = q[4], p99 = q[5], mean = m,
        n_counties = uniqueN(panel$area_fips[ok]))
    }
  }
}
dep_distribution <- rbindlist(dep_distribution)
dep_file <- file.path(summary_stats_dir, paste0("dep_var_distribution_", date, ".tex"))
dep_row_end <- paste0(" ", intToUtf8(c(92, 92)))
dep_lines <- c(
  "% LP data, separate county snapshots in 2000 and 2019; all outcomes in percent.",
  "% Percentiles are unweighted; means use baseline_emp weights. Requires booktabs.",
  paste0("% HHI split uses the county median of year-2000 industry_hhi: ", dep_hhi_median),
  "\\begin{tabular}{lrrrrrrrrrrrr}", "\\toprule",
  paste0(" & \\multicolumn{6}{c}{2003} & \\multicolumn{6}{c}{2015}", dep_row_end),
  "\\cmidrule(lr){2-7} \\cmidrule(lr){8-13}",
  paste0(" & P1 & P25 & P50 & P75 & P99 & Mean & P1 & P25 & P50 & P75 & P99 & Mean", dep_row_end),
  "\\midrule")
for (g in dep_groups) {
  if (g != "Overall") dep_lines <- c(dep_lines, "\\addlinespace")
  dep_lines <- c(dep_lines,
    paste0("\\multicolumn{13}{l}{\\textit{", g,
      if (g == "Overall") "" else paste0(" (employment-weighted mean HHI in 2000: ",
        sprintf("%.3f", dep_hhi_means[hhi_group == g, mean_hhi]), ")"),
      "}}", dep_row_end))
  for (v in names(dep_names)) {
    values <- unlist(lapply(distribution_years, function(yr)
      unlist(dep_distribution[group == g & year == yr & measure == v,
        .(p01, p25, p50, p75, p99, mean)], use.names = FALSE)), use.names = FALSE)
    cells <- ifelse(is.finite(values), sprintf("%.2f", values), "--")
    dep_lines <- c(dep_lines,
      paste0(paste(c(dep_names[v], cells), collapse = " & "), dep_row_end))
  }
}
writeLines(c(dep_lines, "\\bottomrule", "\\end{tabular}",
  "% Ties are At-or-below-median HHI. Missing baseline HHI remains in Overall but not in either HHI subgroup.",
  "% Outcome-specific valid counties are used in each year; this is not a balanced sample."
), dep_file)
message("LP dependent-variable distribution saved: ", dep_file)


# coerage stats ----------------------------------------------------------------
reg[, lodes_coverage := aux_resident_emp / employed]
summary(reg[, lodes_coverage], na.rm = T)
weighted.mean(reg[, lodes_coverage], na.rm = T, w = reg[, baseline_emp])

toplot <- reg |> fgroup_by(year) |> 
  fsummarise(lodes_jobs = fsum(aux_resident_emp), 
       labor_force = fsum(labor_force), 
       employed = fsum(employed), 
       tax_returns = fsum(returns), 
       qcew = fsum(workplace_emp) , 
       irs_nonmig = fsum(returns + returns_3_inflow+ returns_3_outflow)) |>
  data.table()


toplot <- toplot[year %in% c(2003:2015)]
toplot <- melt(toplot, id.vars = "year", variable.name = "source")

ggplot(toplot, aes(x = year, y = value, color = source))+ 
  geom_line(size = 2) + 
  theme(legend.justification = "bottom", 
        legend.position = "bottom")

# paper references ------
# Latest completed main final_v2 run; exclude heterogeneity and sensitivity folders.
final_run_dirs <- list.dirs(figs, recursive = FALSE, full.names = TRUE)
final_run_dirs <- final_run_dirs[
  grepl("^final_v2_[0-9]{8}_[0-9]{6}$", basename(final_run_dirs)) &
    file.exists(file.path(final_run_dirs, "results.csv"))
]
if (!length(final_run_dirs)) {
  stop("No final_v2 run with results.csv found in ", figs, ". Run 06_final_v2.R first.")
}
# The timestamp in the folder name determines the most recent run.
latest_final_run <- sort(final_run_dirs, decreasing = TRUE)[1L]
lp_results_file <- file.path(latest_final_run, "results.csv")
lp_results <- fread(lp_results_file)
message("LP results loaded from: ", lp_results_file)


# Scalar paper inputs join the other TeX outputs in the dated Overleaf bundle.
paper_stats_dir <- summary_stats_dir

dir.create(paper_stats_dir, recursive = TRUE, showWarnings = FALSE)

# Average annual shocks over the calendar-year windows used by the models.
# These are descriptive sample-window means, not horizon-specific estimation-sample means.
paper_shock_winsor_p <- 0.01  # Match the pooled shock caps in 06_final_v2.R.
paper_shocks <- fread(lp_input_file,
  select = c("year", "baseline_emp", "IPW_US", "empw_neighbor_IPW_US"))
paper_shocks <- paper_shocks[year > 1994 & year < 2020]
for (v in c("IPW_US", "empw_neighbor_IPW_US")) {
  x <- paper_shocks[[v]]
  q <- quantile(x, c(paper_shock_winsor_p, 1 - paper_shock_winsor_p), na.rm = TRUE)
  paper_shocks[, (paste0("w_", v)) := pmin(pmax(x, q[1L]), q[2L])]
}
paper_shock_periods <- unique(lp_results[, .(start_year, end_year)])
setorder(paper_shock_periods, start_year, end_year)
paper_shock_means <- list()
for (i in seq_len(nrow(paper_shock_periods))) {
  first_year <- paper_shock_periods$start_year[i]
  last_year <- paper_shock_periods$end_year[i]
  period <- paper_shocks[year %between% c(first_year, last_year)]
  for (role in c("own", "neighbor")) {
    variable <- if (role == "own") "w_IPW_US" else "w_empw_neighbor_IPW_US"
    x <- period[[variable]]
    w <- as.double(period$baseline_emp)
    ok <- is.finite(x) & is.finite(w) & w > 0
    average <- if (any(ok)) weighted.mean(x[ok], w[ok], na.rm = TRUE) else NA_real_
    if (!is.finite(average)) stop("No usable shock observations for ", role, ": ", first_year, "-", last_year)
    stem <- paste0("avg_", role, "_shock_", first_year, "-", last_year)
    cat(sprintf("%.3f", average),
        file = file.path(paper_stats_dir, paste0(stem, "_thousand_dollars_per_worker.tex")))
    cat(sprintf("%.0f", 1000 * average),
        file = file.path(paper_stats_dir, paste0(stem, "_dollars_per_worker.tex")))
    paper_shock_means[[paste(role, first_year, last_year)]] <- data.table(
      shock = role, start_year = first_year, end_year = last_year,
      mean_annual_shock_thousand_dollars = average, mean_annual_shock_dollars = 1000 * average)
  }
}
paper_shock_means <- rbindlist(paper_shock_means)
print(paper_shock_means)
# No CSV exports here: the paper folder contains only input-able .tex files.


# Average responses for each requested window: no sums across horizon coefficients.
# Keep main outcomes only; w_test and the industry-HHI test are intentionally excluded.
paper_outcomes <- c("aux_outside_jobs", "unemployed_share_labor_force",
  "labor_force_share_population", "sh_empl_mfg", "cum_total_domestic_migration",
  "cum_domestic_outflow_share_migration")
paper_model_types <- c("two_shock_ratio_change", "two_shock_lagged_denom_change")
paper_windows <- list("0-3" = 0:3, "1-3" = 1:3, "3-4" = 3:4)
paper_models <- lp_results[
  depvar %in% paper_outcomes & model %in% paper_model_types & shock == "w_IPW_US"
]
missing_outcomes <- setdiff(paper_outcomes, unique(paper_models$depvar))
if (length(missing_outcomes)) warning("Not in the selected results.csv; no substitute exported: ",
                                     paste(missing_outcomes, collapse = ", "))
# Fail on ambiguous repetitions, rather than pooling distinct regressions.
paper_specs <- unique(paper_models[, .(depvar, model, start_year, end_year, model_id)])
stopifnot(!anyDuplicated(paper_specs[, .(depvar, model, start_year, end_year)]))
paper_summary <- list()
for (i in seq_len(nrow(paper_specs))) {
  spec <- paper_specs[i]
  estimates <- paper_models[model_id == spec$model_id]
  shock_mean <- paper_shock_means[shock == "own" & start_year == spec$start_year &
                                   end_year == spec$end_year, mean_annual_shock_thousand_dollars]
  stopifnot(length(shock_mean) == 1L, is.finite(shock_mean))
  for (window in names(paper_windows)) {
    hs <- paper_windows[[window]]
    selected <- estimates[horizon %in% hs]
    if (nrow(selected) != length(hs) || !all(hs %in% selected$horizon) ||
        !all(is.finite(selected$coef))) {
      warning("Incomplete horizon window; skipped ", spec$model_id, " h", window)
      next
    }
    average <- mean(selected$coef)
    response_at_mean_shock <- average * shock_mean
    stem <- paste0("avg_", spec$depvar, "_", spec$start_year, "-", spec$end_year, "_h", window)
    cat(sprintf("%.3f", average), file = file.path(paper_stats_dir, paste0(stem, ".tex")))
    cat(sprintf("%.3f", response_at_mean_shock),
        file = file.path(paper_stats_dir, paste0(stem, "_at_mean_annual_shock.tex")))
    paper_summary[[paste(spec$model_id, window)]] <- data.table(
      depvar = spec$depvar, model_id = spec$model_id, start_year = spec$start_year,
      end_year = spec$end_year, horizons = window, mean_coefficient = average,
      mean_annual_shock = shock_mean, response_at_mean_annual_shock = response_at_mean_shock)
  }
}
paper_summary <- rbindlist(paper_summary)
# The scaling above is a representative annual-shock response, not a total historical effect.
# No SE for a horizon mean is inferred from separate-horizon SEs.
print(paper_summary)
message("TeX paper inputs saved to: ", paper_stats_dir)
