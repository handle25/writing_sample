################################################################################
# Two-shock heterogeneity using the existing utilities.R heterogeneity functions.
# Six outcomes split by coastal status, baseline openness, and baseline industry HHI (18 calls).
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

# Fixed county groups, matching the earlier heterogeneity definitions.
reg[, state_fips := substr(sprintf("%05d", as.integer(as.character(area_fips))), 1, 2)]
coastal_fips <- c("01", "02", "06", "09", "10", "12", "13", "15",
                  "22", "23", "24", "25", "28", "33", "34", "36",
                  "37", "41", "44", "45", "48", "51", "53")
reg[, coastal_group := fifelse(is.na(area_fips), NA_character_,
                               fifelse(state_fips %in% coastal_fips, "coastal", "other"))]
open_var <- "outside_jobs_share_resident_emp"
base_open <- reg[year == 2000 & is.finite(get(open_var)),
                 .(openness_2000 = mean(get(open_var))), by = area_fips]
stopifnot(nrow(base_open) > 0L)
median_open <- median(base_open$openness_2000, na.rm = TRUE)
base_open[, openness_group := fifelse(openness_2000 > median_open, "high", "low")]
reg[, `:=`(openness_2000 = NA_real_, openness_group = NA_character_)]
reg[base_open, on = "area_fips", `:=`(openness_2000 = i.openness_2000,
                                     openness_group = i.openness_group)]
# Missing baseline openness is excluded only from the openness split.
# Fixed industry concentration split: unweighted county median in 2000.
base_hhi <- reg[year == 2000 & is.finite(industry_hhi),
                .(industry_hhi_2000 = mean(industry_hhi)), by = area_fips]
stopifnot(nrow(base_hhi) > 0L)
median_hhi <- median(base_hhi$industry_hhi_2000, na.rm = TRUE)
base_hhi[, industry_hhi_group := fifelse(industry_hhi_2000 > median_hhi,
                                         "High industry HHI", "Low industry HHI")]
reg[, `:=`(industry_hhi_2000 = NA_real_, industry_hhi_group = NA_character_)]
reg[base_hhi, on = "area_fips", `:=`(industry_hhi_2000 = i.industry_hhi_2000,
                                    industry_hhi_group = i.industry_hhi_group)]
# Counties missing baseline HHI are excluded only from the HHI split.

group_definitions <- unique(reg[, .(area_fips, coastal_group, openness_2000, openness_group,
                                    industry_hhi_2000, industry_hhi_group)])

run_dir <- file.path(figs, paste0("final_v2_heterogeneity_", format(Sys.time(), "%Y%m%d_%H%M%S")))
figure_root <- file.path(run_dir, "figures", "appendix", "heterogeneity")
table_root <- file.path(run_dir, "tables", "appendix", "heterogeneity")
dir.create(figure_root, recursive = TRUE, showWarnings = FALSE)
dir.create(table_root, recursive = TRUE, showWarnings = FALSE)

# Auxiliary commuting: count-change LPs -----------------------------------------
figs <- file.path(figure_root, "coastal")
tables_dir <- file.path(table_root, "coastal")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
commuting_full_resident_emp_coastal <- run_lp_multiple_shocks_heterogeneity(
  reg,
  group_var = "coastal_group",
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
  figure = TRUE,
  unit = "area_fips"
)

figs <- file.path(figure_root, "openness")
tables_dir <- file.path(table_root, "openness")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
commuting_full_resident_emp_openness <- run_lp_multiple_shocks_heterogeneity(
  reg,
  group_var = "openness_group",
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
  figure = TRUE,
  unit = "area_fips"
)

figs <- file.path(figure_root, "industry_hhi")
tables_dir <- file.path(table_root, "industry_hhi")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
commuting_full_resident_emp_industry_hhi <- run_lp_multiple_shocks_heterogeneity(
  reg,
  group_var = "industry_hhi_group",
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
  figure = TRUE,
  unit = "area_fips"
)


# Ratio outcomes below use percentage-point changes in both model versions.
# Unemployment and labor-force participation ----------------------------------
figs <- file.path(figure_root, "coastal")
tables_dir <- file.path(table_root, "coastal")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
unemployment_ratio_coastal <- run_lp_ratio_multiple_shocks_heterogeneity(
  reg,
  group_var = "coastal_group",
  outcome = "unemployed_share_labor_force",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  figure = TRUE,
  unit = "area_fips"
)

figs <- file.path(figure_root, "openness")
tables_dir <- file.path(table_root, "openness")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
unemployment_ratio_openness <- run_lp_ratio_multiple_shocks_heterogeneity(
  reg,
  group_var = "openness_group",
  outcome = "unemployed_share_labor_force",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  figure = TRUE,
  unit = "area_fips"
)

figs <- file.path(figure_root, "industry_hhi")
tables_dir <- file.path(table_root, "industry_hhi")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
unemployment_ratio_industry_hhi <- run_lp_ratio_multiple_shocks_heterogeneity(
  reg,
  group_var = "industry_hhi_group",
  outcome = "unemployed_share_labor_force",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  figure = TRUE,
  unit = "area_fips"
)


figs <- file.path(figure_root, "coastal")
tables_dir <- file.path(table_root, "coastal")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
lfp_two_shock_coastal <- run_lp_ratio_multiple_shocks_heterogeneity(
  reg,
  group_var = "coastal_group",
  outcome = "labor_force_share_population",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  figure = TRUE,
  unit = "area_fips"
)

figs <- file.path(figure_root, "openness")
tables_dir <- file.path(table_root, "openness")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
lfp_two_shock_openness <- run_lp_ratio_multiple_shocks_heterogeneity(
  reg,
  group_var = "openness_group",
  outcome = "labor_force_share_population",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  figure = TRUE,
  unit = "area_fips"
)

figs <- file.path(figure_root, "industry_hhi")
tables_dir <- file.path(table_root, "industry_hhi")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
lfp_two_shock_industry_hhi <- run_lp_ratio_multiple_shocks_heterogeneity(
  reg,
  group_var = "industry_hhi_group",
  outcome = "labor_force_share_population",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  figure = TRUE,
  unit = "area_fips"
)


# Manufacturing share: 1995-2008 -----------------------------------------------
winsor(reg, "sh_empl_mfg", p = .01)
figs <- file.path(figure_root, "coastal")
tables_dir <- file.path(table_root, "coastal")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
manufacturing_two_shock_coastal <- run_lp_ratio_multiple_shocks_heterogeneity(
  reg,
  group_var = "coastal_group",
  outcome = "w_sh_empl_mfg",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2008,
  horizons = 0:7,
  figure = TRUE,
  unit = "area_fips"
)

figs <- file.path(figure_root, "openness")
tables_dir <- file.path(table_root, "openness")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
manufacturing_two_shock_openness <- run_lp_ratio_multiple_shocks_heterogeneity(
  reg,
  group_var = "openness_group",
  outcome = "w_sh_empl_mfg",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2008,
  horizons = 0:7,
  figure = TRUE,
  unit = "area_fips"
)

figs <- file.path(figure_root, "industry_hhi")
tables_dir <- file.path(table_root, "industry_hhi")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
manufacturing_two_shock_industry_hhi <- run_lp_ratio_multiple_shocks_heterogeneity(
  reg,
  group_var = "industry_hhi_group",
  outcome = "w_sh_empl_mfg",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2008,
  horizons = 0:7,
  figure = TRUE,
  unit = "area_fips"
)


# Total migration: preserve the selected 2% employment-neighbor preprocessing.
winsor(reg, "empw_neighbor_IPW_US", p = .02)
winsor(reg, "empw_neighbor_IPW_OTH", p = .02)
reg[year >= 1995, cum_domestic_inflow := cumsum(exemptions_3_inflow), by = area_fips]
reg[year >= 1995, cum_domestic_outflow := cumsum(exemptions_3_outflow), by = area_fips]
reg[, cum_total_domestic_migration := cum_domestic_inflow + cum_domestic_outflow]

figs <- file.path(figure_root, "coastal")
tables_dir <- file.path(table_root, "coastal")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
migration_total_coastal <- run_lp_multiple_shocks_heterogeneity(
  reg,
  group_var = "coastal_group",
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
  figure = TRUE,
  unit = "area_fips"
)

figs <- file.path(figure_root, "openness")
tables_dir <- file.path(table_root, "openness")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
migration_total_openness <- run_lp_multiple_shocks_heterogeneity(
  reg,
  group_var = "openness_group",
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
  figure = TRUE,
  unit = "area_fips"
)

figs <- file.path(figure_root, "industry_hhi")
tables_dir <- file.path(table_root, "industry_hhi")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
migration_total_industry_hhi <- run_lp_multiple_shocks_heterogeneity(
  reg,
  group_var = "industry_hhi_group",
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
  figure = TRUE,
  unit = "area_fips"
)


# Cumulative outward migration as a share of cumulative total migration.
reg[, cum_domestic_outflow_share_migration :=
      cum_domestic_outflow / cum_total_domestic_migration]
winsor(reg, "cum_domestic_outflow_share_migration", p = .01)

figs <- file.path(figure_root, "coastal")
tables_dir <- file.path(table_root, "coastal")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
migration_outward_share_coastal <- run_lp_ratio_multiple_shocks_heterogeneity(
  reg,
  group_var = "coastal_group",
  outcome = "w_cum_domestic_outflow_share_migration",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  figure = TRUE,
  unit = "area_fips"
)

figs <- file.path(figure_root, "openness")
tables_dir <- file.path(table_root, "openness")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
migration_outward_share_openness <- run_lp_ratio_multiple_shocks_heterogeneity(
  reg,
  group_var = "openness_group",
  outcome = "w_cum_domestic_outflow_share_migration",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  figure = TRUE,
  unit = "area_fips"
)

figs <- file.path(figure_root, "industry_hhi")
tables_dir <- file.path(table_root, "industry_hhi")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
migration_outward_share_industry_hhi <- run_lp_ratio_multiple_shocks_heterogeneity(
  reg,
  group_var = "industry_hhi_group",
  outcome = "w_cum_domestic_outflow_share_migration",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015,
  horizons = 0:7,
  figure = TRUE,
  unit = "area_fips"
)

final_results <- list(
  commuting_full_resident_emp_industry_hhi = commuting_full_resident_emp_industry_hhi,
  unemployment_ratio_industry_hhi = unemployment_ratio_industry_hhi,
  lfp_two_shock_industry_hhi = lfp_two_shock_industry_hhi,
  manufacturing_two_shock_industry_hhi = manufacturing_two_shock_industry_hhi,
  migration_total_industry_hhi = migration_total_industry_hhi,
  migration_outward_share_industry_hhi = migration_outward_share_industry_hhi,
  commuting_full_resident_emp_coastal = commuting_full_resident_emp_coastal,
  commuting_full_resident_emp_openness = commuting_full_resident_emp_openness,
  unemployment_ratio_coastal = unemployment_ratio_coastal,
  unemployment_ratio_openness = unemployment_ratio_openness,
  lfp_two_shock_coastal = lfp_two_shock_coastal,
  lfp_two_shock_openness = lfp_two_shock_openness,
  manufacturing_two_shock_coastal = manufacturing_two_shock_coastal,
  manufacturing_two_shock_openness = manufacturing_two_shock_openness,
  migration_total_coastal = migration_total_coastal,
  migration_total_openness = migration_total_openness,
  migration_outward_share_coastal = migration_outward_share_coastal,
  migration_outward_share_openness = migration_outward_share_openness
)
# Tables for each panel, retaining all estimated horizons.
for (spec in names(final_results)) {
  result <- final_results[[spec]]
  split <- if (grepl("_coastal$", spec)) "coastal" else if (grepl("_industry_hhi$", spec)) "industry_hhi" else "openness"
  for (g in unique(result$group)) {
    panel <- result[group == g]
    export_lp_table(panel, outcome = spec, shock_1 = "w_IPW_US",
      shock_2 = "w_empw_neighbor_IPW_US", horizons = sort(unique(panel$h)),
      output_dir = file.path(table_root, split),
      prefix = paste0(spec, "_", g, "_", date))
  }
}
saveRDS(final_results, file.path(run_dir, paste0("heterogeneity_results_", date, ".rds")))
saveRDS(list(groups = group_definitions, openness_variable = open_var,
             openness_median = median_open, industry_hhi_median = median_hhi, coastal_state_fips = coastal_fips),
        file.path(run_dir, "heterogeneity_group_definitions.rds"))
message("Figures: ", figure_root, "\nTables: ", table_root)