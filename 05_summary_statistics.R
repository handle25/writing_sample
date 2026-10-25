path <- "D:/writing_sample/data"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
date <- Sys.Date()
# Match the data vintage used in 06_final_v2.R; edit this to select another build.
input_date <- "2026-10-25"
lp_input_file <- file.path(path, "output", paste0("lp_transformed_reg_", input_date, ".csv"))
figs <- "D:/writing_sample/figures"

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
# ## dep vars -------------------------------------------------------------------
# file <- file.path(figs, paste0("dep_var_distribution_", date, ".tex"))
# p <- c(.01,.25,.50,.75,.99)
# d <- 2
# 
# # to_summarize <- c("w_d_labor_force_share_population", "w_d_unemployed_share_labor_force",
#                   # "w_d_outside_jobs_share_population", "w_d_net_outmigration")
# 
# cat("\\begin{tabular}{l", rep("c", length(p)), "c} \n", file=file)
# cat("\\toprule \n & \\multicolumn{", length(p), "}{c}{Percentile} & \\\\ \n \\cmidrule{2-6}\n", file=file, append=T)
# cat(" & 1 & 25 & 50 & 75 & 99 & Mean \\\\\n \\midrule\n", file=file, append=T)
# 
# cat("\\multicolumn{7}{l}{\\textit{Panel A: 2000--2007}} \\\\\n", file=file, append=T)
# for (v in to_summarize) {
#   panel <- reg[year %in% 2000:2007, .(value = get(v), weight = baseline_emp)]
#   # Use the same valid observations and baseline employment weights for both statistics.
#   panel <- panel[is.finite(value) & is.finite(weight) & weight > 0]
#   if (nrow(panel) == 0L) {
#     q <- rep(NA_real_, length(p))
#     m <- NA_real_
#   } else {
#     q <- round(Hmisc::wtd.quantile(panel$value, weights = panel$weight,
#                                    probs = p, normwt = TRUE, na.rm = TRUE), d)
#     m <- round(weighted.mean(panel$value, w = panel$weight, na.rm = TRUE), 3)
#   }
#   cat(names_dict[v], q, m, sep="&", file=file, append=T)
#   cat("\\\\\n", file=file, append=T)
# }
# 
# cat("\\addlinespace\n", file=file, append=T)
# cat("\\multicolumn{7}{l}{\\textit{Panel B: 2007--2013}} \\\\\n", file=file, append=T)
# for (v in to_summarize) {
#   panel <- reg[year %in% 2007:2013, .(value = get(v), weight = baseline_emp)]
#   # Use the same valid observations and baseline employment weights for both statistics.
#   panel <- panel[is.finite(value) & is.finite(weight) & weight > 0]
#   if (nrow(panel) == 0L) {
#     q <- rep(NA_real_, length(p))
#     m <- NA_real_
#   } else {
#     q <- round(Hmisc::wtd.quantile(panel$value, weights = panel$weight,
#                                    probs = p, normwt = TRUE, na.rm = TRUE), d)
#     m <- round(weighted.mean(panel$value, w = panel$weight, na.rm = TRUE), 3)
#   }
#   cat(names_dict[v], q, m, sep="&", file=file, append=T)
#   cat("\\\\\n", file=file, append=T)
# }
# 
# cat("\\bottomrule \n\\end{tabular}", file=file, append=T)
# 
# 
# 
# local projections 
reg <- fread(lp_input_file)

file <- file.path(figs, paste0("lp_shock_distribution_", date, ".tex"))
p <- c(.01,.05,.25,.50,.75,.90,.99) 
means <- 0 
cat("\\begin{tabular}{l", rep("c", length(p)), "c} \n", file = file)
cat("\\toprule \n & \\multicolumn{", length(p), "}{c}{Percentile}  \\\\ \n \\cmidrule{2-8}  ",  
    file = file, append = T)
cat(" & 1 & 5 & 25 & 50 & 75 & 90 & 99 & Mean \\\\\n \\midrule ",  file = file, append = T)

for (y in 2000:2013){
  panel <- reg[year == y, .(value = w_IPW_US, weight = baseline_emp)]
  panel <- panel[is.finite(value) & is.finite(weight) & weight > 0]
  sum <- if (nrow(panel)) round(Hmisc::wtd.quantile(panel$value,
    weights = panel$weight, probs = p, normwt = TRUE, na.rm = TRUE), d) else rep(NA_real_, length(p))
  mean <- if (nrow(panel)) round(weighted.mean(panel$value, panel$weight, na.rm = TRUE), 3) else NA_real_
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

ggsave(paste0(path, "/../figures/map_", var, "_", date, ".pdf"))



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
snapshot_columns <- c("state", paste0("2000_", snapshot_vars), paste0("2019_", snapshot_vars))
setcolorder(snapshot_wide, snapshot_columns)
setorder(snapshot_wide, state)
dir.create(figs, recursive = TRUE, showWarnings = FALSE)
snapshot_csv <- file.path(figs, paste0("state_outcome_snapshot_2000_2019_", date, ".csv"))
snapshot_tex <- file.path(figs, paste0("state_outcome_snapshot_2000_2019_", date, ".tex"))
fwrite(snapshot_wide, snapshot_csv, na = "NA")

# Requires booktabs and longtable; headers repeat across pages.
row_end <- paste0(" ", intToUtf8(c(92,92)))
header <- c("\\toprule",
  paste0("State & \\multicolumn{4}{c}{2000} & \\multicolumn{4}{c}{2019}", row_end),
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
  paste0(" & Comm. out & LFP & Unemp. & Mfg. & Comm. out & LFP & Unemp. & Mfg.", row_end),
  "\\midrule")
format_snapshot <- function(x) ifelse(is.finite(x), sprintf("%.2f", x), "--")
body <- vapply(seq_len(nrow(snapshot_wide)), function(i) {
  values <- unlist(snapshot_wide[i, snapshot_columns[-1L], with = FALSE], use.names = FALSE)
  paste0(paste(c(snapshot_wide$state[i], format_snapshot(values)), collapse = " & "), row_end)
}, character(1))
writeLines(c(
  "% All entries are baseline-employment-weighted percentages from the dated LP input.",
  "\\begin{longtable}{lrrrrrrrr}",
  paste0("\\caption{State outcome snapshots, 2000 and 2019}\\label{tab:state_outcome_snapshot}", row_end),
  header, "\\endfirsthead", header, "\\endhead",
  paste0("\\midrule \\multicolumn{9}{r}{Continued on next page}", row_end), "\\endfoot",
  "\\bottomrule", "\\endlastfoot", body, "\\end{longtable}",
  "% Comm. out = full main-plus-auxiliary outside-county jobs / resident jobs;",
  "% LFP = labor force / population; Unemp. = unemployed / labor force; Mfg. = manufacturing employment share.",
  "% All means use baseline_emp weights; invalid values/weights are excluded separately for each measure.",
  "% Counties observed in each year are used; the two snapshots need not be a balanced sample."
), snapshot_tex)
message("State snapshots saved: ", snapshot_tex, " and ", snapshot_csv)
