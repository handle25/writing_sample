################################################################################
# Created 8.15.2026 
# Author: Sophie Handley 
# Purpose: Run LP regressions
################################################################################


rm(list = ls())

# qcewdata 
path <- "D:/writing_sample/data"
setwd(path)

acs <- fread(paste0(path, "/acs/acs_all.csv"))
acs[, area_fips := as.integer(paste0(sprintf("%02.0f", state), sprintf("%03.0f", county)))]

reg <- fread(paste0(path,"/output/lp_transformed_reg.csv"))

reg <- merge(reg, acs, 
             by = c("area_fips", "year"), 
             all.x = TRUE)

for (v in c("IPW_US", "IPW_OTH")) {
  
  q01 <- quantile(reg[[v]], 0.01, na.rm = TRUE)
  q99 <- quantile(reg[[v]], 0.99, na.rm = TRUE)
  
  reg[get(v) < q01, (v) := q01]
  reg[get(v) > q99, (v) := q99]
}


base_t0 <- "outside_jobs"

for (h in 0:10) {
  
  var   <- paste0("diff_share_t0_", h)
  w_var <- paste0("w_diff_share_t0_", h)
  
  # Construct LP outcome
  reg[, (var) :=
        (
          shift(get(base_t0), type = "lead", n = h) -
            shift(get(base_t0), type = "lag", n = 1)
        ) /
        shift(resident_emp, type = "lag", n = 1),
      by = area_fips]
  
  # Winsorize at 1st / 99th percentiles
  q <- quantile(reg[[var]], c(0.01, 0.99), na.rm = TRUE)
  
  reg[, (w_var) := pmin(
    pmax(get(var), q[1]),
    q[2]
  )]
}

# w_outside_earn3333_jobs_share_resident_emp 
# w_outside_jobs_share_resident_emp outside_servc_jobs_share_resident_emp w_outside_servc_jobs_share_service_jobs w_outside_jobs_share_population
base <- "w_outside_servc_jobs_share_service_jobs"

pop02 <- reg[year == 2005,] |> 
  fmutate(pop02 = log(population)) |> 
  fselect(area_fips, pop02)

reg <- merge(
  reg, pop02, 
  by = c("area_fips"), 
  all.x = TRUE
)

reg[, migration_share_pop := net_migration / population]

reg[, l1_y := shift(get(base), 1),
    by = area_fips]

reg[, l2_y := shift(get(base), 2),
    by = area_fips]

reg[, l3_y := shift(get(base), 3),
    by = area_fips]

reg[, l4_y := shift(get(base), 4),
    by = area_fips]

reg[, returns_3_outflow := log(as.integer(returns_3_outflow))]

reg[, l1_shock := shift(IPW_US, 1), by = area_fips]
reg[, l2_shock := shift(IPW_US, 2), by = area_fips]
reg[, l3_shock := shift(IPW_US, 3), by = area_fips]
reg[, l4_shock := shift(IPW_US, 4), by = area_fips]
reg[, d_pop  := (population - shift(population, 1))/shift(population, 1), by = area_fips]

for (h in c(0:10)){
  var <- paste0("diff_", h)
  
  reg[, (var) := 
        shift(get(base), type = "lead", n = h) - 
        shift(get(base), type = "lag", n = 1),
      by = .(area_fips)]
  
  
}

results <- data.table(
  h = 0:10,
  coef = NA_real_,
  se = NA_real_
)
# + i(year, pop02) control 
for (hh in 0:9) {
  
  var <- paste0("diff_", hh)
  # var <- paste0("w_diff_share_t0_", hh)
  
  mod <- feols(
    as.formula(
      paste0(
        var,
        " ~ migration_share_pop + l1_y  + l2_y + l3_y + l4_y |  year |",
        "IPW_US ~ IPW_OTH"
      )
    ),
    data = reg,
    cluster = ~area_fips+year
  )
  print(summary(mod))
  
  results[h == hh, `:=`(
    coef = coef(mod)["fit_IPW_US"],
    se   = se(mod)["fit_IPW_US"]
  )]
}

results[, lower90 := coef - 1.64 * se]
results[, upper90 := coef + 1.64 * se]

results[, lower95 := coef - 1.96 * se]
results[, upper95 := coef + 1.96 * se]

ggplot(results, aes(x = h, y = coef)) +
  geom_line() +
  geom_point() +
  geom_ribbon(
    aes(ymin = lower95, ymax = upper95),
    alpha = 0.2
  ) +
  geom_ribbon(
    aes(ymin = lower90, ymax = upper90),
    alpha = 0.5
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()


################################################################################
# LP: Mean travel time to work
################################################################################

base_commute <- "mean_travel_time_to_work_minutes"

# Create cumulative LP outcomes
for (h in 0:7) {
  
  var <- paste0("diff_commute_time_", h)
  
  reg[, (var) :=
        shift(get(base_commute), type = "lead", n = h) -
        shift(get(base_commute), type = "lag", n = 1),
      by = area_fips]
}

################################################################################
# Lagged commute-time controls
################################################################################

reg[, l1_commute := shift(get(base_commute), 1),
    by = area_fips]

reg[, l2_commute := shift(get(base_commute), 2),
    by = area_fips]

reg[, l3_commute := shift(get(base_commute), 3),
    by = area_fips]

reg[, l4_commute := shift(get(base_commute), 4),
    by = area_fips]


################################################################################
# Run LPs
################################################################################

results_commute <- data.table(
  h = 0:7,
  coef = NA_real_,
  se = NA_real_
)

for (hh in 0:7) {
  
  var <- paste0("diff_commute_time_", hh)
  
  mod_commute <- feols(
    as.formula(
      paste0(
        var,
        " ~ l1_commute + l2_commute + l3_commute + l4_commute | year | ",
        "IPW_US ~ IPW_OTH"
      )
    ),
    data = reg,
    cluster = ~area_fips + year
  )
  
  print(summary(mod_commute))
  
  results_commute[h == hh, `:=`(
    coef = coef(mod_commute)["fit_IPW_US"],
    se   = se(mod_commute)["fit_IPW_US"]
  )]
}


################################################################################
# Confidence intervals
################################################################################

results_commute[, `:=`(
  lower90 = coef - 1.64 * se,
  upper90 = coef + 1.64 * se,
  lower95 = coef - 1.96 * se,
  upper95 = coef + 1.96 * se
)]


################################################################################
# Plot
################################################################################

ggplot(results_commute, aes(x = h, y = coef)) +
  geom_line() +
  geom_point() +
  geom_ribbon(
    aes(ymin = lower95, ymax = upper95),
    alpha = 0.2
  ) +
  geom_ribbon(
    aes(ymin = lower90, ymax = upper90),
    alpha = 0.5
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = 0:7
  ) +
  labs(
    x = "Horizon",
    y = "Change in Mean Commute Time (Minutes)"
  ) +
  theme_bw()