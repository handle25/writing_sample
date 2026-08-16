################################################################################
# Created 8.15.2026 
# Author: Sophie Handley 
# Purpose: Run LP regressions
################################################################################


rm(list = ls())

# qcewdata 
path <- "D:/writing_sample/data"
setwd(path)


reg <- fread(paste0(path,"/output/lp_transformed_reg.csv"))


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
base <- "w_outside_servc_jobs_share_resident_emp"

reg[, l1_y := shift(get(base), 1),
    by = area_fips]

reg[, l2_y := shift(get(base), 2),
    by = area_fips]

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

for (hh in 0:9) {
  
  var <- paste0("diff_", hh)
  # var <- paste0("w_diff_share_t0_", hh)
  
  mod <- feols(
    as.formula(
      paste0(
        var,
        " ~ population  + l1_y  + l2_y +l1_shock + l2_shock + l3_shock | area_fips + year |",
        "IPW_US ~ IPW_OTH"
      )
    ),
    data = reg,
    cluster = ~area_fips+year
  )
  
  results[h == hh, `:=`(
    coef = coef(mod)["fit_IPW_US"],
    se   = se(mod)["fit_IPW_US"]
  )]
}

results[, lower := coef - 1.96 * se]
results[, upper := coef + 1.96 * se]

ggplot(results, aes(x = h, y = coef)) +
  geom_line() +
  geom_point() +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()