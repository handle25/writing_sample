

rm(list = ls())

# qcewdata 
path <- "D:/writing_sample/data"
setwd(path)

lp_reg <- fread(paste0(path,"/output/lp_transformed_reg.csv"))

baseline <- fread(paste0(path, "/output/transformed_reg.csv"))


# One observation per county containing fixed long-difference shock
shock_ld <- baseline[
  year == 2007,
  .(
    area_fips,
    shock_ld_US  = IPW_US,
    shock_ld_OTH = IPW_OTH
  )
]

# Merge onto annual LP panel
reg_dynamic <- merge(
  lp_reg,
  shock_ld,
  by = "area_fips",
  all.x = TRUE
)

years <- sort(unique(reg_dynamic$year))
years <- c(2004:2016)

results <- data.table(
  year = years,
  coef = NA_real_,
  se   = NA_real_
)

for (y in years) {
  
  mod <- feols(
    outside_d_goods_jobs ~ 1 |
      shock_ld_US ~ shock_ld_OTH,
    data = reg_dynamic[year == y,],
    weights = ~baseline_emp,
    cluster = ~statefip
  )
  
  results[year == y, `:=`(
    coef = coef(mod)["fit_shock_ld_US"],
    se   = se(mod)["fit_shock_ld_US"]
  )]
}

results[, `:=`(
  lower = coef - 1.96 * se,
  upper = coef + 1.96 * se
)]

ggplot(results, aes(x = year, y = coef)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2
  ) +
  geom_line() +
  geom_point() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = years
  ) +
  labs(
    x = "Year",
    y = "IV coefficient"
  ) +
  theme_bw()


exit 

reg_dynamic[, year_fac := factor(year)]

model <- ivreg(
  d_resident_emp_population_ratio ~
    factor(area_fips) +
    year_fac +
    shock_ld_US:year_fac |
    factor(area_fips) +
    year_fac +
    shock_ld_OTH:year_fac,
  data = reg_dynamic[],
  weights = baseline_emp
)

results <- feols(
  d_resident_emp_population_ratio ~  1 | area_fips + year
  | i(year, shockUS) ~ i(year, shock_ld_OTH), 
  data = reg_dynamic,
  weights = baseline_emp
)

vcov_tw <- vcovCL(
  model,
  cluster = ~ statefip + year
)

coeftest(model, vcov = vcov_tw)
coefs <- coef(model)
ses   <- sqrt(diag(vcov_tw))

# Keep only year × endogenous shock coefficients
keep <- grepl("year_fac.*:shock_ld_US", names(coefs))

results <- data.table(
  term = names(coefs)[keep],
  coef = coefs[keep],
  se   = ses[keep]
)

# Extract year from coefficient name
results[, year := as.integer(
  sub("year_fac([0-9]+):shock_ld_US", "\\1", term)
)]

results[, `:=`(
  lower = coef - 1.96 * se,
  upper = coef + 1.96 * se
)]

setorder(results, year)

ggplot(results, aes(x = year, y = coef)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2
  ) +
  geom_line() +
  geom_point() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = results$year
  ) +
  labs(
    x = "Year",
    y = "IV coefficient",
    title = "Dynamic Effects of Long-Difference China Shock"
  ) +
  theme_bw()




