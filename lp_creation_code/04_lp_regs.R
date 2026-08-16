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

for (h in c(0:10)){
  var <- paste0("diff_", h)
  
  reg[, (var) := 
        shift(outside_jobs_share_resident_emp, type = "lead", n = h) - 
        shift(outside_jobs_share_resident_emp, type = "lag", n = 1),
      by = .(area_fips)]
  
  
}

results <- data.table(
  h = 0:10,
  coef = NA_real_,
  se = NA_real_
)

for (hh in 0:10) {
  
  var <- paste0("diff_", hh)
  
  mod <- feols(
    as.formula(
      paste0(
        var,
        " ~ as.factor(year) + l_shind_manuf | ",
        "IPW_US ~ IPW_OTH"
      )
    ),
    data = reg,
    cluster = ~statefip
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