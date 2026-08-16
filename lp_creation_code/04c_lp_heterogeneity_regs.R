################################################################################
# Created 8.15.2026
# Author: Sophie Handley
# Purpose: LP heterogeneity by baseline manufacturing share
################################################################################

rm(list = ls())

path <- "D:/writing_sample/data"
setwd(path)

reg <- fread(
  paste0(path, "/output/lp_transformed_reg.csv")
)

setorder(reg, area_fips, year)


################################################################################
# Create baseline manufacturing-share terciles
################################################################################

# Take first non-missing lagged manufacturing share observed for each county
county_q <- reg[
  !is.na(l_shind_manuf),
  .(
    baseline_manuf_share = first(l_shind_manuf)
  ),
  by = area_fips
]

# Rank counties, then split into three equally sized groups
county_q[, pct_rank :=
           frank(
             baseline_manuf_share,
             ties.method = "average"
           ) / .N]

county_q[, manuf_quantile :=
           fcase(
             pct_rank <= 1/3, "Low",
             pct_rank <= 2/3, "Middle",
             default = "High"
           )]

county_q[, manuf_quantile :=
           factor(
             manuf_quantile,
             levels = c("Low", "Middle", "High")
           )]

reg <- merge(
  reg,
  county_q[, .(
    area_fips,
    baseline_manuf_share,
    manuf_quantile
  )],
  by = "area_fips",
  all.x = TRUE
)

# Check split
reg[, uniqueN(area_fips), by = manuf_quantile]


################################################################################
# Outcome
################################################################################

base <- "w_outside_jobs_share_resident_emp"
q99 <- quantile(reg[,resident_emp_population_ratio], .99, na.rm = TRUE)
q01 <- quantile(reg[,resident_emp_population_ratio], .01, na.rm = TRUE)

reg[resident_emp_population_ratio > q99, resident_emp_population_ratio := q99]
reg[resident_emp_population_ratio < q01, resident_emp_population_ratio := q01]

base <- "w_outside_jobs_share_resident_emp"

reg[, l1_y :=
      shift(get(base), 1),
    by = area_fips]

reg[, l2_y :=
      shift(get(base), 2),
    by = area_fips]


################################################################################
# Shock lags
################################################################################

reg[, l1_shock :=
      shift(IPW_US, 1),
    by = area_fips]

reg[, l2_shock :=
      shift(IPW_US, 2),
    by = area_fips]

reg[, l3_shock :=
      shift(IPW_US, 3),
    by = area_fips]


################################################################################
# LP outcomes
################################################################################

for (h in 0:10) {
  
  var <- paste0("diff_", h)
  
  reg[, (var) :=
        shift(
          get(base),
          type = "lead",
          n = h
        ) -
        shift(
          get(base),
          type = "lag",
          n = 1
        ),
      by = area_fips]
}


################################################################################
# Results table
################################################################################

results <- CJ(
  h = 0:10,
  manuf_quantile = factor(
    c("Low", "Middle", "High"),
    levels = c("Low", "Middle", "High")
  )
)

results[, `:=`(
  coef = NA_real_,
  se = NA_real_,
  N = NA_integer_
)]


################################################################################
# LP regressions by manufacturing-share tercile
################################################################################

for (qq in c("Low", "Middle", "High")) {
  
  for (hh in 0:10) {
    
    var <- paste0("diff_", hh)
    
    mod <- feols(
      as.formula(
        paste0(
          var,
          " ~ population + l1_y + l2_y + ",
          "l1_shock + l2_shock + l3_shock | ",
          "area_fips + year | ",
          "IPW_US ~ IPW_OTH"
        )
      ),
      data = reg[manuf_quantile == qq],
      cluster = ~area_fips + year
    )
    
    results[
      h == hh & manuf_quantile == qq,
      `:=`(
        coef = coef(mod)["fit_IPW_US"],
        se = se(mod)["fit_IPW_US"],
        N = nobs(mod)
      )
    ]
  }
}


################################################################################
# Confidence intervals
################################################################################

results[, lower := coef - 1.96 * se]
results[, upper := coef + 1.96 * se]


################################################################################
# Plot
################################################################################

ggplot(
  results,
  aes(
    x = h,
    y = coef
  )
) +
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.2
  ) +
  geom_line() +
  geom_point() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~manuf_quantile, 
    scales = "free_y"
  ) +
  labs(
    x = "Horizon",
    y = "Coefficient",
    title = "LP Response by Baseline Manufacturing Share"
  ) +
  theme_bw()


################################################################################
# Save
################################################################################

# fwrite(
#   results,
#   paste0(
#     path,
#     "/output/lp_heterogeneity_manuf_quantiles.csv"
#   )
# )

################################################################################
# Regional heterogeneity
################################################################################

# MI, WI, IL, IN, OH, PA
midwest_states <- c(
  26, # Michigan
  55, # Wisconsin
  17, # Illinois
  18, # Indiana
  39#, # Ohio
  #42  # Pennsylvania
)

reg[, midwest := statefip %in% midwest_states]

reg[, region_group :=
      fifelse(
        midwest,
        "Industrial Midwest/PA",
        "Rest of US"
      )]

reg[, region_group :=
      factor(
        region_group,
        levels = c(
          "Industrial Midwest/PA",
          "Rest of US"
        )
      )]

# Check counties in each group
reg[, uniqueN(area_fips), by = region_group]


################################################################################
# Results table
################################################################################

region_results <- CJ(
  h = 0:10,
  region_group = factor(
    c(
      "Industrial Midwest/PA",
      "Rest of US"
    ),
    levels = c(
      "Industrial Midwest/PA",
      "Rest of US"
    )
  )
)

region_results[, `:=`(
  coef = NA_real_,
  se = NA_real_,
  N = NA_integer_
)]


################################################################################
# LP regressions by region
################################################################################

for (rr in c(
  "Industrial Midwest/PA",
  "Rest of US"
)) {
  
  for (hh in 0:10) {
    
    var <- paste0("diff_", hh)
    
    mod <- feols(
      as.formula(
        paste0(
          var,
          " ~ population + l1_y + l2_y + ",
          "l1_shock + l2_shock + l3_shock | ",
          "area_fips + year | ",
          "IPW_US ~ IPW_OTH"
        )
      ),
      data = reg[region_group == rr],
      cluster = ~area_fips + year
    )
    
    region_results[
      h == hh & region_group == rr,
      `:=`(
        coef = coef(mod)["fit_IPW_US"],
        se = se(mod)["fit_IPW_US"],
        N = nobs(mod)
      )
    ]
  }
}


################################################################################
# Confidence intervals
################################################################################

region_results[, lower := coef - 1.96 * se]
region_results[, upper := coef + 1.96 * se]


################################################################################
# Plot regional heterogeneity
################################################################################

ggplot(
  region_results,
  aes(
    x = h,
    y = coef
  )
) +
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.2
  ) +
  geom_line() +
  geom_point() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~region_group,
    scales = "free_y"
  ) +
  labs(
    x = "Horizon",
    y = "Coefficient",
    title = "LP Response by Region"
  ) +
  theme_bw()


################################################################################
# Save regional heterogeneity results
################################################################################

fwrite(
  region_results,
  paste0(
    path,
    "/output/lp_heterogeneity_region.csv"
  )
)