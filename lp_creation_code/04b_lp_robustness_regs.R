################################################################################
# Created 8.15.2026
# Author: Sophie Handley
# Purpose: Run LP regressions and placebo tests
#
# Placebo 1:
# Randomly reassign entire county shock paths to another county.
#
# Placebo 2:
# Randomly reshuffle shock timing within county, keeping the US and OTH
# shock pair together.
#
# All LP outcomes use winsorized level outcomes.
# Shocks are winsorized at 1st / 99th percentiles before estimation.
################################################################################

rm(list = ls())

library(data.table)
library(fixest)
library(ggplot2)

################################################################################
# Data
################################################################################

path <- "D:/writing_sample/data"
setwd(path)

reg <- fread(
  paste0(
    path,
    "/output/lp_transformed_reg.csv"
  )
)

# Trade shock available through 2017
reg <- reg[year <= 2017]

setorder(
  reg,
  area_fips,
  year
)

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
################################################################################
# Outcome
################################################################################

# Examples:
#
# w_outside_jobs_share_resident_emp
# w_outside_earn3333_jobs_share_resident_emp
# w_outside_servc_jobs_share_resident_emp
# w_outside_servc_jobs_share_service_jobs
# w_outside_jobs_share_population

base <- "w_outside_servc_jobs_share_resident_emp"


################################################################################
# Winsorize shocks
################################################################################

for (v in c("IPW_US", "IPW_OTH")) {
  
  w_var <- paste0(
    "w_",
    v
  )
  
  q01 <- quantile(
    reg[[v]],
    .01,
    na.rm = TRUE
  )
  
  q99 <- quantile(
    reg[[v]],
    .99,
    na.rm = TRUE
  )
  
  reg[, (w_var) :=
        get(v)]
  
  reg[
    get(v) <= q01,
    (w_var) := q01
  ]
  
  reg[
    get(v) >= q99,
    (w_var) := q99
  ]
}


################################################################################
# Outcome lags
################################################################################

reg[
  ,
  l1_y :=
    shift(
      get(base),
      n = 1,
      type = "lag"
    ),
  by = area_fips
]

reg[
  ,
  l2_y :=
    shift(
      get(base),
      n = 2,
      type = "lag"
    ),
  by = area_fips
]


reg[, l1_shock := shift(IPW_US, 1), by = area_fips]
reg[, l2_shock := shift(IPW_US, 2), by = area_fips]

################################################################################
# LP outcomes
#
# y_(t+h) - y_(t-1)
################################################################################

for (h in 0:10) {
  
  var <- paste0(
    "diff_",
    h
  )
  
  reg[
    ,
    (var) :=
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
    by = area_fips
  ]
}


################################################################################
# LP function
################################################################################

run_lp <- function(data,
                   shock_us,
                   shock_oth,
                   specification) {
  
  results <- data.table(
    h = 0:10,
    coef = NA_real_,
    se = NA_real_,
    N = NA_integer_
  )
  
  for (hh in 0:9) {
    
    var <- paste0(
      "diff_",
      hh
    )
    # var <- paste0("w_diff_share_t0_", hh)
    mod <- feols(
      as.formula(
        paste0(
          var,
          " ~ l_shind_manuf + l1_y + l2_y + l1_shock + l2_shock | area_fips + year |  ",
          shock_us,
          " ~ ",
          shock_oth
        )
      ),
      data = data,
      cluster = ~statefip+year
    )
    
    coef_name <- paste0(
      "fit_",
      shock_us
    )
    
    results[
      h == hh,
      `:=`(
        coef = coef(mod)[coef_name],
        se = se(mod)[coef_name],
        N = nobs(mod)
      )
    ]
  }
  
  results[
    ,
    lower :=
      coef - 1.96 * se
  ]
  
  results[
    ,
    upper :=
      coef + 1.96 * se
  ]
  
  results[
    ,
    specification :=
      specification
  ]
  
  return(results)
}


################################################################################
# Actual LP
################################################################################

actual_results <- run_lp(
  data = reg,
  shock_us = "w_IPW_US",
  shock_oth = "w_IPW_OTH",
  specification = "Actual"
)


################################################################################
# PLACEBO 1
#
# Randomly reassign entire shock series across counties.
#
# This preserves:
# - complete time ordering of the assigned shock series
# - relationship between US and OTH shock
#
# This breaks:
# - geographic assignment of exposure
################################################################################

set.seed(123)

county_map <- data.table(
  area_fips =
    sort(
      unique(reg$area_fips)
    )
)

county_map[
  ,
  shock_fips :=
    sample(area_fips)
]


# Shock panel indexed by original shock county
shock_panel <- reg[
  ,
  .(
    shock_fips = area_fips,
    year,
    placebo1_IPW_US = w_IPW_US,
    placebo1_IPW_OTH = w_IPW_OTH
  )
]


# Attach a random shock county to every outcome county
placebo1 <- merge(
  reg,
  county_map,
  by = "area_fips",
  all.x = TRUE
)


# Bring in that county's full shock history
placebo1 <- merge(
  placebo1,
  shock_panel,
  by = c(
    "shock_fips",
    "year"
  ),
  all.x = TRUE
)


# Check row counts
print(
  c(
    original = nrow(reg),
    placebo1 = nrow(placebo1)
  )
)


placebo1_results <- run_lp(
  data = placebo1,
  shock_us = "placebo1_IPW_US",
  shock_oth = "placebo1_IPW_OTH",
  specification = "County reassignment"
)


################################################################################
# PLACEBO 2
#
# Shuffle timing of shocks within each county.
#
# US and OTH shocks are moved together using the SAME random permutation.
#
# This preserves:
# - county-specific distribution of exposure
# - US / OTH relationship
#
# This breaks:
# - actual timing of exposure
################################################################################

set.seed(456)

placebo2_shocks <- reg[
  ,
  {
    
    idx <- sample(.N)
    
    .(
      year,
      placebo2_IPW_US =
        w_IPW_US[idx],
      
      placebo2_IPW_OTH =
        w_IPW_OTH[idx]
    )
  },
  by = area_fips
]


placebo2 <- merge(
  reg,
  placebo2_shocks,
  by = c(
    "area_fips",
    "year"
  ),
  all.x = TRUE
)


# Check row counts
print(
  c(
    original = nrow(reg),
    placebo2 = nrow(placebo2)
  )
)


placebo2_results <- run_lp(
  data = placebo2,
  shock_us = "placebo2_IPW_US",
  shock_oth = "placebo2_IPW_OTH",
  specification = "Within-county timing shuffle"
)


################################################################################
# Combine results
################################################################################

results_all <- rbindlist(
  list(
    actual_results,
    placebo1_results,
    placebo2_results
  ),
  fill = TRUE
)


################################################################################
# Inspect coefficients and confidence intervals
################################################################################

print(
  results_all[
    ,
    .(
      specification,
      h,
      coef,
      se,
      lower,
      upper,
      N
    )
  ]
)


################################################################################
# Individual plots with 95% confidence intervals
################################################################################

ggplot(
  actual_results,
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
    alpha = .2
  ) +
  geom_line() +
  geom_point() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Actual China Shock",
    x = "Horizon",
    y = "Coefficient"
  ) +
  theme_bw()


ggplot(
  placebo1_results,
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
    alpha = .2
  ) +
  geom_line() +
  geom_point() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Placebo: County Reassignment",
    x = "Horizon",
    y = "Coefficient"
  ) +
  theme_bw()


ggplot(
  placebo2_results,
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
    alpha = .2
  ) +
  geom_line() +
  geom_point() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Placebo: Within-County Timing Shuffle",
    x = "Horizon",
    y = "Coefficient"
  ) +
  theme_bw()


################################################################################
# Faceted plot: actual and both placebos with confidence intervals
################################################################################

ggplot(
  results_all,
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
    alpha = .2
  ) +
  geom_line() +
  geom_point() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ specification,
    scales = "free_y"
  ) +
  labs(
    x = "Horizon",
    y = "Coefficient"
  ) +
  theme_bw()


################################################################################
# Save results
################################################################################

fwrite(
  results_all,
  paste0(
    path,
    "/output/lp_placebo_results.csv"
  )
)