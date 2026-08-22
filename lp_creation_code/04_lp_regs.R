################################################################################
# Created 8.15.2026 
# Author: Sophie Handley 
# Purpose: Run LP regressions
################################################################################


rm(list = ls())
figure_1 <- T
figure_2 <- T
figure_3 <- T
figure_4 <- T

# qcewdata 
path <- "D:/writing_sample/data"
figs <- "D:/writing_sample/figures"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
setwd(path)
# function definition ----------------------------------------------------------
run_lp <- function(
    reg,
    outcome,
    start_year = 2000,
    end_year = 2014,
    horizons = 0:7,
    figure = TRUE
) {
  
  # Keep balanced panel
  reg <- reg[
    area_fips %in%
      reg[, .N, by = area_fips][
        N == length(unique(reg$year)),
        area_fips
      ]
  ]
  
  base <- outcome
  
  # Construct outcome used in LP
  reg[, y_lp := get(base)]
  
  # Lagged outcome controls
  reg[, l1_y := shift(y_lp, 1), by = area_fips]
  reg[, l2_y := shift(y_lp, 2), by = area_fips]
  reg[, l3_y := shift(y_lp, 3), by = area_fips]
  reg[, l4_y := shift(y_lp, 4), by = area_fips]
  
  # LP outcomes
  for (h in horizons) {
    
    var <- paste0("diff_", h)
    
    reg[, (var) :=
          shift(y_lp, type = "lead", n = h) -
          shift(y_lp, type = "lag", n = 1),
        by = area_fips]
  }
  
  # Store results
  results <- data.table(
    h = horizons,
    coef = NA_real_,
    se = NA_real_
  )
  
  # Run LPs
  for (hh in horizons) {
    
    var <- paste0("diff_", hh)
    
    mod <- feols(
      as.formula(
        paste0(
          var,
          " ~ l1_y + l2_y + l3_y + l4_y + industry_hhi_base | year | ",
          "w_IPW_US ~ w_IPW_OTH"
        )
      ),
      data = reg[year %in% start_year:end_year],
      cluster = ~area_fips + year
    )
    
    print(summary(mod))
    
    results[h == hh, `:=`(
      coef = coef(mod)["fit_w_IPW_US"],
      se   = se(mod)["fit_w_IPW_US"]
    )]
  }
  
  # Confidence intervals
  results[, `:=`(
    lower90 = coef - 1.64 * se,
    upper90 = coef + 1.64 * se,
    lower95 = coef - 1.96 * se,
    upper95 = coef + 1.96 * se
  )]
  
  # Plot
  if (figure) {
    
    p <- ggplot(results, aes(x = h, y = coef)) +
      geom_ribbon(
        aes(ymin = lower95, ymax = upper95),
        alpha = 0.2
      ) +
      geom_ribbon(
        aes(ymin = lower90, ymax = upper90),
        alpha = 0.5
      ) +
      geom_line() +
      geom_point() +
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      scale_x_continuous(
        breaks = horizons
      ) +
      theme_bw()
    
    ggsave(
      paste0(figs, "/baseline_lp_", base, ".pdf"),
      p,
      height = 4,
      width = 4
    )
  }
  
  return(results)
}

state_crosswalk <- data.table(
  state = c(
    1,2,4,5,6,8,9,10,12,13,15,16,17,18,19,20,21,22,23,24,
    25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,
    44,45,46,47,48,49,50,51,53,54,55,56
  ),
  state_str = state.abb,
  region = state.region,
  division = state.division
)


reg <- fread(paste0(path, "/output/lp_transformed_reg.csv"))
reg <- reg[year < 2017,]
reg[,test := log(resident_emp)]
reg[, total_jobs := total_goods_jobs + total_servc_jobs + total_trade_jobs]
# reg <- reg[year %in% c(2007:2017), ]
# begin regressions ------------------------------------------------------------
base_t0 <- "total_goods_jobs_share_resident_emp"

# w_outside_earn3333_jobs_share_resident_emp 
significant <- c(
  "w_outside_jobs_share_resident_emp",
  "w_outside_servc_jobs_share_resident_emp",
  "w_outside_goods_jobs_share_resident_emp",
  "w_outside_jobs_share_population",
  "w_total_goods_jobs_share_resident_emp",
  "w_manuf_share_emp",
  "w_manuf_emp_share_pop"
)

significant <- c("net_migration_share_population")

# keep constant sample 
reg <- reg[
  area_fips %in% reg[, .N, by = area_fips][N == length(unique(reg[,year])), area_fips]
]

# reg[, w_outside_jobs_share_resident_emp := log(w_outside_jobs_share_resident_emp)]
results <- run_lp(
  reg,
  "w_outside_jobs_share_resident_emp"
)

     
results_migration <- run_lp(
  reg = reg,
  "net_migration_share_population"
)


results_migration <- run_lp(
  reg = reg,
  "w_manuf_share_emp"
)

results_migration <- run_lp(
  reg = reg,
  "w_manuf_emp_share_pop"
)


reg[, mean_travel_time_to_work_minutes := log(mean_travel_time_to_work_minutes)]
results <- run_lp(
  reg = reg,
  "mean_travel_time_to_work_minutes"
)
