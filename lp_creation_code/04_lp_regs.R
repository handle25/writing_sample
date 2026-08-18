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

# reg <- reg[year %in% c(2007:2017), ]
# begin regressions ------------------------------------------------------------
base_t0 <- "total_goods_jobs_share_resident_emp"

# w_outside_earn3333_jobs_share_resident_emp 
significant <- c(
  "w_outside_jobs_share_resident_emp",
  "w_outside_servc_jobs_share_resident_emp",
  "w_outside_servc_jobs_share_service_jobs",
  "w_outside_jobs_share_population",
  "w_total_goods_jobs_share_resident_emp",
  "w_manuf_share_emp",
  "w_manuf_emp_share_pop"
)
# 
# significant <- c(
#   "w_manuf_share_emp",
#   "w_manuf_emp_share_pop"
# )

pop02 <- reg[year == 2005,] |> 
  fmutate(pop02 = log(population)) |> 
  fselect(area_fips, pop02)

reg <- merge(
  reg, pop02, 
  by = c("area_fips"), 
  all.x = TRUE
)

reg[, migration_share_pop := net_migration / population]

reg[, returns_3_outflow := log(as.integer(returns_3_outflow))]

reg[, l1_shock := shift(w_IPW_US, 1), by = area_fips]
reg[, l2_shock := shift(w_IPW_US, 2), by = area_fips]
reg[, l3_shock := shift(w_IPW_US, 3), by = area_fips]
reg[, l4_shock := shift(w_IPW_US, 4), by = area_fips]
reg[, d_pop  := (population - shift(population, 1))/shift(population, 1), by = area_fips]

for (i in significant){
  base <- i
  
  reg[, l1_y := shift(get(base), 1),
      by = area_fips]
  
  reg[, l2_y := shift(get(base), 2),
      by = area_fips]
  
  reg[, l3_y := shift(get(base), 3),
      by = area_fips]
  
  reg[, l4_y := shift(get(base), 4),
      by = area_fips]
  
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
  for (hh in 0:10) {
    
    var <- paste0("diff_", hh)
    # var <- paste0("w_diff_share_t0_", hh)
    
    mod <- feols(
      as.formula(
        paste0(
          var,
          " ~  l1_y  + l2_y + l3_y + l4_y |  year |",
          "w_IPW_US ~ w_IPW_OTH"
        )
      ),
      data = reg[year %in% 2002:2010],
      cluster = ~area_fips+year
    )
    print(summary(mod))
    
    results[h == hh, `:=`(
      coef = coef(mod)["fit_w_IPW_US"],
      se   = se(mod)["fit_w_IPW_US"]
    )]
  }
  
  results[, lower90 := coef - 1.64 * se]
  results[, upper90 := coef + 1.64 * se]
  
  results[, lower95 := coef - 1.96 * se]
  results[, upper95 := coef + 1.96 * se]
  
  if (figure_1){
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
    ggsave(paste0(figs, "/baseline_lp_", base, ".pdf"))
  }
}

exit 
################################################################################
# LP: Mean travel time to work
###############################################################################
base_commutes <- c("migration_share_pop", 
                   "mean_travel_time_to_work_minutes")

for (i in base_commutes){
  base_commute <- i 
  reg[, greater_45 := commute_30_34E + commute_45_59E + commute_60_89E + commute_90plusE]
  reg[, greater_45_share := greater_45 / workers_not_wfhE]
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
          "w_IPW_US ~ w_IPW_OTH"
        )
      ),
      data = reg,
      cluster = ~area_fips + year
    )
    
    print(summary(mod_commute))
    
    results_commute[h == hh, `:=`(
      coef = coef(mod_commute)["fit_w_IPW_US"],
      se   = se(mod_commute)["fit_w_IPW_US"]
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
  if (figure_2){
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
    ggsave(paste0(figs, "/baseline_lp_", base_commute, ".pdf"))
  }
}

################################################################################
# Heterogeneity: commute-time LP by baseline commute-time tercile
################################################################################
base_commutes <- c("migration_share_pop", 
                   "mean_travel_time_to_work_minutes")


# Baseline commute time: first non-missing value observed for each county
county_q <- reg[
  !is.na(mean_travel_time_to_work_minutes),
  .(
    baseline = first(mean_travel_time_to_work_minutes)
  ),
  by = area_fips
]

# Split counties into terciles
county_q[, quantile :=
           frank(
             baseline,
             ties.method = "average"
           ) / .N]

county_q[, quantile :=
           fifelse(
             quantile <= 1/3, 1,
             fifelse(quantile <= 2/3, 2, 3)
           )]

reg <- merge(
  reg,
  county_q[, .(area_fips, quantile)],
  by = "area_fips",
  all.x = TRUE
)


################################################################################
# Run LP separately by tercile
################################################################################
for (i in base_commutes){
  base_commute <- i 
  
  for (h in 0:7) {
    
    var <- paste0("diff_commute_time_", h)
    
    reg[, (var) :=
          shift(get(base_commute), type = "lead", n = h) -
          shift(get(base_commute), type = "lag", n = 1),
        by = area_fips]
  }
  
  reg[, l1_commute := shift(get(base_commute), 1),
      by = area_fips]
  
  reg[, l2_commute := shift(get(base_commute), 2),
      by = area_fips]
  
  reg[, l3_commute := shift(get(base_commute), 3),
      by = area_fips]
  
  reg[, l4_commute := shift(get(base_commute), 4),
      by = area_fips]
  
  results_commute_q <- data.table()
  
  for (q in 1:3) {
    
    temp_results <- data.table(
      h = 0:7,
      coef = NA_real_,
      se = NA_real_
    )
    
    for (hh in 0:10) {
      
      var <- paste0("diff_commute_time_", hh)
      
      q99 <- quantile(reg[,get(var)], probs = .99, na.rm = TRUE)
      q01 <- quantile(reg[,get(var)], probs = .01, na.rm = TRUE)
      
      reg[get(var) > q99, (var) := q99]
      reg[get(var) < q01, (var) := q01]
      
      mod_q <- feols(
        as.formula(
          paste0(
            var,
            " ~ l1_commute + l2_commute + l3_commute + l4_commute | year | ",
            "w_IPW_US ~ w_IPW_OTH"
          )
        ),
        data = reg[quantile == q],
        cluster = ~area_fips + year
      )
      
      temp_results[h == hh, `:=`(
        coef = coef(mod_q)["fit_w_IPW_US"],
        se   = se(mod_q)["fit_w_IPW_US"]
      )]
    }
    
    temp_results[, quantile := q]
    
    results_commute_q <- rbind(
      results_commute_q,
      temp_results
    )
  }
  
  
  ################################################################################
  # Confidence intervals
  ################################################################################
  
  results_commute_q[, `:=`(
    lower90 = coef - 1.64 * se,
    upper90 = coef + 1.64 * se,
    lower95 = coef - 1.96 * se,
    upper95 = coef + 1.96 * se
  )]
  
  
  ################################################################################
  # Plot each tercile
  ################################################################################
  if (figure_3){
    make_commute_plot <- function(q) {
      
      ggplot(
        results_commute_q[quantile == q],
        aes(x = h, y = coef)
      ) +
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
          title = paste0("Baseline Commute Time Tercile ", q),
          x = "Horizon",
          y = "Change in Mean Commute Time (Minutes)"
        ) +
        theme_bw()
    }
    
    p1 <- make_commute_plot(1)
    p2 <- make_commute_plot(2)
    p3 <- make_commute_plot(3)
    
    p1 + p2 + p3
    ggsave(paste0(figs, "/tercile_", base_commute, ".pdf"))
  }
}

################################################################################
# Heterogeneity: commute-time LP by Census region
################################################################################

for (i in base_commutes){
  base_commute <- i 
  
  for (h in 0:7) {
    
    var <- paste0("diff_commute_time_", h)
    
    reg[, (var) :=
          shift(get(base_commute), type = "lead", n = h) -
          shift(get(base_commute), type = "lag", n = 1),
        by = area_fips]
  }
  
  reg[, l1_commute := shift(get(base_commute), 1),
      by = area_fips]
  
  reg[, l2_commute := shift(get(base_commute), 2),
      by = area_fips]
  
  reg[, l3_commute := shift(get(base_commute), 3),
      by = area_fips]
  
  reg[, l4_commute := shift(get(base_commute), 4),
      by = area_fips]
  
  results_commute_region <- data.table()
  
  regions <- unique(na.omit(reg$region))
  
  for (r in regions) {
    
    temp_results <- data.table(
      h = 0:7,
      coef = NA_real_,
      se = NA_real_
    )
    
    for (hh in 0:7) {
      
      var <- paste0("diff_commute_time_", hh)
      
      mod_region <- feols(
        as.formula(
          paste0(
            var,
            " ~ l1_commute + l2_commute + l3_commute + l4_commute | year | ",
            "w_IPW_US ~ w_IPW_OTH"
          )
        ),
        data = reg[region == r],
        cluster = ~area_fips + year
      )
      
      temp_results[h == hh, `:=`(
        coef = coef(mod_region)["fit_w_IPW_US"],
        se   = se(mod_region)["fit_w_IPW_US"]
      )]
    }
    
    temp_results[, region := r]
    
    results_commute_region <- rbind(
      results_commute_region,
      temp_results
    )
  }
  
  
  ################################################################################
  # Confidence intervals
  ################################################################################
  
  results_commute_region[, `:=`(
    lower90 = coef - 1.64 * se,
    upper90 = coef + 1.64 * se,
    lower95 = coef - 1.96 * se,
    upper95 = coef + 1.96 * se
  )]
  
  
  ################################################################################
  # Plot each region
  ################################################################################
  if (figure_4){
    make_region_plot <- function(r) {
      
      ggplot(
        results_commute_region[region == r],
        aes(x = h, y = coef)
      ) +
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
          title = r,
          x = "Horizon",
          y = "Change in Outcome"
        ) +
        theme_bw()
      
    }
    
    
    ################################################################################
    # Stitch four regions together
    ################################################################################
    
    p_northeast <- make_region_plot("Northeast")
    p_midwest   <- make_region_plot("North Central")
    p_south     <- make_region_plot("South")
    p_west      <- make_region_plot("West")
    
    p_northeast + p_midwest + p_south + p_west +
      plot_layout(nrow = 1)
    
    ggsave(paste0(figs, "/regional_", base_commute, ".pdf"))
  }
}