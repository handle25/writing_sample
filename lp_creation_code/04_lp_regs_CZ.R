################################################################################
# Created 8.21.2026 
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
date = Sys.Date()
# function definition ----------------------------------------------------------
run_lp <- function(
    reg,
    outcome,
    start_year = 2000,
    end_year = 2007,
    horizons = 0:7,
    figure = TRUE
) {
  
  # Keep observations needed for outcome + future horizons
  reg <- reg[!is.na(get(outcome)) & year <= end_year + max(horizons)]
  
  # Keep balanced panel
  reg <- reg[
    commuting_zone_id_2000 %in%
      reg[, .N, by = commuting_zone_id_2000][
        N == length(unique(reg$year)),
        commuting_zone_id_2000
      ]
  ]
  
  setorder(reg, commuting_zone_id_2000, year)
  
  base <- outcome
  reg[, y_lp := get(base)]
  
  # Lagged outcome controls
  reg[, l1_y := shift(y_lp, 1), by = commuting_zone_id_2000]
  reg[, l2_y := shift(y_lp, 2), by = commuting_zone_id_2000]
  
  # Lagged exposure
  reg[, l1_US := shift(w_IPW_US, 1), by = commuting_zone_id_2000]
  reg[, l1_OTH := shift(w_IPW_OTH, 1), by = commuting_zone_id_2000]
  
  # LP outcomes
  for (h in horizons) {
    var <- paste0("diff_", h)
    reg[, (var) := shift(y_lp, h, type = "lead") - shift(y_lp, 1, type = "lag"),
        by = commuting_zone_id_2000]
  }
  
  results <- data.table(h = horizons, coef = NA_real_, se = NA_real_)
  
  for (hh in horizons) {
    
    var <- paste0("diff_", hh)
    
    mod <- feols(
      as.formula(paste0(
        var,
        " ~ l1_y + l2_y + l1_US + l_shind_manuf | ",
        "commuting_zone_id_2000 + year | ",
        "w_IPW_US ~ w_IPW_OTH"
      )),
      data = reg[year %in% start_year:end_year],
      cluster = ~commuting_zone_id_2000,
      weights = ~baseline_emp
    )
    
    print(summary(mod))
    
    results[h == hh, `:=`(
      coef = coef(mod)["fit_w_IPW_US"],
      se = se(mod)["fit_w_IPW_US"]
    )]
  }
  
  results[, `:=`(
    lower90 = coef - 1.64 * se,
    upper90 = coef + 1.64 * se,
    lower95 = coef - 1.96 * se,
    upper95 = coef + 1.96 * se
  )]
  
  if (figure) {
    p <- ggplot(results, aes(x = h, y = coef)) +
      geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2) +
      geom_ribbon(aes(ymin = lower90, ymax = upper90), alpha = 0.5) +
      geom_line() +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_x_continuous(breaks = horizons) +
      theme_bw()
    
    ggsave(
      paste0(figs, "/baseline_lp_CZ_", base, date, ".pdf"),
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


reg <- fread(paste0(path, "/output/lp_transformed_reg_CZ.csv"))
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
# reg <- reg[
#   commuting_zone_id_2000 %in% 
#     reg[, .N, by = commuting_zone_id_2000][N == length(unique(reg[,year])), commuting_zone_id_2000]
# ]

results <- run_lp(
  reg = reg,
  "industry_hhi"
)


# baseline LPs -----------------------------------------------------------------

run_lp(reg, "w_outside_jobs_share_population_2000", start_year = 2002, end_year = 2007)

run_lp(reg, "w_outside_jobs_share_labor_force", start_year = 2002, end_year = 2007)
run_lp(reg, "w_outside_jobs_share_resident_emp", start_year = 2002, end_year = 2007)

run_lp(reg, "w_net_migration_share_population", start_year = 2000, end_year = 2007)

run_lp(reg, "w_net_migration_outofstate_share_population", start_year = 2000, end_year = 2007)

run_lp(reg, "w_net_migration_share_population", start_year = 2007, end_year = 2013)




results <- run_lp(
  reg = reg,
  "industry_hhi_nomanufac"
)



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

cz_geo <- readRDS(
  paste0(path, "/output/cz_map.rds")
)


plot_reg <- copy(reg)

q <- quantile(reg[,d_sh_empl_mfg], probs = c(.05, .95), na.rm = T)
reg[,w_d_sh_empl_mfg := d_sh_empl_mfg]
reg[w_d_sh_empl_mfg < q[1], w_d_sh_empl_mfg := q[1]]
reg[w_d_sh_empl_mfg > q[2], w_d_sh_empl_mfg := q[2]]


for (y in c(2000, 2007, 2013)){
  map_dt <- merge(
    cz_geo,
    plot_reg,
    by = "commuting_zone_id_2000",
    all.x = TRUE
  ) |> 
    filter(year == y )
  
  plot <- ggplot(map_dt) +
    geom_sf(
      aes(fill = w_d_sh_empl_mfg),
      color = "grey70",
      linewidth = 0.1
    ) +
    coord_sf(
      xlim = c(-90, -80),
      ylim = c(34, 50)
    ) +
    scale_fill_gradient2(
      low = "red",
      high = "blue",
      midpoint = 0
    ) +
    theme_void() 
  
  plot_full <- ggplot(map_dt) +
    geom_sf(
      aes(fill = w_d_sh_empl_mfg),
      color = "grey70",
      linewidth = 0.1
    ) +
    coord_sf(
      xlim = c(-120, -60),
      ylim = c(24, 50)
    ) +
    scale_fill_gradient2(
      low = "red",
      high = "blue",
      midpoint = 0
    ) +
    theme_void() 
  
  assign(paste0("plot", y), plot)
  assign(paste0("plot_full", y), plot_full)
}

plot2007
ggsave(paste0(path,"/../figures/w_d_sh_empl_mfg_2006_2007_CZ",".pdf"))