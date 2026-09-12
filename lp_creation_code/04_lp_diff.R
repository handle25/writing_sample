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
date <- Sys.Date()

# qcewdata 
path <- "D:/writing_sample/data"
figs <- "D:/writing_sample/figures"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
setwd(path)

# read in data -----------------------------------------------------------------
reg <- fread(paste0(path, "/output/lp_transformed_reg.csv")) 

setorder(reg, area_fips, year)

reg[, l1_net_migration_share_population :=
      shift(net_migration / population, 1),
    by = area_fips]

reg[, l2_net_migration_share_population :=
      shift(net_migration / population, 2),
    by = area_fips]

# function definition ----------------------------------------------------------
winsor <- function(dt, var, p = 0.01) {
  
  q <- quantile(
    dt[[var]],
    probs = c(p, 1 - p),
    na.rm = TRUE
  )
  
  w_var <- paste0("w_", var)
  
  dt[, (w_var) := pmin(
    pmax(get(var), q[1]),
    q[2]
  )]
}

run_lp <- function(
    reg,
    outcome,
    start_year = 2000,
    end_year = 2007,
    horizons = 0:7,
    figure = TRUE, 
    denominator = "population_2000", 
    controls = ""
) {
  
  reg <- copy(reg)
  
  # Make sure shifts are chronological
  setorder(reg, area_fips, year)
  
  # Keep balanced panel
  reg <- reg[
    area_fips %in%
      reg[, .N, by = area_fips][
        N == length(unique(reg$year)),
        area_fips
      ]
  ]
  
  # Log outcome
  # reg[, y_lp := log(get(outcome))]
  reg[, y_lp := get(outcome) ]
  
  reg[, y_control := get(outcome) / get(denominator) * 100]
  
  reg[, l1_y := shift(y_control, 1), by = area_fips]
  reg[, l2_y := shift(y_control, 2), by = area_fips]
  reg[, l3_y := shift(y_control, 3), by = area_fips]
  reg[, l4_y := shift(y_control, 4), by = area_fips]
  
  reg[, denom := shift(
    get(denominator),
    1
  ), by = area_fips]
  
  # LP outcomes
  for (h in horizons) {
    
    var <- paste0("diff_base_", h)
    dvar <- paste0("diff_", h)
    
    reg[, (var) :=
          shift(y_lp, type = "lead", n = h) -
          shift(y_lp, type = "lag", n = 1),
        by = area_fips]
    reg[, (dvar) := get(var) / denom * 100 ]
  }
  
  # Restrict to estimation period AFTER constructing leads/lags
  reg_est <- reg[year %in% start_year:end_year]
  
  # Winsorize LP outcome separately at each horizon
  for (h in horizons) {
    
    dvar <- paste0("diff_", h)
    
    winsor(reg_est, dvar)
  }
  
  # Store results
  results <- data.table(
    h = horizons,
    coef = NA_real_,
    se = NA_real_
  )
  
  # Run LPs
  for (hh in horizons) {
    
    var <- paste0("w_diff_", hh)
    
    mod <- feols(
      as.formula(
        paste0(
          var,
          " ~ l1_y + l2_y + l_sh_empl_mfg", controls, 
          "| year +area_fips  | ",
          "w_IPW_US ~ w_IPW_OTH"
        )
      ),
      data = reg_est,
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
      paste0(figs, "/differenced_lp_", outcome, "_share_", denominator, date, ".pdf"),
      p,
      height = 4,
      width = 4
    ) 
    print(p)
  }
  return(results)
}

# run regs ---------------------------------------------------------------------
reg[, l_sh_empl_mfg := shift(sh_empl_mfg), by = area_fips]
run_lp(reg, outcome = "outside_jobs", denominator = "population")
run_lp(reg, outcome = "outside_jobs", denominator = "labor_force")
run_lp(reg, outcome = "net_migration", denominator = "population")
run_lp(reg, outcome = "net_migration", denominator = "population_2000")
run_lp(reg, outcome = "labor_force", denominator = "population", controls = "+l1_net_migration_share_population +l2_net_migration_share_population")
run_lp(reg, outcome = "unemployed", denominator = "population", controls = "")
run_lp(reg, outcome = "unemployed", denominator = "labor_force")
run_lp(reg, outcome = "outside_jobs", denominator = "population_2000", controls = "+l1_net_migration_share_population")

