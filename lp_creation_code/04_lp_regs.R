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

date <- Sys.Date()
path <- "D:/writing_sample/data"
figs <- "D:/writing_sample/figures"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"

reg <- fread(paste0(path, "/output/lp_transformed_reg.csv"))
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
setwd(path)
# function definition ----------------------------------------------------------
# function definition ----------------------------------------------------------
run_lp <- function(
    reg, 
    outcome, 
    controls = "",
    start_year=2000, 
    end_year=2007, 
    horizons=0:7, 
    figure=TRUE,
    shock = "w_IPW_US",
    instrument = "w_IPW_OTH"
) {
  reg <- copy(reg)
  reg <- reg[!is.na(get(outcome)) & year <= end_year + max(horizons)]
  reg <- reg[area_fips %in% reg[, .N, by=area_fips][N == length(unique(reg$year)), area_fips]]
  setorder(reg, area_fips, year)
  
  reg[, y_lp := get(outcome)]
  reg[, l1_y := shift(y_lp), by=area_fips]
  reg[, l2_y := shift(y_lp, 2), by=area_fips]
  reg[, l_sh_empl_mfg := shift(sh_empl_mfg), by=area_fips]
  
  for (h in horizons) {
    var <- paste0("diff_", h)
    reg[, (var) := shift(y_lp, type="lead", n=h) - l1_y]
  }
  
  reg_est <- reg[year %in% start_year:end_year]
  
  for (h in horizons) winsor(reg_est, paste0("diff_", h))
  results <- data.table(h=horizons, coef=NA_real_, se=NA_real_)
  
  for (hh in horizons) {
    var <- paste0("w_diff_", hh)
    
    mod <- feols(
      as.formula(paste0(
        var,
        " ~ l1_y + l2_y + l_sh_empl_mfg", controls,
        " | area_fips + year | ",
        shock, " ~ ", instrument
      )),
      data=reg_est,
      cluster=~area_fips,
      weights=~baseline_emp
    )
    
    fit_shock <- paste0("fit_", shock)
    
    results[h == hh, `:=`(
      coef=coef(mod)[fit_shock],
      se=se(mod)[fit_shock]
    )]
  }
  
  results[, `:=`(
    lower90=coef - 1.64*se,
    upper90=coef + 1.64*se,
    lower95=coef - 1.96*se,
    upper95=coef + 1.96*se
  )]
  
  if (figure) {
    p <- ggplot(results, aes(h, coef)) +
      geom_ribbon(aes(ymin=lower95, ymax=upper95), alpha=.2) +
      geom_ribbon(aes(ymin=lower90, ymax=upper90), alpha=.5) +
      geom_line() + geom_point() +
      geom_hline(yintercept=0, linetype="dashed") +
      scale_x_continuous(breaks=horizons) +
      theme_bw()
    
    ggsave(paste0(figs, "/baseline_lp_", outcome, "_", shock, "_", date, ".pdf"), p, height=4, width=4)
    print(p)
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
# destination-condition LPs ----------------------------------------------------
reg[, outside_jobs_share_returns := outside_jobs / returns]
winsor(reg, "outside_jobs_share_returns")

run_lp(reg, "ew_share_into_less_unemp", start_year=2000, end_year=2007)
run_lp(reg, "ew_share_into_less_unemp", start_year=2000, end_year=2007, 
       controls = "+fn_mean_dest_IPW_US")
run_lp(reg, "ew_share_into_less_exposed", start_year=2000, end_year=2007,
       controls = "+fn_mean_dest_IPW_US")

# main outcomes ---------------------------------------------------------------
reg[, outside_jobs_share_returns := outside_jobs / returns ]
# Labor market
run_lp(reg, "labor_force_share_population", start_year=2000, end_year=2007,
       controls = "+ex_mean_dest_IPW_US")
run_lp(reg, "unemployed_share_labor_force", start_year=2000, end_year=2007)

# Commuting
run_lp(reg, "outside_jobs_share_population", start_year=2000, end_year=2007)
run_lp(reg, "outside_jobs_share_labor_force", start_year=2000, end_year=2007)
run_lp(reg, "w_outside_jobs_share_returns", start_year=2000, end_year=2007)

# Migration
run_lp(reg, "exemptions_net_migration_share_population", start_year=2000, end_year=2007)
run_lp(reg, "exemptions_3_outflow_share_exemptions_total_migration", start_year=2000, end_year=2007)

# Destination of migration
run_lp(reg, "ew_share_into_less_unemp", start_year=2000, end_year=2007)
run_lp(reg, "ew_share_into_less_exposed", start_year=2000, end_year=2007)

# Income
run_lp(reg, "ln_agi_per_return", start_year=2000, end_year=2007)
