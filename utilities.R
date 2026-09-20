# Functions --------------------------------------------------------------------

winsor <- function(dt, var, p = 0.01) {
  q <- quantile(dt[[var]], probs = c(p, 1 - p), na.rm = TRUE)
  
  w_var <- paste0("w_", var)
  
  dt[, (w_var) := pmin(pmax(get(var), q[1]), q[2])]
}


make_share <- function(dt, num, denom) {
  
  share_var <- paste0(num, "_share_", denom)
  
  dt[, (share_var) := get(num) / get(denom)]
  
  winsor(dt, share_var)
}

# create share vars, in line with ADH 
share_denom_all <- function(dt, var) {
  make_share(dt, var, "resident_emp")
  make_share(dt, var, "workplace_emp")
  make_share(dt, var, "outside_jobs")
  make_share(dt, var, "population")
}

make_base_year <- function(dt, var, base_year = 2000) {
  newvar <- paste0(var, "_", base_year)
  
  dt_year <- dt[
    year == base_year,
    .(value = mean(get(var), na.rm = TRUE)),
    by = area_fips
  ]
  
  setnames(dt_year, "value", newvar)
  
  dt <- merge(dt, dt_year, 
              by = "area_fips", 
              all.x = T)
  return(dt)
}

# regression functions
# run_lp runs regressions that are base diffs 
# output: single shock vars 
# can also take in no denominator vars for difference in shares as well. 
run_lp_lagged_denom <- function(
    reg,
    outcome,
    start_year = 2000,
    end_year = 2007,
    horizons = 0:7,
    figure = TRUE, 
    denominator, 
    controls = "",
    shock = "w_IPW_US",
    instrument = "w_IPW_OTH"
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
  
  # Shock lags
  reg[, `:=`(
    l1_own_shock = shift(get(shock), 1),
    l2_own_shock = shift(get(shock), 2),
    l1_own_iv = shift(get(instrument), 1),
    l2_own_iv = shift(get(instrument), 2)
  ), by=area_fips]
  
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
          " ~ l1_y + l2_y + l_sh_empl_mfg ", controls,
          " | year + area_fips | ",
          shock, " ~ ", instrument
        )
      ),
      data=reg_est,
      cluster=~area_fips + year,
      weight=~baseline_emp
    )
    
    print(summary(mod))
    
    fit_shock <- paste0("fit_", shock)
    
    results[h == hh, `:=`(
      coef=coef(mod)[fit_shock],
      se=se(mod)[fit_shock]
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
      paste0(figs, "/differenced_lp_", outcome, "_share_", denominator, "_", shock, "_", date, ".pdf"),
      p,
      height=4,
      width=4
    )
    print(p)
  }
  return(results)
}

# local projection function for multiple shocks within network 
run_lp_multiple_shocks <- function(
    reg,
    outcome,
    denominator,
    shock_1 = "w_IPW_US",
    shock_2 = "w_ex_mean_dest_IPW_US",
    instrument_1 = "w_IPW_OTH",
    instrument_2 = "w_ex_mean_dest_IPW_OTH",
    start_year = 2000,
    end_year = 2007,
    horizons = 0:7,
    controls = "",
    figure = TRUE
) {
  
  reg <- copy(reg)
  setorder(reg, area_fips, year)
  
  # Shock lags
  reg[, `:=`(
    l1_own_shock = shift(get(shock_1), 1),
    l2_own_shock = shift(get(shock_1), 2),
    l1_network_shock = shift(get(shock_2), 1),
    l2_network_shock = shift(get(shock_2), 2),
    l1_own_iv = shift(get(instrument_1), 1),
    l2_own_iv = shift(get(instrument_1), 2),
    l1_network_iv = shift(get(instrument_2), 1),
    l2_network_iv = shift(get(instrument_2), 2)
  ), by=area_fips]
  
  reg <- reg[
    area_fips %in%
      reg[, .N, by=area_fips][N == length(unique(reg$year)), area_fips]
  ]
  
  # Outcome and lags
  reg[, y_lp := get(outcome)]
  reg[, y_control := get(outcome) / get(denominator) * 100]
  reg[, l1_y := shift(y_control, 1), by=area_fips]
  reg[, l2_y := shift(y_control, 2), by=area_fips]
  reg[, denom := shift(get(denominator), 1), by=area_fips]
  
  # LP outcomes
  for (h in horizons) {
    reg[, (paste0("diff_", h)) :=
          (shift(y_lp, h, type="lead") - shift(y_lp, 1)) / denom * 100,
        by=area_fips]
  }
  
  reg_est <- reg[year %in% start_year:end_year]
  
  for (h in horizons) winsor(reg_est, paste0("diff_", h))
  
  # Long-form results: one row per horizon x shock
  results <- CJ(
    h = horizons,
    shock = c(shock_1, shock_2)
  )
  
  results[, `:=`(coef=NA_real_, se=NA_real_)]
  
  for (hh in horizons) {
    
    var <- paste0("w_diff_", hh)
    
    mod <- feols(
      as.formula(paste0(
        var,
        " ~ l1_y + l2_y + l_sh_empl_mfg + l1_own_shock", controls,
        " | year + area_fips | ",
        shock_1, " + ", shock_2,
        " ~ ",
        instrument_1, " + ", instrument_2
      )),
      data=reg_est,
      cluster=~area_fips + year,
      weights=~baseline_emp
    )
    
    for (s in c(shock_1, shock_2)) {
      fit_s <- paste0("fit_", s)
      results[h == hh & shock == s, `:=`(
        coef=coef(mod)[fit_s],
        se=se(mod)[fit_s]
      )]
    }
  }
  
  results[, `:=`(
    lower90=coef - 1.64*se,
    upper90=coef + 1.64*se,
    lower95=coef - 1.96*se,
    upper95=coef + 1.96*se
  )]
  
  # Cleaner names for plot
  results[, shock_label := fifelse(
    shock == shock_1,
    "Own-county exposure",
    "Commuting-network exposure"
  )]
  
  if (figure) {
    p <- ggplot(results, aes(x=h, y=coef, color=shock_label, fill=shock_label)) +
      geom_hline(yintercept=0, linetype="dashed") +
      geom_ribbon(aes(ymin=lower95, ymax=upper95), alpha=.10, color=NA) +
      geom_ribbon(aes(ymin=lower90, ymax=upper90), alpha=.18, color=NA) +
      geom_line(linewidth=.8) +
      geom_point(size=2) +
      scale_x_continuous(breaks=horizons) +
      labs(
        x="Horizon",
        y="Coefficient",
        color=NULL,
        fill=NULL
      ) +
      theme_minimal() +
      theme(legend.position="bottom")
    
    print(p)
  }
  
  return(results[])
}
