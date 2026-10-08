
# paths ------------------------------------------------------------------------
path <- "D:/writing_sample/data"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
tryCatch(
  expr = {
    setwd(path)
  },
  error = function(e) {
    # This code runs ONLY if an error occurs
    message("can't change wd to external drive, working locally \n", e$message)
    setwd(local)
  }
)
source(paste0(local, "/../code/writing_sample/00_load_workspace.R"))
figs <- paste0(getwd(), "/../", "figures")
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

make_base_year <- function(dt, var, base_year = 2000, unit = "area_fips") {
  newvar <- paste0(var, "_", base_year)
  
  dt_year <- dt[
    year == base_year,
    .(value = mean(get(var), na.rm = TRUE)),
    by = unit
  ]
  
  setnames(dt_year, "value", newvar)
  
  dt <- merge(dt, dt_year, 
              by = unit, 
              all.x = T)
  return(dt)
}

# regression functions
# run_lp runs regressions that are base diffs 
# output: single shock vars 
# can also take in no denominator vars for difference in shares as well. 

## run_lp_lagged_denom ---------------------------------------------------------
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
    instrument = "w_IPW_OTH", 
    table = TRUE, 
    table_horizons = c(0, 2, 4, 7),
    unit = "area_fips"
) {
  reg <- copy(reg)
  
  # Make sure shifts are chronological
  setorderv(reg, c(unit, "year"))
  
  # Keep balanced panel
  reg <- reg[get(unit) %in% reg[, .N, by=unit][N == uniqueN(reg$year), get(unit)]]
  
  # Shock lags
  reg[, `:=`(
    l1_own_shock = shift(get(shock), 1),
    l2_own_shock = shift(get(shock), 2),
    l1_own_iv = shift(get(instrument), 1),
    l2_own_iv = shift(get(instrument), 2)
  ), by=unit]
  
  # Log outcome
  # reg[, y_lp := log(get(outcome))]
  reg[, y_lp := get(outcome) ]
  
  reg[, y_control := get(outcome) / get(denominator) * 100]
  
  reg[, l1_y := shift(y_control, 1), by=unit]
  reg[, l2_y := shift(y_control, 2), by=unit]
  reg[, l3_y := shift(y_control, 3), by=unit]
  reg[, l4_y := shift(y_control, 4), by=unit]
  
  reg[, denom := shift(
    get(denominator),
    1
  ), by=unit]
  
  # LP outcomes
  for (h in horizons) {
    
    var <- paste0("diff_base_", h)
    dvar <- paste0("diff_", h)
    
    reg[, (var) :=
          shift(y_lp, type = "lead", n = h) -
          shift(y_lp, type = "lag", n = 1),
        by=unit]
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
          " ~ l1_y + l_sh_empl_mfg ", controls,
          " | year + ", unit, " | ",
          shock, " ~ ", instrument
        )
      ),
      data=reg_est,
      cluster=as.formula(paste0("~", unit, "+year")),
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
  
  if (table) {
    
    export_lp_single_table(
      results = results,
      outcome = paste0(outcome, "_share_", denominator),
      horizons = table_horizons,
      output_dir = figs,
      filename = paste0(
        "differenced_lp_",
        outcome, "_share_", denominator, "_",
        shock, "_", date
      )
    )
    
  }
  
  return(results)
}

# local projection function for multiple shocks within network 
## run_lp_multiple_shocks ------------------------------------------------------
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
    figure = TRUE,
    table = TRUE,
    table_horizons = c(0, 2, 4, 7),
    unit = "area_fips"
) {
  
  reg <- copy(reg)
  setorderv(reg, c(unit, "year"))
  
  # Shock lags
  reg[, `:=`(
    l1_own_shock_1 = shift(get(shock_1), 1),
    l2_own_shock_1 = shift(get(shock_1), 2),
    l1_own_shock_2 = shift(get(shock_2), 1),
    l2_own_shock_2 = shift(get(shock_2), 2),
    l1_own_iv_1 = shift(get(instrument_1), 1),
    l1_own_iv_2 = shift(get(instrument_2), 2)
  ), by=unit]
  
  reg <- reg[get(unit) %in% reg[, .N, by=unit][N == uniqueN(reg$year), get(unit)]]
  
  # Outcome and lags
  reg[, y_lp := get(outcome)]
  reg[, y_control := get(outcome) / get(denominator) * 100]
  reg[, l1_y := shift(y_control, 1), by=unit]
  reg[, l2_y := shift(y_control, 2), by=unit]
  reg[, denom := shift(get(denominator), 1), by=unit]
  
  # LP outcomes
  for (h in horizons) {
    reg[, (paste0("diff_", h)) :=
          (shift(y_lp, h, type="lead") - shift(y_lp, 1)) / denom * 100,
        by=unit]
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
        " ~ l1_y + l_sh_empl_mfg ", controls,
        " | year + ", unit, " | ",
        shock_1, " + ", shock_2,
        " ~ ",
        instrument_1, " + ", instrument_2
      )),
      data=reg_est,
      cluster=as.formula(paste0("~", unit, "+year")),
      weights=~baseline_emp
    )
    print(summary(mod, stage = 1))
    
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
    if (unit == "commuting_zone_id_2000") "Own-CZ exposure" else "Own-county exposure",
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
    ggsave(
      paste0(figs, "/differenced_multishock_lp_", outcome, "_share_", denominator, "_", shock_1, "_", date, ".pdf"),
      p,
      height=4,
      width=4
    )
  }
  
  
  if (table) {
    
    export_lp_table(
      results = results,
      outcome = paste0(outcome, "_share_", denominator),
      shock_1 = shock_1,
      shock_2 = shock_2,
      horizons = table_horizons,
      output_dir = figs,
      prefix = paste0(
        "differenced_multishock_lp_",
        outcome, "_share_", denominator, "_",
        shock_1, "_", date
      )
    )
    
  }
  
  return(results[])
}

## run_lp_ratio ----------------------------------------------------------------
run_lp_ratio <- function(
    reg, 
    outcome, 
    controls = "",
    start_year=2000, 
    end_year=2007, 
    horizons=0:7, 
    figure=TRUE,
    shock = "w_IPW_US",
    instrument = "w_IPW_OTH", 
    table = TRUE, 
    table_horizons = c(0, 2, 4, 7),
    unit = "area_fips"
) {
  reg <- copy(reg)
  reg <- reg[!is.na(get(outcome)) & year <= end_year + max(horizons)]
  reg <- reg[get(unit) %in% reg[, .N, by=unit][N == uniqueN(reg$year), get(unit)]]
  setorderv(reg, c(unit, "year"))
  
  reg[, y_lp := get(outcome)]
  reg[, l1_y := shift(y_lp), by=unit]
  reg[, l2_y := shift(y_lp, 2), by=unit]
  reg[, l_sh_empl_mfg := shift(sh_empl_mfg), by=unit]
  
  for (h in horizons) {
    var <- paste0("diff_", h)
    reg[, (var) := shift(y_lp, type="lead", n=h) - l1_y, by=unit]
  }
  
  reg_est <- reg[year %in% start_year:end_year]
  
  for (h in horizons) winsor(reg_est, paste0("diff_", h))
  results <- data.table(h=horizons, coef=NA_real_, se=NA_real_)
  
  for (hh in horizons) {
    var <- paste0("w_diff_", hh)
    
    mod <- feols(
      as.formula(paste0(
        var,
        " ~ l1_y + l_sh_empl_mfg", controls,
        " | ", unit, " + year | ",
        shock, " ~ ", instrument
      )),
      data=reg_est,
      cluster=as.formula(paste0("~", unit, "+year")),
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
  
  if (table) {
    
    export_lp_single_table(
      results = results,
      outcome = outcome,
      horizons = table_horizons,
      output_dir = figs,
      filename = paste0(
        "baseline_lp_",
        outcome, "_",
        shock, "_", date
      )
    )
    
  }
  
  return(results)
}

# local projection function for multiple shocks - ratio outcomes
## run_lp_ratio_multiple_shocks ------------------------------------------------
run_lp_ratio_multiple_shocks <- function(
    reg,
    outcome,
    shock_1="w_IPW_US",
    shock_2="w_ex_mean_dest_IPW_US",
    instrument_1="w_IPW_OTH",
    instrument_2="w_ex_mean_dest_IPW_OTH",
    start_year=2000,
    end_year=2007,
    horizons=0:7,
    controls="",
    figure=TRUE,
    table = TRUE,
    table_horizons = c(0, 2, 4, 7),
    unit = "area_fips"
) {
  reg <- copy(reg)
  reg <- reg[!is.na(get(outcome)) & year <= end_year + max(horizons)]
  reg <- reg[get(unit) %in% reg[, .N, by=unit][N == uniqueN(reg$year), get(unit)]]
  setorderv(reg, c(unit, "year"))
  
  # Outcome and lags
  reg[, y_lp := get(outcome)]
  reg[, l1_y := shift(y_lp), by=unit]
  reg[, l2_y := shift(y_lp,2), by=unit]
  reg[, l_sh_empl_mfg := shift(sh_empl_mfg), by=unit]
  
  reg[, `:=`(
    l1_own_shock_1 = shift(get(shock_1), 1),
    l2_own_shock_1 = shift(get(shock_1), 2),
    l1_own_shock_2 = shift(get(shock_2), 1),
    l2_own_shock_2 = shift(get(shock_2), 2),
    l1_own_iv_1 = shift(get(instrument_1), 1),
    l1_own_iv_2 = shift(get(instrument_2), 2)
  ), by=unit]
  
  # LP outcomes: Y[t+h] - Y[t-1]
  for(h in horizons) {
    reg[, (paste0("diff_",h)) := shift(y_lp,h,type="lead") - l1_y, by=unit]
  }
  
  # Restrict after constructing leads/lags
  reg_est <- reg[year %in% start_year:end_year]
  
  # Winsorize each horizon separately
  for(h in horizons) winsor(reg_est,paste0("diff_",h))
  
  # Long-form results
  results <- CJ(h=horizons,shock=c(shock_1,shock_2))
  results[, `:=`(coef=NA_real_,se=NA_real_)]
  
  # Run LPs
  for(hh in horizons) {
    
    var <- paste0("w_diff_",hh)
    
    mod <- feols(
      as.formula(paste0(
        var,
        " ~ l1_y + l_sh_empl_mfg ", controls,
        " | year + ", unit, " | ",
        shock_1, " + ", shock_2,
        " ~ ", instrument_1, " + ", instrument_2
      )),
      data=reg_est,
      cluster=as.formula(paste0("~", unit, "+year")),
      weights=~baseline_emp
    )
    
    for(s in c(shock_1,shock_2)) {
      fit_s <- paste0("fit_",s)
      
      results[h == hh & shock == s, `:=`(
        coef=coef(mod)[fit_s],
        se=se(mod)[fit_s]
      )]
    }
  }
  
  # Confidence intervals
  results[, `:=`(
    lower90=coef - 1.64*se,
    upper90=coef + 1.64*se,
    lower95=coef - 1.96*se,
    upper95=coef + 1.96*se
  )]
  
  # Plot labels
  results[, shock_label := fifelse(
    shock == shock_1,
    if (unit == "commuting_zone_id_2000") "Own-CZ exposure" else "Own-county exposure",
    "Commuting-network exposure"
  )]
  
  # Plot
  if(figure) {
    p <- ggplot(results,aes(x=h,y=coef,color=shock_label,fill=shock_label)) +
      geom_hline(yintercept=0,linetype="dashed") +
      geom_ribbon(aes(ymin=lower95,ymax=upper95),alpha=.10,color=NA) +
      geom_ribbon(aes(ymin=lower90,ymax=upper90),alpha=.18,color=NA) +
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
    ggsave(paste0(figs, "/baseline_multishock_lp_", outcome, "_", shock_1, "_", date, ".pdf"), p, height=4, width=4)
  }
  
  
  if (table) {
    
    export_lp_table(
      results = results,
      outcome = outcome,
      shock_1 = shock_1,
      shock_2 = shock_2,
      horizons = table_horizons,
      output_dir = figs,
      prefix = paste0(
        "baseline_multishock_lp_",
        outcome, "_", shock_1, "_", date
      )
    )
    
  }
  
  return(results[])
}

################################################################################
# Heterogeneous LP — two shocks / two instruments
################################################################################

run_lp_ratio_multiple_shocks_heterogeneity <- function(
    reg,
    outcome,
    group_var,
    shock_1 = "w_IPW_US",
    shock_2 = "w_ex_mean_dest_IPW_US",
    instrument_1 = "w_IPW_OTH",
    instrument_2 = "w_ex_mean_dest_IPW_OTH",
    start_year = 2000,
    end_year = 2015,
    horizons = 0:7,
    controls = "",
    figure = TRUE,
    unit = "area_fips"
) {
  
  reg <- copy(reg)
  
  reg <- reg[
    !is.na(get(outcome)) &
      !is.na(get(group_var)) &
      year <= end_year + max(horizons)
  ]
  
  reg <- reg[get(unit) %in% reg[, .N, by=unit][N == uniqueN(reg$year), get(unit)]]
  
  setorderv(reg, c(unit, "year"))
  
  # Outcome and lags
  reg[, y_lp := get(outcome)]
  reg[, l1_y := shift(y_lp), by=unit]
  reg[, l2_y := shift(y_lp, 2), by=unit]
  reg[, l_sh_empl_mfg := shift(sh_empl_mfg), by=unit]
  
  # Shock lags
  reg[, `:=`(
    l1_own_shock_1 = shift(get(shock_1), 1),
    l2_own_shock_1 = shift(get(shock_1), 2),
    l1_own_shock_2 = shift(get(shock_2), 1),
    l2_own_shock_2 = shift(get(shock_2), 2),
    l1_own_iv_1 = shift(get(instrument_1), 1),
    l1_own_iv_2 = shift(get(instrument_2), 2)
  ), by=unit]
  
  # LP outcomes
  for (h in horizons) {
    reg[, (paste0("diff_", h)) :=
          shift(y_lp, h, type = "lead") - l1_y,
        by=unit]
  }
  
  # Estimation period
  reg_est <- reg[year %in% start_year:end_year]
  
  # Winsorize separately within each heterogeneity group
  for (h in horizons) {
    
    var <- paste0("diff_", h)
    wvar <- paste0("w_", var)
    
    reg_est[, (wvar) := {
      q <- quantile(
        get(var),
        probs = c(.01, .99),
        na.rm = TRUE
      )
      pmin(pmax(get(var), q[1]), q[2])
    }, by = group_var]
  }
  
  # Results table
  groups <- unique(reg_est[[group_var]])
  
  results <- CJ(
    h = horizons,
    group = groups,
    shock = c(shock_1, shock_2)
  )
  
  results[, `:=`(
    coef = NA_real_,
    se = NA_real_,
    N = NA_integer_
  )]
  
  # LPs by group
  for (g in groups) {
    
    for (hh in horizons) {
      
      var <- paste0("w_diff_", hh)
      
      mod <- feols(
        as.formula(
          paste0(
            var,
            " ~ l1_y + l_sh_empl_mfg ", controls,
            " | year + ", unit, " | ",
            shock_1, " + ", shock_2,
            " ~ ",
            instrument_1, " + ", instrument_2
          )
        ),
        data = reg_est[get(group_var) == g],
        cluster=as.formula(paste0("~", unit, "+year")),
        weights = ~baseline_emp
      )
      
      for (s in c(shock_1, shock_2)) {
        
        fit_s <- paste0("fit_", s)
        
        results[
          h == hh & group == g & shock == s,
          `:=`(
            coef = coef(mod)[fit_s],
            se = se(mod)[fit_s],
            N = nobs(mod)
          )
        ]
      }
    }
  }
  
  # Confidence intervals
  results[, `:=`(
    lower90 = coef - 1.64 * se,
    upper90 = coef + 1.64 * se,
    lower95 = coef - 1.96 * se,
    upper95 = coef + 1.96 * se
  )]
  
  # Labels
  results[, shock_label :=
            fifelse(
              shock == shock_1,
              "Own-county exposure",
              "Commuting-network exposure"
            )]
  
  # Plot
  if (figure) {
    
    p <- ggplot(
      results,
      aes(
        x = h,
        y = coef,
        color = shock_label,
        fill = shock_label
      )
    ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      geom_ribbon(
        aes(
          ymin = lower95,
          ymax = upper95
        ),
        alpha = .10,
        color = NA
      ) +
      geom_ribbon(
        aes(
          ymin = lower90,
          ymax = upper90
        ),
        alpha = .18,
        color = NA
      ) +
      geom_line(linewidth = .8) +
      geom_point(size = 2) +
      scale_x_continuous(
        breaks = horizons
      ) +
      facet_wrap(
        ~group,
        scales = "free_y"
      ) +
      labs(
        x = "Horizon",
        y = "Coefficient",
        color = NULL,
        fill = NULL
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom"
      )
    
    print(p)
  }
  
  return(results[])
}
################################################################################
# Heterogeneous LP — lagged denominator, two shocks / two instruments
################################################################################

run_lp_multiple_shocks_heterogeneity <- function(
    reg,
    outcome,
    denominator,
    group_var,
    shock_1 = "w_IPW_US",
    shock_2 = "w_ex_mean_dest_IPW_US",
    instrument_1 = "w_IPW_OTH",
    instrument_2 = "w_ex_mean_dest_IPW_OTH",
    start_year = 2000,
    end_year = 2007,
    horizons = 0:7,
    controls = "",
    figure = TRUE,
    unit = "area_fips"
) {
  
  reg <- copy(reg)
  setorderv(reg, c(unit, "year"))
  
  # Shock lags
  reg[, `:=`(
    l1_own_shock_1 = shift(get(shock_1), 1),
    l2_own_shock_1 = shift(get(shock_1), 2),
    l1_own_shock_2 = shift(get(shock_2), 1),
    l2_own_shock_2 = shift(get(shock_2), 2),
    l1_own_iv_1 = shift(get(instrument_1), 1),
    l2_own_iv_1 = shift(get(instrument_1), 2),
    l1_own_iv_2 = shift(get(instrument_2), 1),
    l2_own_iv_2 = shift(get(instrument_2), 2)
  ), by=unit]
  
  # Balanced panel
  reg <- reg[get(unit) %in% reg[, .N, by=unit][N == uniqueN(reg$year), get(unit)]]
  
  # Outcome and lags
  reg[, y_lp := get(outcome)]
  reg[, y_control := get(outcome) / get(denominator) * 100]
  reg[, l1_y := shift(y_control, 1), by=unit]
  reg[, l2_y := shift(y_control, 2), by=unit]
  reg[, l_sh_empl_mfg := shift(sh_empl_mfg), by=unit]
  
  # Lagged denominator
  reg[, denom := shift(get(denominator), 1), by=unit]
  
  # LP outcomes
  for (h in horizons) {
    reg[, (paste0("diff_", h)) :=
          (shift(y_lp, h, type = "lead") - shift(y_lp, 1)) /
          denom * 100,
        by=unit]
  }
  
  # Estimation sample
  reg_est <- reg[
    year %in% start_year:end_year &
      !is.na(get(group_var))
  ]
  
  # Winsorize within heterogeneity group x horizon
  for (h in horizons) {
    
    var <- paste0("diff_", h)
    wvar <- paste0("w_", var)
    
    reg_est[, (wvar) := {
      q <- quantile(
        get(var),
        probs = c(.01, .99),
        na.rm = TRUE
      )
      pmin(pmax(get(var), q[1]), q[2])
    }, by = group_var]
  }
  
  # Results
  groups <- unique(reg_est[[group_var]])
  
  results <- CJ(
    h = horizons,
    group = groups,
    shock = c(shock_1, shock_2)
  )
  
  results[, `:=`(
    coef = NA_real_,
    se = NA_real_,
    N = NA_integer_
  )]
  
  # LP regressions
  for (g in groups) {
    
    for (hh in horizons) {
      
      var <- paste0("w_diff_", hh)
      
      mod <- feols(
        as.formula(
          paste0(
            var,
            " ~ l1_y + l_sh_empl_mfg ", controls,
            " | year + ", unit, " | ",
            shock_1, " + ", shock_2,
            " ~ ",
            instrument_1, " + ", instrument_2
          )
        ),
        data = reg_est[get(group_var) == g],
        cluster=as.formula(paste0("~", unit, "+year")),
        weights = ~baseline_emp
      )
      
      for (s in c(shock_1, shock_2)) {
        
        fit_s <- paste0("fit_", s)
        
        results[
          h == hh & group == g & shock == s,
          `:=`(
            coef = coef(mod)[fit_s],
            se = se(mod)[fit_s],
            N = nobs(mod)
          )
        ]
      }
    }
  }
  
  # Confidence intervals
  results[, `:=`(
    lower90 = coef - 1.64 * se,
    upper90 = coef + 1.64 * se,
    lower95 = coef - 1.96 * se,
    upper95 = coef + 1.96 * se
  )]
  
  results[, shock_label :=
            fifelse(
              shock == shock_1,
              "Own-county exposure",
              "Neighbor exposure"
            )]
  
  # Plot
  if (figure) {
    
    p <- ggplot(
      results,
      aes(
        x = h,
        y = coef,
        color = shock_label,
        fill = shock_label
      )
    ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      geom_ribbon(
        aes(ymin = lower95, ymax = upper95),
        alpha = .10,
        color = NA
      ) +
      geom_ribbon(
        aes(ymin = lower90, ymax = upper90),
        alpha = .18,
        color = NA
      ) +
      geom_line(linewidth = .8) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = horizons) +
      facet_wrap(
        ~group,
        scales = "free_y"
      ) +
      labs(
        x = "Horizon",
        y = "Coefficient",
        color = NULL,
        fill = NULL
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom"
      )
    
    print(p)
  }
  
  return(results[])
}



################################################################################
# Export two-shock LP results to LaTeX
################################################################################

export_lp_table <- function(
    results,
    outcome,
    shock_1,
    shock_2,
    horizons = c(0, 2, 4, 7),
    output_dir = figs,
    prefix = "lp",
    digits = 3
) {
  
  dt <- copy(as.data.table(results))
  dt <- dt[h %in% horizons]
  
  # Identify shocks
  dt[, shock_label := fifelse(
    shock == shock_1,
    "Own-county exposure",
    "Neighboring-county exposure"
  )]
  
  # Significance stars (normal approximation)
  dt[, stars := fifelse(
    abs(coef / se) >= qnorm(.9995), "***",
    fifelse(
      abs(coef / se) >= qnorm(.995), "**",
      fifelse(abs(coef / se) >= qnorm(.95), "*", "")
    )
  )]
  
  dt[, coefficient := paste0(
    sprintf(paste0("%.", digits, "f"), coef),
    stars
  )]
  
  dt[, std_error := paste0(
    "(",
    sprintf(paste0("%.", digits, "f"), se),
    ")"
  )]
  
  # Separate rows for coefficients and standard errors
  estimates <- dt[, .(
    shock_label,
    h,
    coefficient,
    std_error
  )]
  
  coef_wide <- dcast(
    estimates,
    shock_label ~ h,
    value.var = "coefficient"
  )
  
  se_wide <- dcast(
    estimates,
    shock_label ~ h,
    value.var = "std_error"
  )
  
  # Preserve requested shock ordering
  shock_order <- c(
    "Own-county exposure",
    "Neighboring-county exposure"
  )
  
  coef_wide[, order := match(shock_label, shock_order)]
  se_wide[, order := match(shock_label, shock_order)]
  
  setorder(coef_wide, order)
  setorder(se_wide, order)
  
  coef_wide[, order := NULL]
  se_wide[, order := NULL]
  
  # Build alternating coefficient / SE rows
  rows <- vector("list", 2 * nrow(coef_wide))
  
  for (i in seq_len(nrow(coef_wide))) {
    
    rows[[2 * i - 1]] <- coef_wide[i]
    rows[[2 * i]] <- se_wide[i]
    
    rows[[2 * i]][, shock_label := ""]
  }
  
  tab <- rbindlist(rows)
  
  setnames(
    tab,
    c("shock_label", as.character(horizons)),
    c("Exposure", paste0("Year ", horizons)),
    skip_absent = TRUE
  )
  
  # Export LaTeX
  
  filename <- paste0(prefix, ".tex")
  
  latex <- xtable(
    as.data.frame(tab),
    caption = paste(
      "Cumulative local projection estimates:",
      gsub("_", " ", outcome)
    ),
    label = paste0(
      "tab:",
      gsub("[^A-Za-z0-9]+", "_", prefix),
      "_",
      gsub("[^A-Za-z0-9]+", "_", outcome)
    ),
    align = c("l", "l", rep("c", ncol(tab) - 1))
  )
  
  print(
    latex,
    file = file.path(output_dir, filename),
    include.rownames = FALSE,
    sanitize.text.function = identity,
    floating = TRUE,
    booktabs = TRUE
  )
  
  invisible(tab[])
}


export_lp_single_table <- function(
    results,
    outcome,
    horizons = c(0, 2, 4, 7),
    output_dir = figs,
    filename,
    digits = 3
) {
  
  dt <- copy(as.data.table(results))
  dt <- dt[h %in% horizons]
  setorder(dt, h)
  
  stopifnot(all(horizons %in% dt$h))
  
  dt[, stars := fifelse(
    abs(coef / se) >= qnorm(.9995), "***",
    fifelse(
      abs(coef / se) >= qnorm(.995), "**",
      fifelse(abs(coef / se) >= qnorm(.95), "*", "")
    )
  )]
  
  fmt <- paste0("%.", digits, "f")
  
  estimates <- paste0(sprintf(fmt, dt$coef), dt$stars)
  errors <- paste0("(", sprintf(fmt, dt$se), ")")
  
  tab <- as.data.frame(
    rbind(
      c("Import exposure", estimates),
      c("", errors)
    ),
    stringsAsFactors = FALSE
  )
  
  names(tab) <- c(
    "Exposure",
    paste0("Year ", sort(horizons))
  )
  
  latex <- xtable(
    tab,
    caption = paste(
      "Cumulative local projection estimates:",
      gsub("_", " ", outcome)
    ),
    label = paste0(
      "tab:",
      gsub("[^A-Za-z0-9]+", "_", filename)
    ),
    align = c("l", "l", rep("c", length(horizons)))
  )
  
  print(
    latex,
    file = file.path(output_dir, paste0(filename, ".tex")),
    include.rownames = FALSE,
    sanitize.text.function = identity,
    floating = TRUE,
    booktabs = TRUE
  )
  
  invisible(tab)
}

