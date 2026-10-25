
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

# Keep weights consistent in IV goodness-of-fit calculations. Some fixest versions
# recompute IV SSR without weights inside r2(), despite weighted denominators.
# Use second-stage (fitted endogenous regressor) residuals, as in fixest::r2.
lp_fit_stats <- function(mod) {
  residuals <- if (!is.null(mod$iv_stage) && mod$iv_stage == 2) {
    mod$iv_residuals
  } else {
    mod$residuals
  }
  weights <- if (is.null(mod$weights)) rep(1, length(residuals)) else mod$weights
  ssr <- sum(weights * residuals^2)
  within_r2 <- if (!is.null(mod$ssr_fe_only) && mod$ssr_fe_only > 0) {
    1 - ssr / mod$ssr_fe_only
  } else NA_real_
  adjusted_r2 <- if (mod$ssr_null > 0 && mod$nobs > mod$nparams) {
    1 - (ssr / mod$ssr_null) * (mod$nobs - 1) / (mod$nobs - mod$nparams)
  } else NA_real_
  c(r2 = within_r2, adj_r2 = adjusted_r2)
}


# Save two views of the SAME two-shock estimates. Filtering is for plotting only.
lp_secondary_shock_label <- function(shock) {
  if (grepl("empw_neighbor", shock, fixed = TRUE)) {
    "Employment-weighted geographic-neighbor exposure"
  } else if (grepl("mean_neighbor", shock, fixed = TRUE) &&
             !grepl("ex_|fn_", shock)) {
    "Mean geographic-neighbor exposure"
  } else {
    "Commuting-network exposure"
  }
}

save_lp_single_figure <- function(plot, filename, output_dir = figs,
                                 height = 4, width = 4) {
  single_dir <- file.path(output_dir, "single")
  dir.create(single_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(single_dir, filename), plot,
         height = height, width = width, bg = "white")
}

save_lp_shock_figures <- function(full_plot, results, shock_1, filename,
                                  output_dir = figs, height = 4, width = 7) {
  own_results <- results[results$shock == shock_1, ]
  if (!nrow(own_results)) stop("Own-shock plotting data is empty.")

  # Keep each shock's color identical in the main and appendix figures.
  shock_labels <- unique(as.character(results$shock_label))
  own_labels <- unique(as.character(own_results$shock_label))
  palette <- setNames(rep("#00BFC4", length(shock_labels)), shock_labels)
  palette[own_labels] <- "#F8766D"  # Orange/coral own-county response.
  full_plot <- full_plot +
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette) +
    guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2))

  # Train the main figure's axes on own-shock estimates and bands only.
  own_plot <- full_plot %+% own_results + theme(legend.position = "none")

  multi_dir <- file.path(output_dir, "multi")
  appendix_dir <- file.path(output_dir, "appendix", "multi")
  dir.create(multi_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(appendix_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(multi_dir, filename), own_plot,
         height = height, width = width, bg = "white")
  ggsave(file.path(appendix_dir, filename), full_plot,
         height = height, width = width, bg = "white")
  print(own_plot)
  invisible(list(own = own_plot, full = full_plot))
}

save_lp_heterogeneity_figures <- function(full_plot, results, shock_1, filename,
                                          output_dir = figs, height = 4, width = 8) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  # Additional display scaling only; returned estimates and table data stay unchanged.
  results <- copy(results)
  display_columns <- c("coef", "lower90", "upper90", "lower95", "upper95")
  results[, (display_columns) := lapply(.SD, function(x) x * 100),
          .SDcols = display_columns]
  full_plot <- full_plot %+% results + labs(y = "Coefficient x 100")
  own <- results[shock == shock_1]
  palette <- setNames(rep("#00BFC4", length(unique(results$shock_label))),
                      unique(results$shock_label))
  palette[unique(own$shock_label)] <- "#F8766D"
  full_plot <- full_plot + scale_color_manual(values = palette) +
    scale_fill_manual(values = palette) + facet_wrap(~group, nrow = 1, scales = "free_y")
  own_plot <- full_plot %+% own + theme(legend.position = "none")
  stem <- sub("\\.pdf$", "", filename)
  ggsave(file.path(output_dir, paste0(stem, "_both_shocks.pdf")), full_plot,
         height = height, width = width, bg = "white")
  ggsave(file.path(output_dir, paste0(stem, "_own_shock.pdf")), own_plot,
         height = height, width = width, bg = "white")
  print(full_plot)
  invisible(list(own = own_plot, full = full_plot))
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
    table_horizons = horizons,
    unit = "area_fips",
    table_output_dir = figs
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
    se = NA_real_, r2 = NA_real_, adj_r2 = NA_real_
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
      se=se(mod)[fit_shock],
      r2=lp_fit_stats(mod)[["r2"]],
        adj_r2=lp_fit_stats(mod)[["adj_r2"]]
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
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_ribbon(aes(ymin = lower95, ymax = upper95),
                  fill = "#F8766D", alpha = .10, color = NA) +
      geom_ribbon(aes(ymin = lower90, ymax = upper90),
                  fill = "#F8766D", alpha = .18, color = NA) +
      geom_line(color = "#F8766D", linewidth = .8) +
      geom_point(color = "#F8766D", size = 2) +
      scale_x_continuous(breaks = horizons) +
      labs(x = "Horizon", y = "Coefficient") +
      theme_minimal() +
      theme(legend.position = "none")
    
    save_lp_single_figure(
      p,
      filename = paste0("differenced_lp_", outcome, "_share_", denominator,
                        "_", shock, "_", date, ".pdf"),
      output_dir = figs, height = 4, width = 7
    )
    print(p)
  }
  
  if (table) {
    
    export_lp_single_table(
      results = results,
      outcome = paste0(outcome, "_share_", denominator),
      horizons = table_horizons,
      output_dir = table_output_dir,
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
    shock_2 = "w_empw_neighbor_IPW_US",
    instrument_1 = "w_IPW_OTH",
    instrument_2 = "w_empw_neighbor_IPW_OTH",
    start_year = 2000,
    end_year = 2007,
    horizons = 0:7,
    controls = "",
    figure = TRUE,
    table = TRUE,
    table_horizons = horizons,
    unit = "area_fips", 
    height = 4, 
    width = 4,
    table_output_dir = figs
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
  
  results[, `:=`(coef=NA_real_, se=NA_real_, r2=NA_real_, adj_r2=NA_real_)]
  
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
        se=se(mod)[fit_s],
        r2=lp_fit_stats(mod)[["r2"]],
        adj_r2=lp_fit_stats(mod)[["adj_r2"]]
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
    lp_secondary_shock_label(shock_2)
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
    
    save_lp_shock_figures(
      p, results, shock_1,
      filename = paste0("differenced_multishock_lp_", outcome, "_share_", denominator,
                        "_", shock_1, "_", date, ".pdf"),
      output_dir = figs, height = height, width = width
    )
  }
  
  
  if (table) {
    
    export_lp_table(
      results = results,
      outcome = paste0(outcome, "_share_", denominator),
      shock_1 = shock_1,
      shock_2 = shock_2,
      horizons = table_horizons,
      output_dir = table_output_dir,
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
    table_horizons = horizons,
    unit = "area_fips",
    table_output_dir = figs,
    outcome_scale = 1
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
    reg[, (var) := (shift(y_lp, type="lead", n=h) - l1_y) * outcome_scale, by=unit]
  }
  
  reg_est <- reg[year %in% start_year:end_year]
  
  for (h in horizons) winsor(reg_est, paste0("diff_", h))
  results <- data.table(h=horizons, coef=NA_real_, se=NA_real_, r2=NA_real_, adj_r2=NA_real_)
  
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
      se=se(mod)[fit_shock],
      r2=lp_fit_stats(mod)[["r2"]],
        adj_r2=lp_fit_stats(mod)[["adj_r2"]]
    )]
  }
  
  results[, `:=`(
    lower90=coef - 1.64*se,
    upper90=coef + 1.64*se,
    lower95=coef - 1.96*se,
    upper95=coef + 1.96*se
  )]
  
  if (figure) {
    p <- ggplot(results, aes(x = h, y = coef)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_ribbon(aes(ymin = lower95, ymax = upper95),
                  fill = "#F8766D", alpha = .10, color = NA) +
      geom_ribbon(aes(ymin = lower90, ymax = upper90),
                  fill = "#F8766D", alpha = .18, color = NA) +
      geom_line(color = "#F8766D", linewidth = .8) +
      geom_point(color = "#F8766D", size = 2) +
      scale_x_continuous(breaks = horizons) +
      labs(x = "Horizon", y = "Coefficient") +
      theme_minimal() +
      theme(legend.position = "none")
    
    save_lp_single_figure(
      p, filename = paste0("baseline_lp_", outcome, "_", shock, "_", date, ".pdf"),
      output_dir = figs, height = 4, width = 7
    )
    print(p)
  }
  
  if (table) {
    
    export_lp_single_table(
      results = results,
      outcome = outcome,
      horizons = table_horizons,
      output_dir = table_output_dir,
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
    shock_2="w_empw_neighbor_IPW_US",
    instrument_1="w_IPW_OTH",
    instrument_2="w_empw_neighbor_IPW_OTH",
    start_year=2000,
    end_year=2007,
    horizons=0:7,
    controls="",
    figure=TRUE,
    table = TRUE,
    table_horizons = horizons,
    unit = "area_fips", 
    height = 4, 
    width = 6,
    table_output_dir = figs
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
    reg[, (paste0("diff_",h)) := (shift(y_lp,h,type="lead") - l1_y)*100, by=unit]
  }
  
  # Restrict after constructing leads/lags
  reg_est <- reg[year %in% start_year:end_year]
  
  # Winsorize each horizon separately
  for(h in horizons) winsor(reg_est,paste0("diff_",h))
  
  # Long-form results
  results <- CJ(h=horizons,shock=c(shock_1,shock_2))
  results[, `:=`(coef=NA_real_,se=NA_real_, r2=NA_real_, adj_r2=NA_real_)]
  
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
        se=se(mod)[fit_s],
        r2=lp_fit_stats(mod)[["r2"]],
        adj_r2=lp_fit_stats(mod)[["adj_r2"]]
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
    lp_secondary_shock_label(shock_2)
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
    
    save_lp_shock_figures(
      p, results, shock_1,
      filename = paste0("baseline_multishock_lp_", outcome, "_", shock_1, "_", date, ".pdf"),
      output_dir = figs, height = height, width = width
    )
  }
  
  
  if (table) {
    
    export_lp_table(
      results = results,
      outcome = outcome,
      shock_1 = shock_1,
      shock_2 = shock_2,
      horizons = table_horizons,
      output_dir = table_output_dir,
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
    shock_2 = "w_empw_neighbor_IPW_US",
    instrument_1 = "w_IPW_OTH",
    instrument_2 = "w_empw_neighbor_IPW_OTH",
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
    se = NA_real_, r2 = NA_real_, adj_r2 = NA_real_,
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
            N = nobs(mod),
            r2 = lp_fit_stats(mod)[["r2"]],
            adj_r2 = lp_fit_stats(mod)[["adj_r2"]]
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
              lp_secondary_shock_label(shock_2)
            )]
  
  # Plot
  if (figure) {
    
    p <- ggplot(
      results,
      aes(x = h, y = coef, color = shock_label, fill = shock_label)
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
    
    save_lp_heterogeneity_figures(
      p, results, shock_1,
      filename = paste0(sub("_group$", "", group_var), "_ratio_", outcome, "_", date, ".pdf"),
      output_dir = figs, height = 4, width = 8
    )
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
    shock_2 = "w_empw_neighbor_IPW_US",
    instrument_1 = "w_IPW_OTH",
    instrument_2 = "w_empw_neighbor_IPW_OTH",
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
    se = NA_real_, r2 = NA_real_, adj_r2 = NA_real_,
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
            N = nobs(mod),
            r2 = lp_fit_stats(mod)[["r2"]],
            adj_r2 = lp_fit_stats(mod)[["adj_r2"]]
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
              lp_secondary_shock_label(shock_2)
            )]
  
  # Plot
  if (figure) {
    
    p <- ggplot(
      results,
      aes(x = h, y = coef, color = shock_label, fill = shock_label)
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
    
    save_lp_heterogeneity_figures(
      p, results, shock_1,
      filename = paste0(sub("_group$", "", group_var), "_count_", outcome, "_", date, ".pdf"),
      output_dir = figs, height = 4, width = 8
    )
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
    abs(coef / se) >= qnorm(.995), "***",
    fifelse(
      abs(coef / se) >= qnorm(.975), "**",
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
  
  # Each model has one fit statistic per horizon, shared by both shock rows.
  for (stat in c("r2", "adj_r2")) {
    stat_label <- if (stat == "r2") "Within R-squared" else "Adjusted R-squared"
    stat_row <- dt[shock == shock_1, .(
      shock_label = stat_label,
      h,
      coefficient = sprintf(paste0("%.", digits, "f"), get(stat))
    )]
    stat_wide <- dcast(stat_row, shock_label ~ h, value.var = "coefficient")
    setcolorder(stat_wide, names(tab))
    tab <- rbind(tab, stat_wide, fill = TRUE)
  }

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
    abs(coef / se) >= qnorm(.995), "***",
    fifelse(
      abs(coef / se) >= qnorm(.975), "**",
      fifelse(abs(coef / se) >= qnorm(.95), "*", "")
    )
  )]
  
  fmt <- paste0("%.", digits, "f")
  
  estimates <- paste0(sprintf(fmt, dt$coef), dt$stars)
  errors <- paste0("(", sprintf(fmt, dt$se), ")")
  
  tab <- as.data.frame(
    rbind(
      c("Import exposure", estimates),
      c("", errors),
      c("Within R-squared", sprintf(fmt, dt$r2)),
      c("Adjusted R-squared", sprintf(fmt, dt$adj_r2))
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

