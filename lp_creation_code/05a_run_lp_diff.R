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
network_vars <- grep("^(ex_|fn_)", names(reg), value=TRUE)

# for (v in network_vars) {
#   reg[year == 2001, (v) := reg[year == 2000, get(v)[match(area_fips, reg[year == 2001, area_fips])]]]
# }
reg <- reg[year<=2019]
balanced <- unique(reg[year %in% 1998:2019,.N, by = area_fips][N==22]$area_fips)
reg <- reg[area_fips %in% balanced,]
setorder(reg, area_fips, year)
reg[, baseline_resident_emp := resident_emp[year == 2000][1], by=area_fips]
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
    denominator = NULL, 
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

run_lp_multi <- function(
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

# run regs ---------------------------------------------------------------------
reg[, l_sh_empl_mfg := shift(sh_empl_mfg), by = area_fips]
reg[, total_migration := returns_3_inflow + returns_3_outflow]
reg[, net_migration := returns_3_inflow - returns_3_outflow]
reg[, net_migration_share_population := net_migration / total_migration]
reg[, l2_ex_mean_dest_IPW_US := shift(ex_mean_dest_IPW_US, n = 2), by = area_fips]
reg[, l1_w_ex_mean_dest_IPW_US := shift(w_ex_mean_dest_IPW_US, n = 1), by = area_fips]


shocks <- grep("IPW", names(reg), value=TRUE)
for (v in shocks) reg[, (paste0("l1_", v)) := shift(get(v), 1), by=area_fips]
for (v in shocks) winsor(reg, v, p = .02)


# run regressions --------------------------------------------------------------

# Labor market
run_lp(reg, outcome="labor_force", denominator="population")
run_lp(reg, outcome="labor_force", denominator="population", 
       shock = "w_IPW_US", 
       instrument = "w_fn_mean_dest_IPW_OTH",
       controls = "+l1_own_shock+l2_own_shock")

run_lp(reg, outcome="labor_force", denominator="population", 
       controls = "+l1_own_shock+l2_own_shock")

run_lp(reg, outcome="labor_force", denominator="population", 
       controls = "+l1_own_shock+l2_own_shock+w_fn_mean_dest_IPW_US")

# unemployment 
run_lp(reg, outcome="unemployed", denominator="labor_force", 
       shock = "w_IPW_US", 
       instrument = "w_ex_mean_dest_IPW_OTH",
       controls = "+l1_own_shock+l2_own_shock+l1_w_ex_mean_dest_IPW_OTH",
       end_year = 2010)

run_lp(reg, outcome="unemployed", denominator="labor_force", 
       shock = "w_IPW_US", 
       instrument = "w_IPW_OTH",
       controls = "+l1_own_shock+l2_own_shock",
       end_year = 2010)

run_lp(reg, outcome="unemployed", denominator="labor_force", 
       controls = "+ex_mean_dest_IPW_US", end_year = 2012)

run_lp(reg, outcome="unemployed", denominator="labor_force",
       shock="w_ex_mean_dest_IPW_US",
       instrument="w_ex_mean_dest_IPW_OTH",
       end_year=2008)


# Commuting
run_lp(reg, outcome="outside_jobs", denominator="population",
       controls = "+ex_mean_dest_IPW_US", end_year = 2013)
run_lp(reg, outcome="outside_jobs", denominator="population",
       controls = "+ex_mean_dest_IPW_US")
run_lp(reg, outcome="outside_jobs", denominator="labor_force",
       controls = "+ex_mean_dest_IPW_US", end_year = 2013)
run_lp(reg, outcome="outside_jobs", denominator="workplace_emp",
       controls = "+ex_mean_dest_IPW_US", end_year = 2007)
run_lp(reg, outcome="outside_jobs", denominator="workplace_emp",
         controls = "+ex_mean_dest_IPW_US", start_year = 2010, end_year = 2013)

# Migration
run_lp(reg, outcome="exemptions_net_migration", denominator="population",
              controls = "+ex_mean_dest_IPW_US")
run_lp(reg, outcome="exemptions_3_outflow", denominator="exemptions_total_migration")
run_lp(reg, outcome="returns_3_outflow", denominator="returns_total_migration")
run_lp(reg, outcome="returns_net_migration", denominator="population")


# multi shock regs -------------------------------------------------------------

lf_multi <- run_lp_multi(
  reg,
  outcome="unemployed",
  denominator="labor_force",
  shock_1="w_IPW_US",
  shock_2="w_ex_mean_dest_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_ex_mean_dest_IPW_OTH",
  start_year=2000,
  end_year=2010
)

lf_multi <- run_lp_multi(
  reg,
  outcome="exemptions_net_migration",
  denominator="population",
  shock_1="w_IPW_US",
  shock_2="w_ex_mean_dest_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_ex_mean_dest_IPW_OTH",
  controls="+l1_own_shock+l2_own_shock",
  start_year=2000,
  end_year=2007
)
lf_multi <- run_lp_multi(
  reg,
  outcome="outside_jobs",
  denominator="population",
  shock_1="w_IPW_US",
  shock_2="w_ex_mean_dest_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_ex_mean_dest_IPW_OTH",
  controls="+l1_own_shock+l2_own_shock",
  start_year=2000,
  end_year=2007
)



lf_multi <- run_lp_multi(
  reg,
  outcome="labor_force",
  denominator="population",
  shock_1="w_IPW_US",
  shock_2="w_ex_mean_dest_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_ex_mean_dest_IPW_OTH",
  controls="+l1_own_shock+l2_own_shock",
  start_year=2000,
  end_year=2010
)


lf_multi <- run_lp_multi(
  reg,
  outcome="outside_jobs",
  denominator="labor_force",
  shock_1="l1_w_IPW_US",
  shock_2="l1_w_ex_mean_dest_IPW_US",
  instrument_1="l1_w_IPW_OTH",
  instrument_2="l1_w_ex_mean_dest_IPW_OTH",
  start_year=2000,
  end_year=2010, 
  controls = "+rw_mean_into_IPW_US"
)

lf_multi <- run_lp_multi(
  reg,
  outcome="outside_jobs",
  denominator="labor_force",
  shock_1="l1_w_IPW_US",
  shock_2="l1_w_ex_mean_dest_IPW_US",
  instrument_1="l1_w_IPW_OTH",
  instrument_2="l1_w_ex_mean_dest_IPW_OTH",
  start_year=2000,
  end_year=2007, 
  controls = "+rw_share_into_less_unemp"
)

lf_multi <- run_lp_multi(
  reg,
  outcome="outside_jobs",
  denominator="labor_force",
  shock_1="l1_w_IPW_US",
  shock_2="l1_w_ex_mean_dest_IPW_US",
  instrument_1="l1_w_IPW_OTH",
  instrument_2="l1_w_ex_mean_dest_IPW_OTH",
  controls = "+l1_own_shock+l2_own_shock+exemptions_3_outflow_share_net_migration", 
  start_year=2000,
  end_year=2007
)

lf_multi <- run_lp_multi(
  reg,
  outcome="outside_jobs",
  denominator="labor_force",
  shock_1="l1_w_IPW_US",
  shock_2="l1_w_ex_mean_dest_IPW_US",
  instrument_1="l1_w_IPW_OTH",
  instrument_2="l1_w_ex_mean_dest_IPW_OTH",
  controls = "+l1_own_shock", 
  start_year=2000,
  end_year=2007
)

lf_multi <- run_lp_multi(
  reg,
  outcome="outside_jobs",
  denominator="labor_force",
  shock_1="w_IPW_US",
  shock_2="w_ex_mean_dest_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_ex_mean_dest_IPW_OTH",
  start_year=2000,
  end_year=2007
)
pca_dt <- reg[, c("unemployed","IPW_US", "ex_mean_dest_IPW_US", "rw_share_into_less_unemp", "year")]
pca_dt<- pca_dt[!is.na(IPW_US), ]
pca_dt<- pca_dt[!is.na(unemployed), ]
pca_dt<- pca_dt[!is.na(ex_mean_dest_IPW_US), ]
pca_dt<- pca_dt[!is.na(rw_share_into_less_unemp), ]
pca <- prcomp(pca_dt)