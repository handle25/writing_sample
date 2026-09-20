################################################################################
# Created 8.22.2026
# Author: Sophie Handley
# Purpose: Run CZ LP heterogeneity regressions by baseline industry HHI tercile
################################################################################

rm(list = ls())

# Paths -----------------------------------------------------------------------
path <- "D:/writing_sample/data"
figs <- "D:/writing_sample/figures"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
setwd(path)


################################################################################
# Function definition
################################################################################

run_lp_heterogeneity <- function(
    reg,
    outcome,
    heterogeneity_var = "industry_hhi_base",
    start_year = 2000,
    end_year = 2014,
    horizons = 0:7,
    figure = TRUE,
    save_figure = TRUE
) {
  
  # Work on a copy so the original data.table is not modified
  reg <- copy(reg)
  
  # Sort before constructing lags/leads
  setorder(reg, commuting_zone_id_2000, year)
  
  # Keep balanced panel -------------------------------------------------------
  reg <- reg[
    commuting_zone_id_2000 %in%
      reg[, .N, by = commuting_zone_id_2000][
        N == length(unique(reg$year)),
        commuting_zone_id_2000
      ]
  ]
  
  
  ##############################################################################
  # Construct heterogeneity terciles
  ##############################################################################
  
  # One baseline heterogeneity value per commuting zone.
  # If industry_hhi_base is already constant within CZ, this simply retains it.
  # Otherwise, take the first non-missing value in time order.
  hetero_cz <- reg[
    !is.na(get(heterogeneity_var)),
    .(
      heterogeneity_value = first(get(heterogeneity_var))
    ),
    by = commuting_zone_id_2000
  ]
  
  # Rank CZs rather than CZ-year observations so each CZ receives equal weight
  # in defining the tercile cutoffs.
  hetero_cz[, pct_rank :=
              frank(
                heterogeneity_value,
                ties.method = "average"
              ) / .N]
  
  hetero_cz[, heterogeneity_group :=
              fcase(
                pct_rank <= 1 / 3, "Low",
                pct_rank <= 2 / 3, "Middle",
                default = "High"
              )]
  
  hetero_cz[, heterogeneity_group :=
              factor(
                heterogeneity_group,
                levels = c("Low", "Middle", "High")
              )]
  
  reg <- merge(
    reg,
    hetero_cz[, .(
      commuting_zone_id_2000,
      heterogeneity_value,
      heterogeneity_group
    )],
    by = "commuting_zone_id_2000",
    all.x = TRUE
  )
  
  setorder(reg, commuting_zone_id_2000, year)
  
  # Check number of CZs in each tercile
  print(
    reg[
      !is.na(heterogeneity_group),
      .(N_CZ = uniqueN(commuting_zone_id_2000)),
      by = heterogeneity_group
    ]
  )
  
  
  ##############################################################################
  # Construct LP variables
  ##############################################################################
  
  base <- outcome
  
  reg[, y_lp := get(base)]
  
  # Lagged outcome controls
  reg[, l1_y := shift(y_lp, 1), by = commuting_zone_id_2000]
  reg[, l2_y := shift(y_lp, 2), by = commuting_zone_id_2000]
  reg[, l3_y := shift(y_lp, 3), by = commuting_zone_id_2000]
  reg[, l4_y := shift(y_lp, 4), by = commuting_zone_id_2000]
  
  # LP outcomes: y_{t+h} - y_{t-1}
  for (h in horizons) {
    
    var <- paste0("diff_", h)
    
    reg[, (var) :=
          shift(y_lp, type = "lead", n = h) -
          shift(y_lp, type = "lag", n = 1),
        by = commuting_zone_id_2000]
  }
  
  
  ##############################################################################
  # Results table
  ##############################################################################
  
  groups <- c("Low", "Middle", "High")
  
  results <- CJ(
    h = horizons,
    heterogeneity_group = groups
  )
  
  results[, heterogeneity_group :=
            factor(
              heterogeneity_group,
              levels = groups
            )]
  
  results[, `:=`(
    coef = NA_real_,
    se = NA_real_,
    N = NA_integer_,
    N_CZ = NA_integer_
  )]
  
  
  ##############################################################################
  # Run LPs separately by tercile
  ##############################################################################
  
  for (qq in groups) {
    
    for (hh in horizons) {
      
      var <- paste0("diff_", hh)
      
      mod <- feols(
        as.formula(
          paste0(
            var,
            " ~ l1_y + l2_y + l3_y + l4_y | year | ",
            "w_IPW_US ~ w_IPW_OTH"
          )
        ),
        data = reg[
          year %in% start_year:end_year &
            heterogeneity_group == qq
        ],
        cluster = ~state + year,
        weight = ~baseline_emp
      )
      
      print(summary(mod))
      
      results[
        h == hh & heterogeneity_group == qq,
        `:=`(
          coef = coef(mod)["fit_w_IPW_US"],
          se = se(mod)["fit_w_IPW_US"],
          N = nobs(mod),
          N_CZ = uniqueN(
            reg[
              year %in% start_year:end_year &
                heterogeneity_group == qq,
              commuting_zone_id_2000
            ]
          )
        )
      ]
    }
  }
  
  
  ##############################################################################
  # Confidence intervals
  ##############################################################################
  
  results[, `:=`(
    lower90 = coef - 1.64 * se,
    upper90 = coef + 1.64 * se,
    lower95 = coef - 1.96 * se,
    upper95 = coef + 1.96 * se
  )]
  
  
  ##############################################################################
  # Make one plot per tercile and stitch with patchwork
  ##############################################################################
  
  plot_list <- lapply(groups, function(qq) {
    
    ggplot(
      results[heterogeneity_group == qq],
      aes(x = h, y = coef)
    ) +
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
      labs(
        x = "Horizon",
        y = "Coefficient",
        title = paste0(qq, " ", heterogeneity_var)
      ) +
      theme_bw()
  })
  
  # Separate ggplots automatically allow free y-scales.
  p <- plot_list[[1]] +
    plot_list[[2]] +
    plot_list[[3]] +
    patchwork::plot_layout(ncol = 3) +
    patchwork::plot_annotation(
      title = paste0(
        "LP Response by Baseline ",
        heterogeneity_var
      )
    )
  
  if (figure) {
    print(p)
  }
  
  if (save_figure) {
    ggsave(
      paste0(
        figs,
        "/heterogeneity_lp_CZ_",
        base,
        "_by_",
        heterogeneity_var,
        ".pdf"
      ),
      p,
      height = 4,
      width = 12
    )
  }
  
  return(
    list(
      results = results,
      plot = p,
      heterogeneity_crosswalk = hetero_cz
    )
  )
}


################################################################################
# State crosswalk
################################################################################

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


################################################################################
# Read regression data
################################################################################

reg <- fread(
  paste0(path, "/output/lp_transformed_reg_CZ.csv")
)

reg <- reg[year < 2017]

setorder(reg, commuting_zone_id_2000, year)

reg[, test := log(resident_emp)]
reg[, total_jobs := total_goods_jobs + total_servc_jobs + total_trade_jobs]


################################################################################
# Keep constant sample
################################################################################

reg <- reg[
  commuting_zone_id_2000 %in%
    reg[, .N, by = commuting_zone_id_2000][
      N == length(unique(reg$year)),
      commuting_zone_id_2000
    ]
]


################################################################################
# Optional winsorization used in baseline file
################################################################################

q99 <- quantile(reg[, industry_hhi], probs = .98, na.rm = TRUE)
q01 <- quantile(reg[, industry_hhi], probs = .02, na.rm = TRUE)

reg[industry_hhi < q01, industry_hhi := q01]
reg[industry_hhi > q99, industry_hhi := q99]


################################################################################
# Heterogeneity regression: split CZs by industry_hhi_base
################################################################################

hetero_results <- run_lp_heterogeneity(
  reg = reg,
  outcome = "w_outside_jobs_share_resident_emp",
  heterogeneity_var = "industry_hhi_base",
  start_year = 2000,
  end_year = 2014,
  horizons = 0:7
)


significant <- c(
  "w_outside_jobs_share_resident_emp",
  "w_outside_servc_jobs_share_resident_emp",
  "w_outside_goods_jobs_share_resident_emp",
  "w_outside_jobs_share_population",
  "w_total_goods_jobs_share_resident_emp",
  "w_manuf_share_emp",
  "w_manuf_emp_share_pop"
)


hetero_results <- run_lp_heterogeneity(
  reg = reg,
  outcome = "net_migration_share_population",
  heterogeneity_var = "industry_hhi_base",
  start_year = 2000,
  end_year = 2014,
  horizons = 0:7
)


# Results data.table
results_industry_hhi <- hetero_results$results

# Patchwork plot
p_industry_hhi <- hetero_results$plot


################################################################################
# Other outcomes: just change outcome
################################################################################

# hetero_hhi <- run_lp_heterogeneity(
#   reg = reg,
#   outcome = "industry_hhi",
#   heterogeneity_var = "industry_hhi_base"
# )
#
# hetero_migration <- run_lp_heterogeneity(
#   reg = reg,
#   outcome = "net_migration_share_population",
#   heterogeneity_var = "industry_hhi_base"
# )
#
# hetero_manuf <- run_lp_heterogeneity(
#   reg = reg,
#   outcome = "w_manuf_share_emp",
#   heterogeneity_var = "industry_hhi_base"
# )


################################################################################
# Optional: save coefficient results
################################################################################

fwrite(
  results_industry_hhi,
  paste0(
    path,
    "/output/lp_heterogeneity_CZ_industry_hhi_base.csv"
  )
)