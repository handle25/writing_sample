################################################################################
# Regressions 
################################################################################

reg <- fread(paste0(path, "/output/transformed_reg.csv"))
reg <- reg[year %in% c(2007, 2013), ]
reg[, t2 := as.integer(year == 2013)]


# TEMPORARY: winsorize shocks at 0.5th / 99.5th percentiles -------------------

for (v in c("IPW_US", "IPW_OTH")) {
  
  q01 <- quantile(reg[[v]], 0.005, na.rm = TRUE)
  q99 <- quantile(reg[[v]], 0.995, na.rm = TRUE)
  
  reg[get(v) < q01, (v) := q01]
  reg[get(v) > q99, (v) := q99]
}


################################################################################
# Variable labels
################################################################################

names_dict <- c(
  
  # Regressors -----------------------------------------------------------------
  
  "fit_IPW_US" =
    "Import Exposure",
  
  "t2" =
    "Period 2",
  
  "l_shind_manuf" =
    "Initial Manufacturing Share",
  
  
  # Services -------------------------------------------------------------------
  
  "w_d_total_servc_jobs_share_resident_emp" =
    "$\\Delta \\frac{Employment_{resident, service}}{Employment_{resident}}$",
  
  "w_d_outside_servc_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Employment_{outside, service}}{Employment_{outside}}$",
  
  "w_d_outside_servc_jobs_share_service_jobs" =
    "$\\Delta \\frac{Employment_{outside, service}}{Employment_{resident, service}}$",
  
  "w_d_outside_servc_jobs_share_resident_emp" =
    "$\\Delta \\frac{Employment_{outside, service}}{Employment_{resident}}$",
  
  "w_d_outside_servc_jobs_share_population" =
    "$\\Delta \\frac{Employment_{outside, service}}{Population}$",
  
  "w_d_servc_jobs_share_population" =
    "$\\Delta \\frac{Employment_{resident, service}}{Population}$",
  
  
  # Goods ----------------------------------------------------------------------
  
  "w_d_total_goods_jobs_share_resident_emp" =
    "$\\Delta \\frac{Employment_{resident, goods}}{Employment_{resident}}$",
  
  "w_d_outside_goods_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Employment_{outside, goods}}{Employment_{outside}}$",
  
  "w_d_outside_goods_jobs_share_goods_jobs" =
    "$\\Delta \\frac{Employment_{outside, goods}}{Employment_{resident, goods}}$",
  
  "w_d_outside_goods_jobs_share_resident_emp" =
    "$\\Delta \\frac{Employment_{outside, goods}}{Employment_{resident}}$",
  
  "w_d_outside_goods_jobs_share_population" =
    "$\\Delta \\frac{Employment_{outside, goods}}{Population}$",
  
  
  # Outside employment ---------------------------------------------------------
  
  "w_d_outside_jobs_share_resident_emp" =
    "$\\Delta \\frac{Employment_{outside}}{Employment_{resident}}$",
  
  "w_d_outside_jobs_share_goods_jobs" =
    "$\\Delta \\frac{Employment_{outside}}{Employment_{resident, goods}}$",
  
  "w_d_outside_jobs_share_population" =
    "$\\Delta \\frac{Employment_{outside}}{Population}$",
  
  
  # Employment / population ----------------------------------------------------
  
  "w_d_resident_emp_population_ratio" =
    "$\\Delta \\frac{Employment_{resident}}{Population}$",
  
  "w_d_workplace_emp_population_ratio" =
    "$\\Delta \\frac{Employment_{workplace}}{Population}$",
  
  
  # Migration ------------------------------------------------------------------
  
  "w_d_net_migration_pctchange" =
    "$\\Delta Net\\ Migration$",
  
  "w_net_migration_share_emp" =
    "$\\frac{Net\\ Migration}{Employment_{resident}}$",
  
  
  # Earnings groups: outside employment / outside employment ------------------
  
  "w_d_outside_earn1250_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Employment_{outside, low}}{Employment_{outside}}$",
  
  "w_d_outside_earn1251_3333_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Employment_{outside, mid}}{Employment_{outside}}$",
  
  "w_d_outside_earn3333_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Employment_{outside, high}}{Employment_{outside}}$",
  
  
  # Earnings groups: outside employment / resident employment -----------------
  
  "w_d_outside_earn1250_jobs_share_resident_emp" =
    "$\\Delta \\frac{Employment_{outside, low}}{Employment_{resident}}$",
  
  "w_d_outside_earn1251_3333_jobs_share_resident_emp" =
    "$\\Delta \\frac{Employment_{outside, mid}}{Employment_{resident}}$",
  
  "w_d_outside_earn3333_jobs_share_resident_emp" =
    "$\\Delta \\frac{Employment_{outside, high}}{Employment_{resident}}$",
  
  
  # Earnings groups: outside employment / population --------------------------
  
  "w_d_outside_earn1250_jobs_share_population" =
    "$\\Delta \\frac{Employment_{outside, low}}{Population}$",
  
  "w_d_outside_earn1251_3333_jobs_share_population" =
    "$\\Delta \\frac{Employment_{outside, mid}}{Population}$",
  
  "w_d_outside_earn3333_jobs_share_population" =
    "$\\Delta \\frac{Employment_{outside, high}}{Population}$",
  
  
  # Total resident earnings groups / outside employment -----------------------
  
  "w_d_total_earn1250_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Employment_{resident, low}}{Employment_{outside}}$",
  
  "w_d_total_earn1251_3333_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Employment_{resident, mid}}{Employment_{outside}}$",
  
  "w_d_total_earn3333_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Employment_{resident, high}}{Employment_{outside}}$"
)


################################################################################
# Baseline models
################################################################################

baseline <- function(dep_vars, desc) {
  
  mods <- lapply(dep_vars, function(y) {
    
    fml <- as.formula(
      paste0(
        y,
        " ~ t2 + l_shind_manuf | ",
        "IPW_US ~ IPW_OTH"
      )
    )
    
    feols(
      fml,
      data = reg,
      weights = ~baseline_emp,
      cluster = ~statefip
    )
  })
  
  names(mods) <- dep_vars
  
  etable(
    mods,
    dict = names_dict,
    drop = "Constant",
    tex = TRUE,
    file = paste0(
      path,
      "/../figures/model_",
      desc,
      "_0813.tex"
    ),
    replace = TRUE
  )
  
  invisible(mods)
}


################################################################################
# Quantile models
################################################################################

quantile_models <- function(dep_vars, desc) {
  
  mods <- lapply(dep_vars, function(y) {
    
    fml <- as.formula(
      paste0(
        y,
        " ~ t2 + l_shind_manuf | ",
        "IPW_US ~ IPW_OTH"
      )
    )
    
    feols(
      fml,
      data = reg,
      weights = ~baseline_emp,
      cluster = ~statefip,
      split = ~quantile
    )
  })
  
  names(mods) <- dep_vars
  
  etable(
    mods,
    dict = names_dict,
    drop = "Constant",
    headers = c("Q1", "Q2", "Q3"),
    tex = TRUE,
    file = paste0(
      path,
      "/../figures/model_",
      desc,
      "_0813.tex"
    ),
    replace = TRUE
  )
  
  invisible(mods)
}


################################################################################
# Population-size terciles
################################################################################

reg[, quantile := cut(
  population,
  breaks = quantile(
    population,
    probs = 0:3/3,
    na.rm = TRUE
  ),
  include.lowest = TRUE,
  labels = FALSE
), by = year]


################################################################################
# Outside employment outcomes
################################################################################

dep_vars <- c(
  "w_d_outside_jobs_share_goods_jobs",
  "w_d_outside_goods_jobs_share_outside_jobs",
  "w_d_outside_goods_jobs_share_population",
  "w_d_resident_emp_population_ratio"
)

baseline(
  dep_vars,
  "outside_jobs"
)


################################################################################
# Service outcomes
################################################################################

dep_vars <- c(
  "w_d_total_servc_jobs_share_resident_emp",
  "w_d_outside_servc_jobs_share_outside_jobs",
  "w_d_outside_servc_jobs_share_service_jobs",
  "w_d_outside_servc_jobs_share_population"
)

baseline(
  dep_vars,
  "services"
)

quantile_models(
  dep_vars,
  "services"
)


################################################################################
# Goods outcomes
################################################################################

dep_vars <- c(
  "w_d_total_goods_jobs_share_resident_emp",
  "w_d_outside_goods_jobs_share_outside_jobs",
  "w_d_outside_goods_jobs_share_goods_jobs"
)

baseline(
  dep_vars,
  "goods"
)


################################################################################
# Broad testing table
################################################################################

dep_vars <- c(
  
  # Services / commuting
  "w_d_outside_servc_jobs_share_outside_jobs",
  "w_d_outside_servc_jobs_share_population",
  "w_d_outside_servc_jobs_share_service_jobs",
  "w_d_total_servc_jobs_share_resident_emp",
  
  # Goods
  "w_d_total_goods_jobs_share_resident_emp",
  
  # Migration
  "w_d_net_migration_pctchange",
  "w_net_migration_share_emp",
  
  # Employment / population
  "w_d_resident_emp_population_ratio",
  "w_d_workplace_emp_population_ratio",
  
  # Outside employment
  "w_d_outside_jobs_share_resident_emp"
)

mods_testing <- baseline(
  dep_vars,
  "testing"
)


################################################################################
# Significant / headline outcomes
################################################################################

dep_vars <- c(
  
  # Services / commuting
  "w_d_outside_servc_jobs_share_service_jobs",
  "w_d_outside_servc_jobs_share_resident_emp",
  "w_d_outside_jobs_share_resident_emp",
  
  # Services overall
  "w_d_total_servc_jobs_share_resident_emp",
  
  # Employment / population
  "w_d_resident_emp_population_ratio",
  "w_d_workplace_emp_population_ratio",
  
  # High-earning outside employment
  "w_d_outside_earn3333_jobs_share_resident_emp",
  "w_d_outside_earn3333_jobs_share_outside_jobs"
)

mods_testing <- baseline(
  dep_vars,
  "significant"
)


################################################################################
# Earnings outcomes
################################################################################

dep_vars <- c(
  
  # Low earnings
  "w_d_outside_earn1250_jobs_share_resident_emp",
  "w_d_outside_earn1250_jobs_share_outside_jobs",
  
  # Middle earnings
  "w_d_outside_earn1251_3333_jobs_share_resident_emp",
  "w_d_outside_earn1251_3333_jobs_share_outside_jobs",
  
  # High earnings
  "w_d_outside_earn3333_jobs_share_resident_emp",
  "w_d_outside_earn3333_jobs_share_outside_jobs",
  "w_d_outside_earn3333_jobs_share_population"
)

mods_testing <- baseline(
  dep_vars,
  "income"
)


################################################################################
# Earnings outcomes by population tercile
################################################################################

for (i in seq_along(dep_vars)) {
  
  quantile_models(
    dep_vars[i],
    paste0(
      "quantile_",
      dep_vars[i]
    )
  )
}


################################################################################
# Individual testing regression
################################################################################

mod <- feols(
  w_d_outside_earn3333_jobs_share_resident_emp ~
    t2 + l_shind_manuf |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

mod

summary(
  mod,
  stage = 1
)

summary(
  mod,
  stage = 2
)


exit


################################################################################
# Maps
################################################################################

# counties <- counties(cb = TRUE, year = 2020)
# 
# counties$area_fips <- as.integer(counties$GEOID)
# 
# map_dt <- merge(
#   counties,
#   reg,
#   by = "area_fips", 
#   all.x = TRUE
# )
# 
# ggplot(map_dt) +
#   geom_sf(
#     aes(fill = outside_d_jobs),
#     color = "grey70",
#     linewidth = 0.1
#   ) +
#   coord_sf(
#     xlim = c(-125, -66),
#     ylim = c(24, 50)
#   ) +
#   theme_void()
# 
# ggplot(map_dt) +
#   geom_sf(
#     aes(fill = d_outside_d_jobs),
#     color = "grey70",
#     linewidth = 0.1
#   ) +
#   coord_sf(
#     xlim = c(-80, -90),
#     ylim = c(34, 50)
#   ) +
#   theme_void()