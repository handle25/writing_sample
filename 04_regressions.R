################################################################################
# Regressions 
################################################################################
path <- "D:/writing_sample/data"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
reg <- fread(paste0(path, "/output/transformed_reg.csv"))
date <- Sys.Date()

reg07 <- reg[year %in% c(2000, 2007)]
reg13 <- reg[year %in% c(2007, 2013)]
reg25 <- reg[year %in% c(2019, 2025)]

reg07[, t2 := as.integer(year == 2007)]
reg13[, t2 := as.integer(year == 2013)]
reg25[, t2 := as.integer(year == 2025)]

################################################################################
# Variable labels
################################################################################

names_dict <- c(
  
  # Regressors -----------------------------------------------------------------
  
  "fit_IPW_US" =
    "Import Exposure",
  "w_fit_IPW_US" =
    "Import Exposure",
  
  "w_IPW_US" =
    "Import Exposure",
  
  "t2" =
    "Period 2",
  
  "l_shind_manuf" =
    "Lagged Manufacturing Share",
  
  
  # Services -------------------------------------------------------------------
  
  "w_d_total_servc_jobs_share_resident_emp" =
    "$\\Delta \\frac{Emp_{resident, service}}{Emp_{resident}}$",
  
  "w_d_outside_servc_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Emp_{outside, service}}{Emp_{outside}}$",
  
  "w_d_outside_servc_jobs_share_total_servc_jobs" =
    "$\\Delta \\frac{Emp_{outside, service}}{Emp_{resident, service}}$",
  
  "w_d_outside_servc_jobs_share_resident_emp" =
    "$\\Delta \\frac{Emp_{outside, service}}{Emp_{resident}}$",
  
  "w_d_outside_servc_jobs_share_population" =
    "$\\Delta \\frac{Emp_{outside, service}}{Population}$",
  
  "w_d_servc_jobs_share_population" =
    "$\\Delta \\frac{Emp_{resident, service}}{Population}$",
  
  
  # Goods ----------------------------------------------------------------------
  
  "w_d_total_goods_jobs_share_resident_emp" =
    "$\\Delta \\frac{Emp_{resident, goods}}{Emp_{resident}}$",
  
  "w_d_outside_goods_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Emp_{outside, goods}}{Emp_{outside}}$",
  
  "w_d_outside_goods_jobs_share_total_goods_jobs" =
    "$\\Delta \\frac{Emp_{outside, goods}}{Emp_{resident, goods}}$",
  
  "w_d_outside_goods_jobs_share_resident_emp" =
    "$\\Delta \\frac{Emp_{outside, goods}}{Emp_{resident}}$",
  
  "w_d_outside_goods_jobs_share_population" =
    "$\\Delta \\frac{Emp_{outside, goods}}{Population}$",
  
  
  # Outside employment ---------------------------------------------------------
  
  "w_d_outside_jobs_share_resident_emp" =
    "$\\Delta \\frac{Emp_{outside}}{Emp_{resident}}$",
  
  "w_d_outside_jobs_share_total_goods_jobs" =
    "$\\Delta \\frac{Emp_{outside}}{Emp_{resident, goods}}$",
  
  "w_d_outside_jobs_share_population" =
    "$\\Delta \\frac{Emp_{outside}}{Population}$",
  
  
  # Employment / population ----------------------------------------------------
  
  "w_d_resident_emp_share_population" =
    "$\\Delta \\frac{Emp_{resident}}{Population}$",
  
  "w_d_workplace_emp_share_population" =
    "$\\Delta \\frac{Emp_{workplace}}{Population}$",
  
  
  # Migration ------------------------------------------------------------------
  
  
  "w_net_migration_share_resident_emp" =
    "$\\frac{Net\\ Migration}{Emp_{resident}}$",
  
  
  # Earnings groups: outside employment / outside employment ------------------
  
  "w_d_outside_earn1250_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Emp_{outside, low}}{Emp_{outside}}$",
  
  "w_d_outside_earn1251_3333_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Emp_{outside, mid}}{Emp_{outside}}$",
  
  "w_d_outside_earn3333_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Emp_{outside, high}}{Emp_{outside}}$",
  
  
  # Earnings groups: outside employment / resident employment -----------------
  
  "w_d_outside_earn1250_jobs_share_resident_emp" =
    "$\\Delta \\frac{Emp_{outside, low}}{Emp_{resident}}$",
  
  "w_d_outside_earn1251_3333_jobs_share_resident_emp" =
    "$\\Delta \\frac{Emp_{outside, mid}}{Emp_{resident}}$",
  
  "w_d_outside_earn3333_jobs_share_resident_emp" =
    "$\\Delta \\frac{Emp_{outside, high}}{Emp_{resident}}$",
  
  
  # Earnings groups: outside employment / population --------------------------
  
  "w_d_outside_earn1250_jobs_share_population" =
    "$\\Delta \\frac{Emp_{outside, low}}{Population}$",
  
  "w_d_outside_earn1251_3333_jobs_share_population" =
    "$\\Delta \\frac{Emp_{outside, mid}}{Population}$",
  
  "w_d_outside_earn3333_jobs_share_population" =
    "$\\Delta \\frac{Emp_{outside, high}}{Population}$",
  
  
  # Total resident earnings groups / outside employment -----------------------
  
  "w_d_total_earn1250_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Emp_{resident, low}}{Emp_{outside}}$",
  
  "w_d_total_earn1251_3333_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Emp_{resident, mid}}{Emp_{outside}}$",
  
  "w_d_total_earn3333_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Emp_{resident, high}}{Emp_{outside}}$",
  
  "w_d_outside_earn3333_jobs_share_workplace_emp" =
    "$\\Delta \\frac{Emp_{resident, high}}{Jobs_{local}}$", 
  
  "d_workplace_emp_share_resident_emp" = 
    "$\\Delta \\frac{Jobs_{local}}{Jobs_{resident}}$",
  
  "net_migration" =
    "$Net\\ Migration$",
  
  
  "w_net_migration_share_resident_emp" =
    "$\\frac{Net\\ Migration}{Emp_{resident}}$",
  
  "d_sh_empl_mfg" =
    "$\\Delta \\frac{Manufacturing}{Employment}$",
  
  "w_net_migration_share_workplace_emp" =
    "$\\frac{Net\\ Migration}{Emp_{workplace}}$",
  
  "w_net_migration_share_population" =
    "$\\frac{Net\\ Migration}{Population}$",
  
  "net_migration" =
    "$Net\\ Migration$",
  
  "net_migration_share_resident_emp" =
    "$\\frac{Net\\ Migration}{Emp_{resident}}$",
  
  "net_migration_share_workplace_emp" =
    "$\\frac{Net\\ Migration}{Emp_{workplace}}$",
  
  "net_migration_share_population" =
    "$\\frac{Net\\ Migration}{Population}$", 
  
  "l_y" = "Lagged Dep Var", 
  "w_d_labor_force_share_population"= 
    "$\\Delta \\frac{Labor\\ Force}{Population}$",
  
  "w_d_unemployed_share_labor_force"= 
    "$\\Delta \\frac{Unemployed}{Labor\\ Force}$",
  
  "w_net_migration_share_population_2000"= 
    "$ \\frac{Net\\ Migration}{Population_{2000}}$",
  
  "w_net_migration_share_population_base_year"= 
    "$ \\frac{Net\\ Migration}{Population_{t0}}$"
  
)


################################################################################
# Baseline models
################################################################################
baseline <- function(
    dep_vars,
    desc,
    base_year = 1995,
    shock_us = "w_IPW_US",
    shock_oth = "w_IPW_OTH"
) {
  
  # Years needed to construct baseline levels correctly
  years_keep <- unique(c(base_year, 2000, 2007, 2013))
  
  reg_temp <- copy(reg[year %in% years_keep])
  setorder(reg_temp, area_fips, year)
  
  mods07 <- lapply(dep_vars, function(y) {
    
    base_y <- sub("^w_", "", y)
    base_y <- sub("^d_", "", base_y)
    
    # Baseline level of outcome
    reg_temp[, l_y := shift(get(base_y)), by = area_fips]
    
    reg_temp[, t2 := as.integer(year == 2007)]
    
    fml <- as.formula(
      paste0(
        y,
        " ~ t2 + l_y | ",
        shock_us, " ~ ", shock_oth
      )
    )
    
    feols(
      fml,
      data = reg_temp[year %in% c(2000, 2007)],
      weights = ~baseline_emp,
      cluster = ~statefip
    )
  })
  
  
  mods13 <- lapply(dep_vars, function(y) {
    
    base_y <- sub("^w_", "", y)
    base_y <- sub("^d_", "", base_y)
    
    reg_temp[, l_y := shift(get(base_y)), by = area_fips]
    
    reg_temp[, t2 := as.integer(year == 2013)]
    
    fml <- as.formula(
      paste0(
        y,
        " ~ t2  + l_y |  ",
        shock_us, " ~ ", shock_oth
      )
    )
    
    feols(
      fml,
      data = reg_temp[year %in% c(2007, 2013)],
      weights = ~baseline_emp,
      cluster = ~statefip
    )
  })
  
  
  mods <- c(mods07, mods13)
  
  names(mods) <- c(dep_vars, dep_vars)
  
  etable(
    mods,
    dict = names_dict,
    drop = "Constant",
    digits = 3,
    headers = list(
      "2000--2007" = length(dep_vars),
      "2007--2013" = length(dep_vars)
    ),
    tex = TRUE,
    file = paste0(
      path,
      "/../figures/model_",
      desc,
      date,
      ".tex"
    ),
    replace = TRUE
  )
  
  invisible(mods)
}

baseline(
  "d_sh_empl_mfg",
  "d_sh_empl_mfg"
)

baseline(
  "w_net_migration_share_population_2000",
  "w_net_migration_share_population_2000"
)
baseline(
  "w_net_migration_share_population_base_year",
  "w_net_migration_share_population_base_year"
)

  
baseline(
  "w_d_labor_force_share_population_2000",
  "w_d_labor_force_share_population_2000"
)
baseline(
  "w_d_outside_jobs_share_population_t_1",
  "w_d_outside_jobs_share_population_t_1",
  base_year = 1990,
  shock_us = "w_IPW_US_10yr",
  shock_oth = "w_IPW_OTH_10yr"
)

baseline(
  "w_d_outside_jobs_share_population_t_1",
  "w_d_outside_jobs_share_population_t_1",
  base_year = 1990,
  shock_us = "w_IPW_US_10yr",
  shock_oth = "w_IPW_OTH_10yr"
)


baseline(
  "w_d_workplace_emp_share_population",
  "w_d_workplace_emp_share_population"
)

baseline(
  "w_d_unemployed_share_labor_force",
  "w_d_unemployed_share_labor_force"
)



baseline(
  "net_migration_share_population",
  "net_migration_share_population"
)

baseline(
  "workplace_emp_share_population",
  "workplace_emp_share_population"
)

baseline(
  "resident_emp_share_population",
  "resident_emp_share_population"
)


################################################################################
# Baseline actual 
################################################################################

baseline <- function(dep_vars, desc) {
  
  mods <- lapply(dep_vars, function(y) {
    
    fml <- as.formula(
      paste0(
        y,
        " ~ t2 | ",
        "w_IPW_US ~ w_IPW_OTH"
      )
    )
    
    feols(
      fml,
      data = reg[year %in% c(2007, 2013)],
      weights = ~baseline_emp,
      cluster = ~statefip+year
    )
  })
  
  names(mods) <- dep_vars
  
  etable(
    mods,
    dict = names_dict,
    drop = "Constant",
    digits = 3, 
    tex = TRUE,
    file = paste0(
      path,
      "/../figures/model_",
      desc,
      date, ".tex"
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
        " ~ t2  | ",
        "w_IPW_US ~ w_IPW_OTH"
      )
    )
    
    feols(
      fml,
      data = reg,
      weights = ~baseline_emp,
      cluster = ~statefip,
      split = ~quantyes ile
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
      "/../figures/quantile_",
      desc,
      date, ".tex"
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
reg[, t2 := ifelse(year == 2013, 1,0)]
reg13 <- reg[year %in% c(2007, 2013)]
dep_vars <- c(
  "w_d_outside_jobs_share_total_goods_jobs",
  "w_d_outside_goods_jobs_share_outside_jobs",
  "w_d_outside_goods_jobs_share_population",
  "w_d_resident_emp_share_population"
)


baseline(
  dep_vars,
  "outside_jobs")


################################################################################
# Service outcomes
################################################################################

dep_vars <- c(
  "w_d_total_servc_jobs_share_resident_emp",
  "w_d_outside_servc_jobs_share_outside_jobs",
  "w_d_outside_servc_jobs_share_total_servc_jobs",
  "w_d_outside_servc_jobs_share_population"
)

baseline(
  dep_vars,
  "services"
)




################################################################################
# Goods outcomes
################################################################################

dep_vars <- c(
  "w_d_total_goods_jobs_share_resident_emp",
  "w_d_resident_emp_share_population",
  "w_d_outside_goods_jobs_share_outside_jobs",
  "w_d_outside_goods_jobs_share_total_goods_jobs"
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
  "w_d_outside_servc_jobs_share_total_servc_jobs",
  "w_d_total_servc_jobs_share_resident_emp",
  
  # Goods
  "w_d_total_goods_jobs_share_resident_emp",
  
  # Migration
  "w_net_migration_share_resident_emp",
  "w_net_migration_share_workplace_emp",
  
  # Employment / population
  "w_d_resident_emp_share_population",
  "w_d_workplace_emp_share_population",
  
  # Outside employment
  "w_d_outside_jobs_share_resident_emp", 
  "w_d_workplace_emp_share_resident_emp"
)

mods_testing <- baseline(
  dep_vars,
  "testing"
)


################################################################################
# Migration outcomes
################################################################################

dep_vars <- c(
  
  # Services / commuting
  "w_net_migration_share_resident_emp",
  "w_net_migration_share_workplace_emp",
  "w_net_migration_share_population"
)

mods_testing <- baseline(
  dep_vars,
  "w_migration"
)


dep_vars <- c(
  
  # Services / commuting
  "net_migration_share_resident_emp",
  "net_migration_share_workplace_emp",
  "net_migration_share_population"
)

mods_testing <- baseline(
  dep_vars,
  "migration"
)

################################################################################
# Significant / headline outcomes
################################################################################

dep_vars <- c(
  
  # Services / commuting
  "w_d_outside_servc_jobs_share_total_servc_jobs",
  "w_d_outside_servc_jobs_share_resident_emp",
  "w_d_outside_jobs_share_resident_emp",
  
  # Services overall
  "w_d_total_servc_jobs_share_resident_emp",
  
  # Employment / population
  "w_d_resident_emp_share_population",
  "w_d_workplace_emp_share_population",
  
  # High-earning outside employment
  "w_d_outside_earn3333_jobs_share_resident_emp",
  "w_d_outside_earn3333_jobs_share_outside_jobs",
  "w_d_outside_earn3333_jobs_share_workplace_emp"
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



################################################################################
# Maps
################################################################################
counties <- counties(cb = TRUE, year = 2020)
counties$area_fips <- as.integer(counties$GEOID)

q <- quantile(reg[,d_sh_empl_mfg], probs = c(.05, .95), na.rm = T)
reg[,w_d_sh_empl_mfg := d_sh_empl_mfg]
reg[w_d_sh_empl_mfg < q[1], w_d_sh_empl_mfg := q[1]]
reg[w_d_sh_empl_mfg > q[2], w_d_sh_empl_mfg := q[2]]
plot_reg <- copy(reg)

for (y in c(2000, 2007, 2013)){
  map_dt <- merge(
    counties,
    plot_reg,
    by = "area_fips",
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
      xlim = c(-93, -77),
      ylim = c(37, 48)
    ) + 
    scale_fill_gradient2(
      low = "red",
      high = "blue",
      midpoint = 0
    ) +
    theme_void() 
  ggsave(paste0(path,"/../figures/w_d_sh_empl_mfg_",y, date,".pdf"), 
         height = 6, width = 6)
  
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
  
  ggsave(paste0(path,"/../figures/w_d_sh_empl_mfg_",y,"_full", date, ".pdf"),
         height = 6, width = 6)
  
  assign(paste0("plot", y), plot)
  assign(paste0("plot_full", y), plot_full)
  
}

