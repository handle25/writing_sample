  ################################################################################
  # Regressions 
  ################################################################################
  path <- "D:/writing_sample/data"
  reg <- fread(paste0(path, "/output/transformed_reg.csv"))
  reg <- reg[year %in% c(2007, 2013), ]
  reg[, t2 := as.integer(year == 2013)]
  reg[, log_population := log(population)]
  reg[, log_workplace_emp := log(workplace_emp)]
  reg[, log_resident_emp := log(resident_emp)]
  reg[, diff_workplace_resident := log_workplace_emp - log_resident_emp]
  
  # TEMPORARY: winsorize shocks at 0.5th / 99.5th percentiles -------------------
  
  for (v in c("IPW_US", "IPW_OTH",
              "d_workplace_emp_share_resident_emp", 
              "diff_workplace_resident")) {
    
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
      "$\\Delta \\frac{Emp_{resident, service}}{Emp_{resident}}$",
    
    "w_d_outside_servc_jobs_share_outside_jobs" =
      "$\\Delta \\frac{Emp_{outside, service}}{Emp_{outside}}$",
    
    "w_d_outside_servc_jobs_share_service_jobs" =
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
    
    "w_d_outside_goods_jobs_share_goods_jobs" =
      "$\\Delta \\frac{Emp_{outside, goods}}{Emp_{resident, goods}}$",
    
    "w_d_outside_goods_jobs_share_resident_emp" =
      "$\\Delta \\frac{Emp_{outside, goods}}{Emp_{resident}}$",
    
    "w_d_outside_goods_jobs_share_population" =
      "$\\Delta \\frac{Emp_{outside, goods}}{Population}$",
    
    
    # Outside employment ---------------------------------------------------------
    
    "w_d_outside_jobs_share_resident_emp" =
      "$\\Delta \\frac{Emp_{outside}}{Emp_{resident}}$",
    
    "w_d_outside_jobs_share_goods_jobs" =
      "$\\Delta \\frac{Emp_{outside}}{Emp_{resident, goods}}$",
    
    "w_d_outside_jobs_share_population" =
      "$\\Delta \\frac{Emp_{outside}}{Population}$",
    
    
    # Employment / population ----------------------------------------------------
    
    "w_d_resident_emp_population_ratio" =
      "$\\Delta \\frac{Emp_{resident}}{Population}$",
    
    "w_d_workplace_emp_population_ratio" =
      "$\\Delta \\frac{Emp_{workplace}}{Population}$",
    
    
    # Migration ------------------------------------------------------------------
    
    "w_d_net_migration_pctchange" =
      "$\\Delta Net\\ Migration$",
    
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
    
    "w_d_net_migration_pctchange" =
      "$\\Delta Net\\ Migration$",
    
    "w_net_migration_share_resident_emp" =
      "$\\frac{Net\\ Migration}{Emp_{resident}}$",
    
    "w_net_migration_share_workplace_emp" =
      "$\\frac{Net\\ Migration}{Emp_{workplace}}$",
    
    "w_net_migration_share_population" =
      "$\\frac{Net\\ Migration}{Population}$",
    
    "w_net_migration_share_population_t0" =
      "$\\frac{Net\\ Migration}{Population_{t_0}}$",
    
    "net_migration" =
      "$Net\\ Migration$",
    
    "d_net_migration_pctchange" =
      "$\\Delta Net\\ Migration$",
    
    "net_migration_share_resident_emp" =
      "$\\frac{Net\\ Migration}{Emp_{resident}}$",
    
    "net_migration_share_workplace_emp" =
      "$\\frac{Net\\ Migration}{Emp_{workplace}}$",
    
    "net_migration_share_population" =
      "$\\frac{Net\\ Migration}{Population}$",
    
    "net_migration_share_population_t0" =
      "$\\frac{Net\\ Migration}{Population_{t_0}}$"
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
        "/../figures/quantile_",
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
    "w_net_migration_share_resident_emp",
    "w_net_migration_share_workplace_emp",
    
    # Employment / population
    "w_d_resident_emp_population_ratio",
    "w_d_workplace_emp_population_ratio",
    
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
    "w_net_migration_share_population",
    "w_net_migration_share_population_t0"
  )
  
  mods_testing <- baseline(
    dep_vars,
    "w_migration"
  )
  
  dep_vars <- c(
    
    # Services / commuting
    "net_migration_share_resident_emp",
    "net_migration_share_workplace_emp",
    "net_migration_share_population",
    "net_migration_share_population_t0"
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

  map_dt <- merge(
    counties,
    reg,
    by = "area_fips",
    all.x = TRUE
  )

  
  ggplot(map_dt) +
    geom_sf(
      aes(fill = diff_workplace_resident),
      color = "grey70",
      linewidth = 0.1
    ) +
    coord_sf(
      xlim = c(-90, -80),
      ylim = c(34, 50)
    ) +
    scale_fill_gradient2(
      low = "red",
      mid = "green",
      high = "blue",
      midpoint = 0
    ) +
    theme_void()
  
  
  
  exit 
  
  ggplot(map_dt) +
    geom_sf(
      aes(fill = w_net_migration_share_population_t0),
      color = "grey70",
      linewidth = 0.1
    ) +
    coord_sf(
      xlim = c(-125, -66),
      ylim = c(24, 50)
    ) +
    theme_void()

  ggplot(map_dt) +
    geom_sf(
      aes(fill = w_net_migration_share_population_t0),
      color = "grey70",
      linewidth = 0.1
    ) +
    coord_sf(
      xlim = c(-90, -80),
      ylim = c(34, 50)
    ) +
    scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "blue",
      midpoint = 0
    ) +
    theme_void()
  
  
  ggplot(map_dt[map_dt$year == 2007,]) +
    geom_sf(
      aes(fill = log_population),
      color = "grey70",
      linewidth = 0.1
    ) +
    coord_sf(
      xlim = c(-90, -80),
      ylim = c(34, 50)
    ) +
    scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "blue",
      midpoint =10
    ) +
    theme_void()
  
  
  
  
  library(patchwork)
  
  p1 <- ggplot(map_dt) +
    geom_sf(
      aes(fill = d_outside_jobs_share_goods_jobs),
      color = "grey70",
      linewidth = 0.1
    ) +
    coord_sf(
      xlim = c(-90, -80),
      ylim = c(34, 50)
    ) +
    scale_fill_gradient2(
      low = "red",
      mid = "green",
      high = "blue",
      midpoint = 0
    ) +
    theme_void()
  
  
  p2 <- ggplot(map_dt[map_dt$year == 2007,]) +
    geom_sf(
      aes(fill = log_population),
      color = "grey70",
      linewidth = 0.1
    ) +
    coord_sf(
      xlim = c(-90, -80),
      ylim = c(34, 50)
    ) +
    scale_fill_gradient2(
      low = "red",
      mid = "green",
      high = "blue",
      midpoint = 10
    ) +
    theme_void()
  
  
  p1 + p2