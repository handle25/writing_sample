################################################################################
# Regressions 
################################################################################
path <- "D:/writing_sample/data"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
reg <- fread(paste0(path, "/output/transformed_reg.csv"))
date <- Sys.Date()


winsor <- function(dt, var, p = 0.01) {
  q <- quantile(dt[[var]], probs = c(p, 1 - p), na.rm = TRUE)
  
  w_var <- paste0("w_", var)
  
  dt[, (w_var) := pmin(pmax(get(var), q[1]), q[2])]
}

setorder(reg, area_fips, year)

reg07 <- reg[year %in% c(2000, 2007)]
reg13 <- reg[year %in% c(2007, 2013)]
reg25 <- reg[year %in% c(2019, 2025)]

reg07[, t2 := as.integer(year == 2007)]
reg13[, t2 := as.integer(year == 2013)]
reg25[, t2 := as.integer(year == 2025)]

reg19 <- reg[year %in% c(2013, 2019)]
reg19[, `:=`(
  lag_IPW_US = w_IPW_US[year == 2013][1],
  lag_IPW_OTH = w_IPW_OTH[year == 2013][1]
), by=area_fips]

# Variable labels
names_dict <- c(
  # Regressors ----------------------------------------------------------------
  "fit_IPW_US" = "Import Exposure",
  "w_fit_IPW_US" = "Import Exposure",
  "w_fit_IPW_US_wap" = "Import Exposure",
  "w_IPW_US" = "Import Exposure",
  "w_IPW_US_wap" = "Import Exposure",
  "w_IPW_US_10yr" = "Import Exposure",
  "t2" = "Period 2",
  
  # Manufacturing / income ----------------------------------------------------
  "d_sh_empl_mfg" = "$\\Delta \\frac{Manufacturing}{Employment}$",
  "d_ln_agi_per_return" = "$\\Delta \\log(AGI\\ per\\ Return)$",
  
  # Commuting -----------------------------------------------------------------
  "w_d_outside_jobs_share_labor_force" =
    "$\\Delta \\frac{Outside\\ Jobs}{Labor\\ Force}$",
  "w_d_outside_jobs_share_population" =
    "$\\Delta \\frac{Outside\\ Jobs}{Population}$",
  
  # Migration -----------------------------------------------------------------
  "w_d_returns_3_outflow_share_returns" =
    "$\\Delta \\frac{Migration^{Out}_{Returns}}{Nonmigrant\\ Returns}$",
  "w_d_returns_3_outflow_share_population" =
    "$\\Delta \\frac{Migration^{Out}_{Returns}}{Population}$",
  "w_d_exemptions_3_outflow_share_exemptions" =
    "$\\Delta \\frac{Migration^{Out}_{Exemptions}}{Nonmigrant\\ Exemptions}$",
  "w_d_returns_3_outflow_share_returns_migration" =
    "$\\Delta \\frac{Migration^{Out}_{Returns}}{Migration^{Out}_{Returns}+Migration^{In}_{Returns}}$",
  "w_d_exemptions_3_outflow_share_exemptions_migration" =
    "$\\Delta \\frac{Migration^{Out}_{Exemptions}}{Migration^{Out}_{Exemptions}+Migration^{In}_{Exemptions}}$",
  
  # Labor market --------------------------------------------------------------
  "w_d_labor_force_share_population" =
    "$\\Delta \\frac{Labor\\ Force}{Population}$",
  "w_d_unemployed_share_labor_force" =
    "$\\Delta \\frac{Unemployed}{Labor\\ Force}$",
  # Migration -------------------------------------------------------------------
  "w_d_returns_3_outflow_share_returns" =
    "$\\Delta \\frac{Migration^{Out}_{Returns}}{Nonmigrant\\ Returns}$",
  "w_d_exemptions_3_outflow_share_exemptions" =
    "$\\Delta \\frac{Migration^{Out}_{Exemptions}}{Nonmigrant\\ Exemptions}$",
  
  "w_d_returns_3_outflow_share_returns_migration" =
    "$\\Delta \\frac{Migration^{Out}_{Returns}}{Migration^{Out}_{Returns}+Migration^{In}_{Returns}}$",
  "w_d_exemptions_3_outflow_share_exemptions_migration" =
    "$\\Delta \\frac{Migration^{Out}_{Exemptions}}{Migration^{Out}_{Exemptions}+Migration^{In}_{Exemptions}}$",
  
  "w_d_exemptions_net_migration_share_population" =
    "$\\Delta \\frac{Migration^{In}_{Exemptions}-Migration^{Out}_{Exemptions}}{Population}$",
  "w_d_returns_net_migration_share_population" =
    "$\\Delta \\frac{Migration^{In}_{Returns}-Migration^{Out}_{Returns}}{Population}$"
)

################################################################################
# Baseline models
################################################################################


baseline <- function(
    dep_vars,
    desc,
    shock_us = "w_IPW_US",
    shock_oth = "w_IPW_OTH", 
    control_table = F, 
    drop = c("Constant", "sh_popfborn", "sh_popedu_c", "l_sh_empl_mfg","sh_empl_f")
) {
  
  reg <- reg07 
  reg[, t2 := as.integer(year == 2007)]
  mods07 <- lapply(dep_vars, function(y) {
    
    fml <- as.formula(
      paste0(y, " ~ t2 ", " | ", shock_us, " ~ ", shock_oth)
    )
    feols(
      fml,
      data = reg[year %in% c(2000, 2007)],
      weights = ~baseline_emp,
      cluster = ~statefip
    )
  })
  
  mods07_lsh <- lapply(dep_vars, function(y) {
    
    fml <- as.formula(
      paste0(y, " ~  t2 + l_sh_empl_mfg", " | ", shock_us, " ~ ", shock_oth)
    )
    feols(
      fml,
      data = reg[year %in% c(2000, 2007)],
      weights = ~baseline_emp,
      cluster = ~statefip
    )
  })
  
  mods07_ctl <- lapply(dep_vars, function(y) {
    
    fml <- as.formula(
      paste0(y, " ~  t2  + l_sh_empl_mfg + sh_popfborn + sh_popedu_c + sh_empl_f ", " | ", shock_us, " ~ ", shock_oth)
    )
    feols(
      fml,
      data = reg[year %in% c(2000, 2007)],
      weights = ~baseline_emp,
      cluster = ~statefip
    )
  })
  
  reg <- reg13
  reg[, t2 := as.integer(year == 2013)]
  mods13 <- lapply(dep_vars, function(y) {
    
    fml <- as.formula(
      paste0(y, " ~  t2 ", " | ", shock_us, " ~ ", shock_oth)
    )
    feols(
      fml,
      data = reg[year %in% c(2007, 2013)],
      weights = ~baseline_emp,
      cluster = ~statefip
    )
  })
  mods13_lsh <- lapply(dep_vars, function(y) {
    reg[, t2 := as.integer(year == 2013)]
    fml <- as.formula(
      paste0(y, " ~  t2  + l_sh_empl_mfg ", " | ", shock_us, " ~ ", shock_oth)
    )
    feols(
      fml,
      data = reg[year %in% c(2007, 2013)],
      weights = ~baseline_emp,
      cluster = ~statefip
    )
  })
  mods13_ctl <- lapply(dep_vars, function(y) {
    reg[, t2 := as.integer(year == 2013)]
    fml <- as.formula(
      paste0(y, " ~  t2  + l_sh_empl_mfg + sh_popfborn + sh_popedu_c + sh_empl_f  ", " | ", shock_us, " ~ ", shock_oth)
    )
    feols(
      fml,
      data = reg[year %in% c(2007, 2013)],
      weights = ~baseline_emp,
      cluster = ~statefip
    )
  })
  
  
  reg <- reg19
  mods19 <- lapply(dep_vars, function(y) {
    
    fml <- as.formula(
      paste0(y, " ~  1  ", " | lag_IPW_US ~ lag_IPW_OTH")
    )
    feols(
      fml,
      data = reg[year %in% c(2019)],
      weights = ~baseline_emp,
      cluster = ~statefip
    )
  })
  mods19_lsh <- lapply(dep_vars, function(y) {
    fml <- as.formula(
      paste0(y, " ~  1+  l_sh_empl_mfg | lag_IPW_US ~ lag_IPW_OTH")
    )
    feols(
      fml,
      data = reg[year %in% c(2019, 2019)],
      weights = ~baseline_emp,
      cluster = ~statefip
    )
  })
  mods19_ctl <- lapply(dep_vars, function(y) {
    fml <- as.formula(
      paste0(y, " ~ 1 +  l_sh_empl_mfg + sh_popfborn + sh_popedu_c + sh_empl_f | lag_IPW_US ~ lag_IPW_OTH")
    )
    feols(
      fml,
      data = reg[year %in% c(2019, 2019)],
      weights = ~baseline_emp,
      cluster = ~statefip
    )
  })
  
  mods <- c(mods07_ctl, mods13_ctl)
  names(mods) <- c(dep_vars, dep_vars)
  file <- paste0(path, "/../figures/final_", desc,"_", shock_us, "_", date, ".tex")
  
  etable(
    mods,
    dict = names_dict,
    drop = drop, 
    extralines = list(
      "\\midrule Baseline manufacturing share" = c("$\\checkmark$", "$\\checkmark$"),
      "Baseline demographic controls " = c("$\\checkmark$", "$\\checkmark$")
    ),
    digits = 3,
    style.tex = style.tex(
      var.title = "",
      fixef.title = "",
      stats.title = "",
      notes.tpt.intro = ""
    ),
    headers = list(
      "2000--2007" = 1,
      "2007--2013" = 1
    ),
    tex = TRUE,
    file = file,
    replace = TRUE, 
    notes = ""
  )
  x <- readLines(file)
  
  x <- sub("Dependent Variable:", "", x, fixed = TRUE)
  x <- sub("Model:", "", x, fixed = TRUE)
  x <- x[!grepl("standard-errors in parentheses", x)]
  x <- x[!grepl("Signif\\. Codes", x)]
  
  i <- grep("2000--2007.*2013", x)
  x <- append(x, "\\midrule", after = i+1)
  
  writeLines(x, file)
  
  invisible(mods)
  
  # all models 
  if (control_table == T){
  mods <- c(mods07,mods07_lsh,mods07_ctl, mods13,mods13_lsh,mods13_ctl)
  names(mods) <- c(dep_vars, dep_vars, dep_vars, dep_vars, dep_vars, dep_vars)
  file <- paste0(path, "/../figures/final_controls_", desc,"_", shock_us, "_", date, ".tex")
  
  etable(
    mods,
    dict = names_dict,
    drop = drop,
    extralines = list(
      "\\midrule Baseline manufacturing share" =
        c("", "$\\checkmark$", "$\\checkmark$",
          "", "$\\checkmark$", "$\\checkmark$"),
      "Baseline demographic controls " =
        c("", "", "$\\checkmark$",
          "", "", "$\\checkmark$")
    ),
    digits = 3,
    style.tex = style.tex(
      var.title = "",
      fixef.title = "",
      stats.title = "",
      notes.tpt.intro = ""
    ),
    headers = list(
      "2000--2007" = 3,
      "2007--2013" = 3
    ),
    tex = TRUE,
    file = file,
    replace = TRUE, 
    notes = ""
  )
  
  x <- readLines(file)
  
  x <- sub("Dependent Variable:", "", x, fixed = TRUE)
  x <- sub("Model:", "", x, fixed = TRUE)
  x <- x[!grepl("standard-errors in parentheses", x)]
  x <- x[!grepl("Signif\\. Codes", x)]
  
  i <- grep("2000--2007.*2013", x)
  x <- append(x, "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}", after = i)
  x <- append(x, "\\midrule", after = i+2)
  
  writeLines(x, file)
  }
  invisible(mods)
}

# first figure -----------------------------------------------------------------
baseline(
  "d_sh_empl_mfg",
  "d_sh_empl_mfg"
)

baseline(
  "d_sh_empl_mfg",
  "d_sh_empl_mfg", 
  shock_us = "w_IPW_US_wap",
  shock_oth = "w_IPW_OTH_wap"
)


baseline(
  "d_sh_empl_mfg",
  "d_sh_empl_mfg", 
  shock_us = "w_fn_mean_dest_IPW_US",
  shock_oth = "w_fn_mean_dest_IPW_OTH"
)
baseline(
  "d_sh_empl_mfg",
  "d_sh_empl_mfg", 
  shock_us = "w_IPW_US+w_ex_mean_dest_IPW_US",
  shock_oth = "w_IPW_OTH+w_ex_mean_dest_IPW_OTH"
)

baseline(
  "d_ln_agi_per_return",
  "d_ln_agi_per_return"
)


# second figure ----------------------------------------------------------------

baseline(
  "w_d_outside_jobs_share_labor_force",
  "w_d_outside_jobs_share_labor_force",
  shock_us = "w_IPW_US_10yr",
  shock_oth = "w_IPW_OTH_10yr"
)

baseline(
  "w_d_outside_jobs_share_labor_force",
  "w_d_outside_jobs_share_labor_force",
  shock_us = "w_IPW_US_10yr",
  shock_oth = "w_IPW_OTH_10yr"
)


baseline(
  "w_d_outside_jobs_share_labor_force",
  "w_d_outside_jobs_share_labor_force",
  shock_us = "w_IPW_US_10yr + w_ex_mean_dest_IPW_US",
  shock_oth = "w_IPW_OTH_10yr + w_ex_mean_dest_IPW_OTH"
)

baseline(
  "w_d_outside_jobs_share_labor_force",
  "w_d_outside_jobs_share_labor_force",
  shock_us = "w_ex_mean_dest_IPW_US",
  shock_oth = "w_ex_mean_dest_IPW_OTH"
)

baseline(
  "w_d_outside_jobs_share_labor_force",
  "w_d_outside_jobs_share_labor_force",
  shock_us = "w_IPW_US_10yr",
  shock_oth = "w_IPW_OTH_10yr"
)


baseline(
  "w_d_outside_jobs_share_labor_force",
  "w_d_outside_jobs_share_labor_force",
  shock_us = "w_fn_mean_dest_IPW_US",
  shock_oth = "w_fn_mean_dest_IPW_OTH"
)

baseline(
  "w_d_outside_jobs_share_population",
  "w_d_outside_jobs_share_population",
  shock_us = "w_IPW_US_10yr",
  shock_oth = "w_IPW_OTH_10yr"
)

# third figure -----------------------------------------------------------------
baseline(
  "w_d_returns_3_outflow_share_returns",
  "w_d_returns_3_outflow_share_returns"
)

baseline(
  "w_d_exemptions_3_outflow_share_exemptions_migration",
  "w_d_exemptions_3_outflow_share_exemptions_migration"
)
baseline(
  "w_d_exemptions_3_outflow_share_exemptions",
  "w_d_exemptions_3_outflow_share_exemptions"
)

# Net migration / population ---------------------------------------------------
baseline(
  "w_d_exemptions_net_migration_share_population",
  "w_d_exemptions_net_migration_share_population"
)
baseline(
  "w_d_exemptions_net_migration_share_exemptions",
  "w_d_exemptions_net_migration_share_exemptions"
)
baseline(
  "w_d_returns_net_migration_share_population",
  "w_d_returns_net_migration_share_population"
)

# Flows / Migration -------------- ---------------------------------------------
baseline(
  "w_d_returns_3_outflow_share_returns_migration",
  "w_d_returns_3_outflow_share_returns_migration"
)

baseline(
  "w_d_exemptions_3_outflow_share_exemptions_migration",
  "w_d_exemptions_3_outflow_share_exemptions_migration"
)


# Labor force ------------------------------------------------------------------
baseline(
  "w_d_labor_force_share_population",
  "w_d_labor_force_share_population"
)

baseline(
  "w_d_unemployed_share_labor_force",
  "w_d_unemployed_share_labor_force"
)

