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

################################################################################
# Variable labels
################################################################################

names_dict <- c(
  
  # Regressors -----------------------------------------------------------------
  
  "fit_IPW_US" =
    "Import Exposure",
  
  "fit_lag_IPW_US" =
    "Import Exposure",
  
  "w_fit_IPW_US" =
    "Import Exposure",
  "w_IPW_US_10yr" =
    "Import Exposure",
  
  "w_IPW_US" =
    "Import Exposure",
  
  "t2" =
    "Period 2",
  
  "l_shind_manuf" =
    "Lagged Manufacturing Share",
  # Migration / stayers ----------------------------------------------------------
  "w_d_returns_3_outflow_share_returns" =
    "$\\Delta \\frac{Migration^{Out}_{Returns}}{Nonmigrant\\ Returns}$",
  
  "w_d_exemptions_3_outflow_share_exemptions" =
    "$\\Delta \\frac{Migration^{Out}_{Exemptions}}{Nonmigrant\\ Exemptions}$",
  
  "w_d_returns_2_outflow_share_returns" =
    "$\\Delta \\frac{Migration^{Out}_{Returns}}{Nonmigrant\\ Returns}$",
  
  "w_d_exemptions_2_outflow_share_exemptions" =
    "$\\Delta \\frac{Migration^{Out}_{Exemptions}}{Nonmigrant\\ Exemptions}$",
  
  
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
  "w_d_ln_labor_force" =
    "$\\Delta \\log(Labor\\ Force)$",
  
  "w_net_migration_share_resident_emp" =
    "$\\frac{Net\\ Migration}{Emp_{resident}}$",
  
  "w_d_net_outmigration_share_exemptions" =
    "$\\Delta \\frac{Migration^{Out}-Migration^{In}}{Nonmigrant\\ Exemptions}$",
  
  
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
  
  "w_net_migration_share_population_t" =
    "$\\frac{Net\\ Migration}{Population_{t0}}$",
  
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
  
  "w_d_unemployed_share_l_labor_force"= 
    "$\\Delta \\frac{Unemployed}{Labor\\ Force_t0}$",
  
  "w_net_migration_share_population_2000"= 
    "$ \\frac{Net\\ Migration}{Population_{2000}}$",
  
  "w_net_migration_share_population_base_year"= 
    "$ \\frac{Net\\ Migration}{Population_{t0}}$",
  
  "w_d_outside_jobs_share_population"= 
    "$\\Delta \\frac{Outside\\ Jobs}{Population_{t}}$",
  "w_d_outside_jobs_share_labor_force" =
    "$\\Delta \\frac{Outside\\ Jobs}{Labor\\ Force_{t}}$",
  
  "w_d_outside_jobs_share_labor_force" =
    "$\\Delta\\frac{Outside\\ Jobs}{Labor\\ Force_{t}}$",
  
  "w_net_migration_share_population_t" =
    "$\\Delta\\frac{Net\\ Migration}{Population_{t0}}$", 
  
  "w_d_unemployment_rate" =
    "$\\Delta\\frac{Unemployed}{Labor\\ Force_{t}}$",
  "d_unemployment_rate" =
    "$\\Delta\\frac{Unemployed}{Labor\\ Force_{t}}$",
  
  "d_lfp" =
    "$\\Delta\\frac{Labor Force}{Population_{t}}$",
  "w_d_lfp" =
    "$\\Delta\\frac{Labor Force}{Population_{t}}$", 
  
  "w_d_net_outmigration" =
    "$\\Delta\\frac{Migration^{Out}_{it}}{\\sum Migration_{it}}$",
  
  "w_d_unemployed_share_labor_force_t0"= 
    "$\\Delta \\frac{Unemployed}{Labor\\ Force_t0}$"
  
)


################################################################################
# Final channel tables
################################################################################
final_table <- function(depvar1, depvar2, desc) {
  
  dep_vars <- c(depvar1, depvar2)
  
  # 2000--2007 ---------------------------------------------------------------
  mods07 <- lapply(dep_vars, function(y) {
    fml <- as.formula(
      paste0(y, " ~ t2 + l_sh_empl_mfg + sh_popfborn + sh_popedu_c + sh_empl_f | w_IPW_US ~ w_IPW_OTH")
    )
    feols(fml, data=reg07, weights=~baseline_emp, cluster=~statefip)
  })
  
  # 2007--2013 ---------------------------------------------------------------
  mods13 <- lapply(dep_vars, function(y) {
    fml <- as.formula(
      paste0(y, " ~ t2 + l_sh_empl_mfg + sh_popfborn + sh_popedu_c + sh_empl_f | w_IPW_US ~ w_IPW_OTH")
    )
    feols(fml, data=reg13, weights=~baseline_emp, cluster=~statefip)
  })
  
  # 2013--2019: effect of prior 2007--2013 exposure --------------------------
  mods19 <- lapply(dep_vars, function(y) {
    fml <- as.formula(
      paste0(y, " ~ l_sh_empl_mfg + sh_popfborn + sh_popedu_c + sh_empl_f | lag_IPW_US ~ lag_IPW_OTH")
    )
    feols(fml, data=reg19[year == 2019], weights=~baseline_emp, cluster=~statefip)
  })
  
  # Order: DV1 DV2 within each period
  mods <- c(mods07, mods13, mods19)
  names(mods) <- c(dep_vars, dep_vars, dep_vars)
  
  file <- paste0(path, "/../figures/final_channel_", desc, date, ".tex")
  
  etable(
    mods,
    dict=names_dict,
    drop=c("Constant", "sh_popfborn", "sh_popedu_c", "l_sh_empl_mfg", "sh_empl_f"),
    extralines=list(
      "\\midrule Baseline manufacturing share"=rep("$\\checkmark$", 6),
      "Baseline demographic controls"=rep("$\\checkmark$", 6)
    ),
    digits=3,
    style.tex=style.tex(
      var.title="",
      fixef.title="",
      stats.title="",
      notes.tpt.intro=""
    ),
    headers=list(
      "2000--2007"=2,
      "2007--2013"=2,
      "2013--2019"=2
    ),
    tex=TRUE,
    file=file,
    replace=TRUE,
    notes=""
  )
  
  x <- readLines(file)
  
  x <- sub("Dependent Variable:", "", x, fixed=TRUE)
  x <- sub("Model:", "", x, fixed=TRUE)
  x <- x[!grepl("standard-errors in parentheses", x)]
  x <- x[!grepl("Signif\\. Codes", x)]
  
  i <- grep("2000--2007.*2007--2013.*2013--2019", x)
  x <- append(x, "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}", after=i)
  x <- append(x, "\\midrule", after=i+2)
  
  writeLines(x, file)
  
  invisible(mods)
}

################################################################################
# Tables
################################################################################

# Migration -------------------------------------------------------------------
final_table(
  "w_d_returns_3_outflow_share_returns_migration",
  "w_d_returns_3_outflow_share_returns_migration",
  "migration_new"
)

# Add additional channel tables in the same form:
# final_table("depvar1", "depvar2", "file_name")

# Main commuting --------------------------------------------------------------
final_table(
  "w_d_outside_jobs_share_resident_emp",
  "w_d_outside_jobs_share_population",
  "commuting"
)

final_table(
  "w_d_outside_jobs_share_resident_emp",
  "w_d_workplace_emp_share_resident_emp",
  "commuting"
)
