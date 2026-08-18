################################################################################
# ADH replication: Table 5
# Change in Employment, Unemployment, and Non-Employment
################################################################################

rm(list = ls())

library(data.table)
library(haven)
library(fixest)


################################################################################
# Path
################################################################################

path <- "D:/writing_sample/data/112670-V1/Public-Release-Data"

reg <- as.data.table(
  read_dta(file.path(path, "dta", "workfile_china.dta"))
)


################################################################################
# Region controls
# Stata specification uses reg*
################################################################################

region_controls <- grep(
  "^reg",
  names(reg),
  value = TRUE
)

region_controls


################################################################################
# Full controls
################################################################################

controls <- c(
  "l_shind_manuf_cbp",
  "l_sh_popedu_c",
  "l_sh_popfborn",
  "l_sh_empl_f",
  "l_sh_routine33",
  "l_task_outsource",
  region_controls,
  "t2"
)

rhs <- paste(controls, collapse = " + ")


################################################################################
# Function to run ADH IV specification
################################################################################

run_adh_iv <- function(y) {
  
  fml <- as.formula(
    paste0(
      y,
      " ~ ",
      rhs,
      " | d_tradeusch_pw ~ d_tradeotch_pw_lag"
    )
  )
  
  feols(
    fml,
    data = reg,
    weights = ~timepwt48,
    cluster = ~statefip
  )
}


################################################################################
# Table 5
#
# Columns 1-5: log changes in counts
################################################################################

m_count_mfg <- run_adh_iv("lnchg_no_empl_mfg")
m_count_nmfg <- run_adh_iv("lnchg_no_empl_nmfg")
m_count_unemp <- run_adh_iv("lnchg_no_unempl")
m_count_nilf <- run_adh_iv("lnchg_no_nilf")
m_count_ssdi <- run_adh_iv("lnchg_no_ssadiswkrs")


################################################################################
# Columns 6-10: changes in shares of working-age population
################################################################################

m_share_mfg <- run_adh_iv("d_sh_empl_mfg")
m_share_nmfg <- run_adh_iv("d_sh_empl_nmfg")
m_share_unemp <- run_adh_iv("d_sh_unempl")
m_share_nilf <- run_adh_iv("d_sh_nilf")
m_share_ssdi <- run_adh_iv("d_sh_ssadiswkrs")


################################################################################
# Display Table 5 coefficients
################################################################################

etable(
  m_count_mfg,
  m_count_nmfg,
  m_count_unemp,
  m_count_nilf,
  m_count_ssdi,
  m_share_mfg,
  m_share_nmfg,
  m_share_unemp,
  m_share_nilf,
  m_share_ssdi,
  keep = "%fit_d_tradeusch_pw",
  headers = c(
    "Mfg Emp",
    "Non-Mfg Emp",
    "Unemp",
    "NILF",
    "SSDI",
    "Mfg Share",
    "Non-Mfg Share",
    "Unemp Share",
    "NILF Share",
    "SSDI Share"
  ),
  digits = 4
)


################################################################################
# Extract exact coefficients and SEs for easy comparison with Excel
################################################################################

models <- list(
  "Mfg employment"         = m_count_mfg,
  "Non-mfg employment"    = m_count_nmfg,
  "Unemployment"          = m_count_unemp,
  "NILF"                  = m_count_nilf,
  "SSDI"                  = m_count_ssdi,
  "Mfg employment share"  = m_share_mfg,
  "Non-mfg emp share"     = m_share_nmfg,
  "Unemployment share"    = m_share_unemp,
  "NILF share"            = m_share_nilf,
  "SSDI share"            = m_share_ssdi
)

results <- rbindlist(
  lapply(names(models), function(nm) {
    
    ct <- coeftable(models[[nm]])
    
    row <- grep(
      "d_tradeusch_pw",
      rownames(ct)
    )
    
    data.table(
      outcome = nm,
      beta = ct[row, 1],
      se = ct[row, 2],
      t = ct[row, 3],
      p = ct[row, 4],
      N = nobs(models[[nm]])
    )
  })
)

print(results)