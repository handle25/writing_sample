################################################################################
# Created 9.19.2026 
# Author: Sophie Handley 
# Purpose: 
################################################################################
rm(list = ls())

# Paths ------------------------------------------------------------------------

local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
path  <- "D:/writing_sample/data"
setwd(path)

source(paste0(local, "/../code/writing_sample/utilities.R"))

# Basic regression variables ---------------------------------------------------
reg <- fread(paste0(path, "/output/lp_merged.csv"))

reg <- make_base_year(reg, "population", 2000)
reg <- make_base_year(reg, "population", 2007)
reg <- make_base_year(reg, "workplace_emp", 2000)
reg <- make_base_year(reg, "resident_emp", 2000)

# Migration --------------------------------------------------------------------
reg[, exemptions_net_migration := exemptions_3_inflow - exemptions_3_outflow]
reg[, returns_net_migration := returns_3_inflow - returns_3_outflow]
reg[, exemptions_total_migration := exemptions_3_inflow + exemptions_3_outflow]
reg[, returns_total_migration := returns_3_inflow + returns_3_outflow]

# Migration shares
make_share(reg, "exemptions_net_migration", "population")
make_share(reg, "returns_net_migration", "population")
make_share(reg, "exemptions_net_migration", "exemptions")
make_share(reg, "returns_net_migration", "returns")

# Direction of migration
make_share(reg, "exemptions_3_outflow", "exemptions_total_migration")
make_share(reg, "exemptions_3_outflow", "exemptions_net_migration")
make_share(reg, "returns_3_outflow", "returns_total_migration")

# Manufacturing employment shares
make_share(reg, "manufac_emp", "resident_emp")
make_share(reg, "manufac_emp", "workplace_emp")
make_share(reg, "manufac_emp", "population")

# Employment-to-population / resident-workplace ratios

make_share(reg, "resident_emp", "population")
make_share(reg, "workplace_emp", "population")
make_share(reg, "workplace_emp", "resident_emp")
make_share(reg, "unemployed", "labor_force")
make_share(reg, "labor_force", "population")

# LODES employment composition

level_vars <- c(
  grep("^outside.*_jobs$", names(reg), value = TRUE),
  grep("^total.*_jobs$", names(reg), value = TRUE),
  grep("^inside.*_jobs$", names(reg), value = TRUE)
)

level_vars <- unique(level_vars)

for (i in level_vars) {
  share_denom_all(reg, i)
}

# Winsorize shift-share variables
shock_vars <- c(
  "IPW_US", "IPW_OTH",
  "IPW_US_pop", "IPW_OTH_pop",
  "IPW_US_wap", "IPW_OTH_wap",
  grep("^(ex|fn)_.*IPW_(US|OTH)$", names(reg), value=TRUE)
)

shock_vars <- unique(shock_vars)

for (i in shock_vars) winsor(reg, i)
# Network shocks only
network_vars <- grep("^(ex|fn)_",shock_vars,value=TRUE)

# Carry 1990 network values through 1999; 2000 through 2001
reg[, (network_vars) := lapply(.SD,function(x) {
  y <- nafill(x,type="locf")
  fifelse(year %between% c(1991,2001),y,x)
}), by=area_fips,.SDcols=network_vars]

# Winsorize all shocks
for(i in shock_vars) winsor(reg,i)

for(v in network_vars) {
  reg[year %between% c(1991,1999), (v) := reg[year==1990, get(v)][match(area_fips,reg[year==1990,area_fips])]]
  reg[year==2001, (v) := reg[year==2000, get(v)][match(area_fips,reg[year==2000,area_fips])]]
}

# Save
fwrite(
  reg,
  paste0(path, "/output/lp_transformed_reg.csv"))

county_conditions <- reg[, .(
  area_fips,
  year,
  IPW_US = IPW_US,
  unemployed_share_labor_force,
  sh_empl_mfg,
  labor_force_share_population
)]

fwrite(
  county_conditions,
  paste0(path, "/output/lp_shock_exposure.csv"))
