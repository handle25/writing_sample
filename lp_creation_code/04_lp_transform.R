################################################################################
# Created 9.19.2026 
# Author: Sophie Handley 
# Purpose: 
################################################################################

# Basic regression variables ---------------------------------------------------
reg <- fread(paste0(path, "/output/lp_merged.csv"))

reg <- make_base_year(reg, "population", 2000)
reg <- make_base_year(reg, "population", 2007)
reg <- make_base_year(reg, "workplace_emp", 2000)
reg <- make_base_year(reg, "resident_emp", 2000)

# Net migration
reg[, resident_workplace_emp_gap := resident_emp - workplace_emp]
reg[, l_resident_workplace_emp_gap := log(resident_workplace_emp_gap)]

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
make_share(reg, "returns_3_outflow", "returns_total_migration")

# reg[, net_migration :=
#       exemptions_3_inflow - exemptions_3_outflow]
# 
# reg[, net_outmigration := exemptions_3_outflow / (exemptions_3_outflow + exemptions_3_inflow) * 100]

# # Standard contemporaneous denominators
# make_share(reg, "net_migration", "resident_emp")
# make_share(reg, "net_migration", "workplace_emp")
# make_share(reg, "net_migration", "population")
# 
# # Preferred LP migration rate:
# # current migration flow / population immediately before period t
# make_share(reg, "net_migration", "population_2007")
# make_share(reg, "net_migration", "population_2000")

# Manufacturing employment shares

make_share(reg, "manufac_emp", "resident_emp")
make_share(reg, "manufac_emp", "workplace_emp")
make_share(reg, "manufac_emp", "population")
make_share(reg, "manufac_emp", "population_2000")
make_share(reg, "manufac_emp", "workplace_emp_2000")
make_share(reg, "manufac_emp", "resident_emp_2000")

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


shock_vars <- c(
  "IPW_US", "IPW_OTH",
  "IPW_US_pop", "IPW_OTH_pop",
  "IPW_US_wap", "IPW_OTH_wap",
  grep("^(ex|fn)_.*IPW_(US|OTH)$", names(reg), value=TRUE)
)

shock_vars <- unique(shock_vars)

for (i in shock_vars) winsor(reg, i)

# Winsorize shift-share variables
winsor(reg, "IPW_US")
winsor(reg, "IPW_OTH")
winsor(reg, "IPW_US_pop")
winsor(reg, "IPW_OTH_pop")
winsor(reg, "IPW_US_wap")
winsor(reg, "IPW_OTH_wap")
winsor(reg, "resident_workplace_emp_gap")
winsor(reg, "l_resident_workplace_emp_gap")


# Save
fwrite(
  reg,
  paste0(path, "/output/lp_transformed_reg.csv"))

fwrite(
  reg,
  paste0(local, "/output/lp_transformed_reg.csv"))

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
