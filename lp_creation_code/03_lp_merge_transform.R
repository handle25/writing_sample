################################################################################
# Created 8.15.2026 
# Author: Sophie Handley 
# Purpose: Construct regression outcomes and merge QCEW, LODES, IRS, population
################################################################################


rm(list = ls())

# qcewdata 
path <- "D:/writing_sample/data"
setwd(path)

################################################################################
# Population
################################################################################

acs <- fread(paste0(path, "/output/lp_population.csv"))
acs <- acs[year != 2010, ]


acs_1y <- 
  fread(paste0(path, "/acs/acs_1y_2005_2024_commuting.csv"))
# acs[, area_fips := as.integer(paste0(sprintf("%02.0f", state), sprintf("%03.0f", county)))]


################################################################################
# Read datasets
################################################################################

qcew <- fread(
  paste0(path, "/output/lp_weighted_qcew.csv")
)

qcew[, .(
  N = .N,
  mean_US   = mean(IPW_US, na.rm = TRUE),
  median_US = median(IPW_US, na.rm = TRUE),
  min_US    = min(IPW_US, na.rm = TRUE),
  max_US    = max(IPW_US, na.rm = TRUE),
  mean_OTH  = mean(IPW_OTH, na.rm = TRUE)
), by = year]

qcew[, area_fips_str := sprintf("%06d", area_fips)]
qcew[, state := floor(area_fips / 1000)]


# PAUSE INCOMPLETE 
# LODES ------------------------------------------------------------------------

lodes <- fread(
  paste0(
    path,
    "/output/lp_lodes_collapsed_all_no_crosswalk.csv"
  )
)

# IRS --------------------------------------------------------------------------

irs <- fread(
  paste0(path, "/irs/lp_irs_migration_full.csv")
)


################################################################################
# Merge datasets
################################################################################

reg <- merge(
  qcew,
  lodes,
  by.x = c("area_fips", "year"),
  by.y = c("county", "year"), 
  all.x = TRUE
)

nrow(reg)

reg <- merge(
  reg,
  irs,
  by = c("area_fips", "year"),
  all.x = TRUE
)

nrow(reg)

reg <- merge(
  reg,
  acs,
  by = c("area_fips", "year"),
  all.x = TRUE
)

reg <- merge(
  reg, 
  acs_1y, 
  by = c("area_fips","year"), 
  all.x = TRUE
)

setorder(
  reg,
  area_fips,
  year
)

# winsorize function 

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



################################################################################
# Rename fundamental employment concepts
################################################################################

# QCEW:
# Employment located at establishments in the county
setnames(
  reg,
  "total_emp",
  "workplace_emp"
)

# LODES:
# Employed residents of the county, regardless of workplace county
setnames(
  reg,
  "total_jobs",
  "resident_emp"
)


################################################################################
# Basic regression variables
################################################################################

# State FIPS for clustering
reg[, statefip :=
      floor(as.integer(area_fips) / 1000)]


################################################################################
# Net migration flows
################################################################################

reg[, net_migration :=
      as.integer(returns_3_inflow) -
      as.integer(returns_3_outflow)]

# Net migration relative to employed residents
reg[, net_migration_share_resident_emp :=
      net_migration / resident_emp]

reg[, net_migration_share_workplace_emp :=
      net_migration / workplace_emp]

reg[, net_migration_share_population :=
      net_migration / population]

reg[, net_migration_share_population_t0 :=
      net_migration / shift(population, type = "lag", n = 1), by = .(area_fips)]

# Winsorize net migration share -----------------------------------------------
for (v in c("net_migration_share_workplace_emp",
            "net_migration_share_resident_emp",
            "net_migration_share_population",
            "net_migration_share_population_t0", 
            "IPW_US", "IPW_OTH")){
 winsor(reg, v)
}


# Winsorize vars ---------------------------------------------------------------
################################################################################
# Employment-to-population ratios
################################################################################

# Employment of county residents / county population
reg[, resident_emp_population_ratio :=
      resident_emp / population]

# Employment located in county / county population
reg[, workplace_emp_population_ratio :=
      workplace_emp / population]

winsor(reg, "resident_emp_population_ratio")
winsor(reg, "workplace_emp_population_ratio")

################################################################################
# Construct level shares for LP outcomes
################################################################################

level_vars <- c(
  grep("^outside.*_jobs$", names(reg), value = TRUE),
  grep("^total.*_jobs$", names(reg), value = TRUE),
  grep("^inside.*_jobs$", names(reg), value = TRUE)
)

level_vars <- unique(level_vars)

for (i in level_vars) {
  
  # Resident employment
  reg[, (paste0(i, "_share_resident_emp")) :=
        get(i) / resident_emp]
  
  # Workplace employment
  reg[, (paste0(i, "_share_workplace_emp")) :=
        get(i) / workplace_emp]
  
  # Outside employment
  reg[, (paste0(i, "_share_outside_jobs")) :=
        get(i) / outside_jobs]
  
  # Resident service employment
  reg[, (paste0(i, "_share_service_jobs")) :=
        get(i) / total_servc_jobs]
  
  # Resident goods employment
  reg[, (paste0(i, "_share_goods_jobs")) :=
        get(i) / total_goods_jobs]
  
  # Population
  reg[, (paste0(i, "_share_population")) :=
        get(i) / population]
}


################################################################################
# Winsorize level shares at 1st / 99th percentiles
################################################################################

share_vars <- grep(
  "_share_(resident_emp|workplace_emp|outside_jobs|service_jobs|goods_jobs|population)$",
  names(reg),
  value = TRUE
)

for (v in share_vars) {
  winsor(reg, v)
}

reg[, workplace_emp_share_resident_emp := workplace_emp / resident_emp]

################################################################################
# Manufacturing employment relative to resident employment
################################################################################

reg[, manuf_share_emp :=
      manufac_emp / workplace_emp]

reg[, manuf_emp_share_pop :=
      manufac_emp / population]

winsor(reg, "manuf_share_emp")
winsor(reg, "manuf_emp_share_pop")



################################################################################
# Save
################################################################################
# Duplicate county-years?
reg[, .N, by = .(area_fips, year)][N > 1]

# Sample coverage
reg[, .(
  N = .N,
  counties = uniqueN(area_fips),
  min_year = min(year, na.rm = TRUE),
  max_year = max(year, na.rm = TRUE)
)]

# Missingness in variables used by the LP
reg[, .(
  miss_mfg = sum(is.na(w_manuf_share_emp)),
  miss_mfg_pop = sum(is.na(w_manuf_emp_share_pop)),
  miss_US = sum(is.na(w_IPW_US)),
  miss_OTH = sum(is.na(w_IPW_OTH)),
  miss_control = sum(is.na(l_shind_manuf))
)]
fwrite(
  reg,
  paste0(
    path,
    "/output/lp_transformed_reg.csv"
  )
)