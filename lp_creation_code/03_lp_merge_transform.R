################################################################################
# Created 8.15.2026 
# Author: Sophie Handley 
# Purpose: Construct regression outcomes and merge QCEW, LODES, IRS, population
################################################################################


rm(list = ls())

# qcewdata 
path <- "D:/writing_sample/data"
setwd(path)

# define functions -------------------------------------------------------------
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

make_share_diff <- function(dt, num, denom, id = "area_fips") {
  
  share_var <- paste0(num, "_share_", denom)
  diff_var  <- paste0("d_", share_var)
  
  dt[, (share_var) := get(num) / get(denom)]
  
  dt[, (diff_var) :=
       get(share_var) -
       shift(get(share_var), 1),
     by = id]
  
  winsor(dt, diff_var)
}

diff_denom_all <- function(dt, var) { 
  make_share_diff(dt, var, "resident_emp")
  make_share_diff(dt, var, "workplace_emp")
  make_share_diff(dt, var, "outside_jobs")
  make_share_diff(dt, var, "total_servc_jobs")
  make_share_diff(dt, var, "total_goods_jobs")
  make_share_diff(dt, var, "population")
}

################################################################################
# Population
################################################################################

acs <- fread(paste0(path, "/acs/population_1995_2023.csv"))

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
cols <- names(irs)[sapply(irs, is.character)]
irs[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

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

# State FIPS for clustering
reg[, statefip :=
      floor(as.integer(area_fips) / 1000)]

################################################################################
# Basic regression variables
################################################################################

reg[, t2 := as.integer(year == 2013)]

# State FIPS for clustering
reg[, statefip :=
      floor(as.integer(area_fips) / 1000)]


################################################################################
# Net migration flows
################################################################################

reg[, net_migration := (returns_3_inflow) - (returns_3_outflow)]

# Net migration relative to employed residents
diff_denom_all(reg, "net_migration")
diff_denom_all(reg, "resident_emp")
diff_denom_all(reg, "manufac_emp")

################################################################################
# Employment-to-population ratios
################################################################################

# Employment of county residents / county population
make_share_diff(reg, "resident_emp", "population")
make_share_diff(reg, "workplace_emp", "population")
make_share_diff(reg, "workplace_emp", "resident_emp")


################################################################################
# Variables to difference in levels
################################################################################

level_vars <- c(
  grep("^outside.*_jobs$", names(reg), value = TRUE),
  grep("^total.*_jobs$", names(reg), value = TRUE),
  grep("^inside.*_jobs$", names(reg), value = TRUE)
)

level_vars <- unique(level_vars)

for (i in level_vars) {
  diff_denom_all(reg, i)
}

winsor(reg, "IPW_US") 
winsor(reg, "IPW_OTH") 

# net migration are already flows, no need to difference. 
winsor(reg, "net_migration_share_resident_emp")
winsor(reg, "net_migration_share_workplace_emp")
winsor(reg, "net_migration_share_population")


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