################################################################################
# Created 8.15.2026 
# Author: Sophie Handley 
# Purpose: Construct LP regression outcomes and merge QCEW, LODES, IRS, population
################################################################################


rm(list = ls())

# Paths ------------------------------------------------------------------------

local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
path  <- "D:/writing_sample/data"

setwd(path)


################################################################################
# Functions
################################################################################

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


make_share <- function(dt, num, denom) {
  
  share_var <- paste0(num, "_share_", denom)
  
  dt[, (share_var) := get(num) / get(denom)]
  
  winsor(dt, share_var)
}


share_denom_all <- function(dt, var) {
  
  make_share(dt, var, "resident_emp")
  make_share(dt, var, "workplace_emp")
  make_share(dt, var, "outside_jobs")
  make_share(dt, var, "total_servc_jobs")
  make_share(dt, var, "total_goods_jobs")
  make_share(dt, var, "population")
  make_share(dt, var, "population_2000")
  make_share(dt, var, "population_2007")
}


################################################################################
# Population
################################################################################

acs <- fread(
  paste0(path, "/acs/population_1995_2023.csv")
)

acs_1y <- fread(
  paste0(path, "/acs/acs_1y_2005_2024_commuting.csv")
)


################################################################################
# QCEW
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


################################################################################
# LODES
################################################################################

lodes <- fread(
  paste0(
    path,
    "/output/lp_new_lodes_collapsed_all_no_crosswalk.csv"
  )
)


################################################################################
# IRS
################################################################################

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
  by = c("area_fips", "year"),
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


################################################################################
# Basic regression variables
################################################################################

reg[, t2 := as.integer(year == 2013)]

# State FIPS for clustering
reg[, statefip :=
      floor(as.integer(area_fips) / 1000)]


################################################################################
# Predetermined population
################################################################################

reg_t_1 <- copy(reg) 
reg_t_1 <- reg_t_1[year == 2000, ] |> 
  fmutate(population_2000 = population) |> 
  fselect(area_fips, population_2000) 

reg <- merge(reg, reg_t_1, 
             by = "area_fips",
             all.x = TRUE)

reg_t_1 <- copy(reg) 
reg_t_1 <- reg_t_1[year == 2007, ] |> 
  fmutate(population_2007 = population) |> 
  fselect(area_fips, population_2007) 

reg <- merge(reg, reg_t_1, 
             by = "area_fips",
             all.x = TRUE)


################################################################################
# Net migration
################################################################################
reg[, resident_workplace_emp_gap := resident_emp - workplace_emp]
reg[, l_resident_workplace_emp_gap := log(resident_workplace_emp_gap)]

reg[, net_migration :=
      exemptions_3_inflow - exemptions_3_outflow]

reg[, net_migration_samestate :=
      exemptions_1_inflow - exemptions_1_outflow]

reg[, net_migration_diffstate :=
      exemptions_2_inflow - exemptions_2_outflow]

# Standard contemporaneous denominators
make_share(reg, "net_migration", "resident_emp")
make_share(reg, "net_migration", "workplace_emp")
make_share(reg, "net_migration", "population")
make_share(reg, "net_migration_samestate", "population")
make_share(reg, "net_migration_diffstate", "population")

# Preferred LP migration rate:
# current migration flow / population immediately before period t
make_share(reg, "net_migration", "population_2007")
make_share(reg, "net_migration", "population_2000")
make_share(reg, "net_migration_samestate", "population_2000")
make_share(reg, "net_migration_samestate", "population_2007")
make_share(reg, "net_migration_diffstate", "population_2000")
make_share(reg, "net_migration_diffstate", "population_2007")


################################################################################
# Manufacturing employment shares
################################################################################

make_share(reg, "manufac_emp", "resident_emp")
make_share(reg, "manufac_emp", "workplace_emp")
make_share(reg, "manufac_emp", "population")
make_share(reg, "manufac_emp", "population_2000")


################################################################################
# Employment-to-population / resident-workplace ratios
################################################################################

make_share(reg, "resident_emp", "population")
make_share(reg, "workplace_emp", "population")
make_share(reg, "workplace_emp", "resident_emp")


################################################################################
# LODES employment composition
################################################################################

level_vars <- c(
  grep("^outside.*_jobs$", names(reg), value = TRUE),
  grep("^total.*_jobs$", names(reg), value = TRUE),
  grep("^inside.*_jobs$", names(reg), value = TRUE)
)

level_vars <- unique(level_vars)

for (i in level_vars) {
  share_denom_all(reg, i)
}


################################################################################
# Winsorize shift-share variables
################################################################################

winsor(reg, "IPW_US")
winsor(reg, "IPW_OTH")
winsor(reg, "IPW_US_pop")
winsor(reg, "IPW_OTH_pop")
winsor(reg, "resident_workplace_emp_gap")
winsor(reg, "l_resident_workplace_emp_gap")


################################################################################
# Diagnostics
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

# Missingness in main variables
reg[, .(
  miss_mfg =
    sum(is.na(w_manufac_emp_share_resident_emp)),
  
  miss_mfg_pop =
    sum(is.na(w_manufac_emp_share_population)),

  miss_US =
    sum(is.na(w_IPW_US)),
  
  miss_OTH =
    sum(is.na(w_IPW_OTH)),
  
  miss_control =
    sum(is.na(l_shind_manuf))
)]


################################################################################
# Save
################################################################################

fwrite(
  reg,
  paste0(
    path,
    "/output/lp_transformed_reg.csv"
  )
)

fwrite(
  reg,
  paste0(
    local,
    "/output/lp_transformed_reg.csv"
  )
)