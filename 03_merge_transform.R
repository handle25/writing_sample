################################################################################
# Created 8.8.2026 
# Author: Sophie Handley 
# Purpose: Construct regression outcomes and merge QCEW, LODES, IRS, population
################################################################################

rm(list = ls())

# qcewdata 
path <- "D:/writing_sample/data"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
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


################################################################################
# Read datasets
################################################################################

qcew <- fread(
  paste0(path, "/output/weighted_qcew.csv")
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


# LODES ------------------------------------------------------------------------

lodes <- fread(
  paste0(
    path,
    "/output/lodes_collapsed_all_no_crosswalk.csv"
  )
)

lodes[
  year %in% c(2002, 2003, 2004),
  year := 2000
]


# IRS --------------------------------------------------------------------------

irs <- fread(
  paste0(path, "/irs/lp_irs_migration_full.csv")
)

cols <- setdiff(names(irs), c("area_fips", "year"))

irs[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

irs[year %in% c(2001:2006), new_year := 2007]
irs[year %in% c(2007:2012), new_year := 2013]
irs[, year := new_year]
irs <- irs |> fgroup_by(year, area_fips) |> 
  fsum()


################################################################################
# Merge datasets
################################################################################

reg <- merge(
  qcew,
  lodes,
  by.x = c("area_fips", "year"),
  by.y = c("county", "year")
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
# Net migration flows
################################################################################

reg[, net_migration := (returns_3_inflow) - (returns_3_outflow)]

# Net migration relative to employed residents
diff_denom_all(reg, "net_migration")
diff_denom_all(reg, "resident_emp")

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

fwrite(
  reg,
  paste0(
    path,
    "/output/transformed_reg.csv"
  )
)

fwrite(reg, paste0(local, "/transformed_reg.csv"))