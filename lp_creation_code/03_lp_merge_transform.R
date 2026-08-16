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

acs <- rbind(
  fread(paste0(path, "/acs/co-est00int-tot.csv")), 
  fread(paste0(path, "/acs/co-est2020.csv")),
  fill = TRUE
)

acs <- melt(
  acs,
  id.vars = c("STATE", "COUNTY"),
  measure.vars = patterns("^POPESTIMATE"),
  variable.name = "year",
  value.name = "population"
) |>
  fmutate(year = as.integer(substr(year, 12, 15)))

# Drop rows created from years not covered by that source file
acs <- acs[!is.na(population)]

# Drop state totals
acs <- acs[COUNTY != 0]

# Create county FIPS
acs[, area_fips := as.integer(paste0(
  sprintf("%02d", STATE),
  sprintf("%03d", COUNTY)
))]
acs[,area_fips := as.character(as.integer(area_fips))]

# population -------------------------------------------------------------------

pop_list <- list()
for (y in c(1995:1999)){
  url <- paste0(
    "https://www2.census.gov/programs-surveys/popest/",
    "tables/1990-2000/intercensal/st-co/stch-icen", y, ".txt"
  )
  
  pop <- fread(url)
  
  names <- c("year","area_fips","age","sex","eth","population")
  
  names(pop) <- names
  pop <- pop |> 
    fgroup_by(area_fips) |> 
    fsummarize(population = fsum(population)) |> 
    fmutate(year = y) 
  
  pop_list[[length(pop_list) + 1]] <- pop
}

pop <- rbindlist(pop_list) 
acs <- rbind(acs, pop, fill = TRUE)
acs[, area_fips := as.integer(area_fips)]

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

# irs <- fread(
#   paste0(path, "/irs_migration_full.csv")
# )


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
# 
# reg <- merge(
#   reg,
#   irs,
#   by = c("area_fips", "year"),
#   all.x = TRUE
# )
# 
# nrow(reg)

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

# 
# ################################################################################
# # Net migration flows
# ################################################################################
# 
# reg[, net_migration :=
#       as.integer(returns_3_inflow) -
#       as.integer(returns_3_outflow)]
# 
# # Net migration relative to employed residents
# reg[, net_migration_share_resident_emp :=
#       net_migration / resident_emp]
# 
# reg[, net_migration_share_workplace_emp :=
#       net_migration / workplace_emp]
# 
# reg[, net_migration_share_population :=
#       net_migration / population]
# 
# reg[, net_migration_share_population_t0 :=
#       net_migration / shift(population, type = "lag", n = 1), by = .(area_fips)]
# 
# reg[, d_net_migration_pctchange :=
#       (net_migration -
#          shift(net_migration, n = 1)) /
#       shift(net_migration, n = 1),
#     by = area_fips]
# 
# 
# # Winsorize net migration share -----------------------------------------------
# for (v in c("net_migration_share_workplace_emp", 
#             "net_migration_share_resident_emp", 
#             "net_migration_share_population",
#             "net_migration_share_population_t0")){
#   w_var <- paste0("w_", v)
#   
#   q99 <- quantile(
#     reg[, get(v)],
#     .99,
#     na.rm = TRUE
#   )
#   
#   q01 <- quantile(
#     reg[, get(v)],
#     .01,
#     na.rm = TRUE
#   )
#   
#   reg[, (w_var) :=
#         get(v)]
#   
#   reg[
#     get(v) >= q99,
#     (w_var) := q99
#   ]
#   
#   reg[
#     get(v) <= q01,
#     (w_var) := q01
#   ]
#   
#   
#   
# }
# 
# 
# 
# # Winsorize change in net migration -------------------------------------------
# 
# q99 <- quantile(
#   reg[, d_net_migration_pctchange],
#   .99,
#   na.rm = TRUE
# )
# 
# q01 <- quantile(
#   reg[, d_net_migration_pctchange],
#   .01,
#   na.rm = TRUE
# )
# 
# reg[, w_d_net_migration_pctchange :=
#       d_net_migration_pctchange]
# 
# reg[
#   d_net_migration_pctchange >= q99,
#   w_d_net_migration_pctchange := q99
# ]
# 
# reg[
#   d_net_migration_pctchange <= q01,
#   w_d_net_migration_pctchange := q01
# ]


################################################################################
# Employment-to-population ratios
################################################################################

# Employment of county residents / county population
reg[, resident_emp_population_ratio :=
      resident_emp / population]

# Employment located in county / county population
reg[, workplace_emp_population_ratio :=
      workplace_emp / population]


# Differences ------------------------------------------------------------------

reg[, d_resident_emp_population_ratio :=
      resident_emp_population_ratio -
      shift(
        resident_emp_population_ratio,
        n = 1
      ),
    by = area_fips]

reg[, d_workplace_emp_population_ratio :=
      workplace_emp_population_ratio -
      shift(
        workplace_emp_population_ratio,
        n = 1
      ),
    by = area_fips]


# Winsorize resident employment / population ----------------------------------

q99 <- quantile(
  reg$d_resident_emp_population_ratio,
  .99,
  na.rm = TRUE
)

q01 <- quantile(
  reg$d_resident_emp_population_ratio,
  .01,
  na.rm = TRUE
)

reg[, w_d_resident_emp_population_ratio :=
      d_resident_emp_population_ratio]

reg[
  d_resident_emp_population_ratio >= q99,
  w_d_resident_emp_population_ratio := q99
]

reg[
  d_resident_emp_population_ratio <= q01,
  w_d_resident_emp_population_ratio := q01
]


# Winsorize workplace employment / population ---------------------------------

q99 <- quantile(
  reg$d_workplace_emp_population_ratio,
  .99,
  na.rm = TRUE
)

q01 <- quantile(
  reg$d_workplace_emp_population_ratio,
  .01,
  na.rm = TRUE
)

reg[, w_d_workplace_emp_population_ratio :=
      d_workplace_emp_population_ratio]

reg[
  d_workplace_emp_population_ratio >= q99,
  w_d_workplace_emp_population_ratio := q99
]

reg[
  d_workplace_emp_population_ratio <= q01,
  w_d_workplace_emp_population_ratio := q01
]


################################################################################
# Variables to difference in levels
################################################################################

level_vars <- c(
  grep("^outside.*_jobs$", names(reg), value = TRUE),
  grep("^total.*_jobs$", names(reg), value = TRUE),
  grep("^inside.*_jobs$", names(reg), value = TRUE)
)

level_vars <- unique(level_vars)
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
  
  w_var <- paste0("w_", v)
  
  q01 <- quantile(reg[[v]], .01, na.rm = TRUE)
  q99 <- quantile(reg[[v]], .99, na.rm = TRUE)
  
  reg[, (w_var) := get(v)]
  
  reg[get(v) <= q01, (w_var) := q01]
  reg[get(v) >= q99, (w_var) := q99]
}

reg[, workplace_emp_share_resident_emp := workplace_emp / resident_emp]
reg[, d_workplace_emp_share_resident_emp := 
      workplace_emp_share_resident_emp - shift(workplace_emp_share_resident_emp, type = "lag", n = 1), 
    by = .(area_fips)]
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