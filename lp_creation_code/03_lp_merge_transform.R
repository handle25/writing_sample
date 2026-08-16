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
exit
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

reg[, d_net_migration_pctchange :=
      (net_migration -
         shift(net_migration, n = 1)) /
      shift(net_migration, n = 1),
    by = area_fips]

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


for (i in level_vars) {
  
  ##############################################################################
  # Denominator = resident employment
  ##############################################################################
  
  var_d_all <- paste0(
    i,
    "_share_resident_emp"
  )
  
  d_var_d_all <- paste0(
    "d_",
    i,
    "_share_resident_emp"
  )
  
  reg[, (var_d_all) :=
        get(i) / resident_emp]
  
  reg[, (d_var_d_all) :=
        get(var_d_all) -
        shift(
          get(var_d_all),
          type = "lag",
          n = 1
        ),
      by = area_fips]
  
  w_var_d_all <- paste0(
    "w_",
    d_var_d_all
  )
  
  q99 <- quantile(
    reg[[d_var_d_all]],
    .99,
    na.rm = TRUE
  )
  
  q01 <- quantile(
    reg[[d_var_d_all]],
    .01,
    na.rm = TRUE
  )
  
  reg[, (w_var_d_all) :=
        get(d_var_d_all)]
  
  reg[
    get(w_var_d_all) >= q99,
    (w_var_d_all) := q99
  ]
  
  reg[
    get(w_var_d_all) <= q01,
    (w_var_d_all) := q01
  ]
  
  ##############################################################################
  # Denominator = local workplace employment
  ##############################################################################
  
  var_d_all <- paste0(
    i,
    "_share_workplace_emp"
  )
  
  d_var_d_all <- paste0(
    "d_",
    i,
    "_share_workplace_emp"
  )
  
  reg[, (var_d_all) :=
        get(i) / workplace_emp]
  
  reg[, (d_var_d_all) :=
        get(var_d_all) -
        shift(
          get(var_d_all),
          type = "lag",
          n = 1
        ),
      by = area_fips]
  
  w_var_d_all <- paste0(
    "w_",
    d_var_d_all
  )
  
  q99 <- quantile(
    reg[[d_var_d_all]],
    .99,
    na.rm = TRUE
  )
  
  q01 <- quantile(
    reg[[d_var_d_all]],
    .01,
    na.rm = TRUE
  )
  
  reg[, (w_var_d_all) :=
        get(d_var_d_all)]
  
  reg[
    get(w_var_d_all) >= q99,
    (w_var_d_all) := q99
  ]
  
  reg[
    get(w_var_d_all) <= q01,
    (w_var_d_all) := q01
  ]
  
  
  ##############################################################################
  # Denominator = outside employment
  ##############################################################################
  
  var_d_out <- paste0(
    i,
    "_share_outside_jobs"
  )
  
  d_var_d_out <- paste0(
    "d_",
    i,
    "_share_outside_jobs"
  )
  
  reg[, (var_d_out) :=
        get(i) / outside_jobs]
  
  reg[, (d_var_d_out) :=
        get(var_d_out) -
        shift(
          get(var_d_out),
          type = "lag",
          n = 1
        ),
      by = area_fips]
  
  w_var_d_out <- paste0(
    "w_",
    d_var_d_out
  )
  
  q99 <- quantile(
    reg[[d_var_d_out]],
    .99,
    na.rm = TRUE
  )
  
  q01 <- quantile(
    reg[[d_var_d_out]],
    .01,
    na.rm = TRUE
  )
  
  reg[, (w_var_d_out) :=
        get(d_var_d_out)]
  
  reg[
    get(w_var_d_out) >= q99,
    (w_var_d_out) := q99
  ]
  
  reg[
    get(w_var_d_out) <= q01,
    (w_var_d_out) := q01
  ]
  
  
  ##############################################################################
  # Denominator = resident service employment
  ##############################################################################
  
  var_d_svc <- paste0(
    i,
    "_share_service_jobs"
  )
  
  d_var_d_svc <- paste0(
    "d_",
    i,
    "_share_service_jobs"
  )
  
  reg[, (var_d_svc) :=
        get(i) / total_servc_jobs]
  
  reg[, (d_var_d_svc) :=
        get(var_d_svc) -
        shift(
          get(var_d_svc),
          type = "lag",
          n = 1
        ),
      by = area_fips]
  
  w_var_d_svc <- paste0(
    "w_",
    d_var_d_svc
  )
  
  q99 <- quantile(
    reg[[d_var_d_svc]],
    .99,
    na.rm = TRUE
  )
  
  q01 <- quantile(
    reg[[d_var_d_svc]],
    .01,
    na.rm = TRUE
  )
  
  reg[, (w_var_d_svc) :=
        get(d_var_d_svc)]
  
  reg[
    get(w_var_d_svc) >= q99,
    (w_var_d_svc) := q99
  ]
  
  reg[
    get(w_var_d_svc) <= q01,
    (w_var_d_svc) := q01
  ]
  
  
  ##############################################################################
  # Denominator = resident goods employment
  ##############################################################################
  
  var_d_good <- paste0(
    i,
    "_share_goods_jobs"
  )
  
  d_var_d_good <- paste0(
    "d_",
    i,
    "_share_goods_jobs"
  )
  
  reg[, (var_d_good) :=
        get(i) / total_goods_jobs]
  
  reg[, (d_var_d_good) :=
        get(var_d_good) -
        shift(
          get(var_d_good),
          type = "lag",
          n = 1
        ),
      by = area_fips]
  
  w_var_d_good <- paste0(
    "w_",
    d_var_d_good
  )
  
  q99 <- quantile(
    reg[[d_var_d_good]],
    .99,
    na.rm = TRUE
  )
  
  q01 <- quantile(
    reg[[d_var_d_good]],
    .01,
    na.rm = TRUE
  )
  
  reg[, (w_var_d_good) :=
        get(d_var_d_good)]
  
  reg[
    get(w_var_d_good) >= q99,
    (w_var_d_good) := q99
  ]
  
  reg[
    get(w_var_d_good) <= q01,
    (w_var_d_good) := q01
  ]
  
  
  ##############################################################################
  # Denominator = population
  ##############################################################################
  
  var_d_pop <- paste0(
    i,
    "_share_population"
  )
  
  d_var_d_pop <- paste0(
    "d_",
    i,
    "_share_population"
  )
  
  # Contemporaneous employment / population ratio
  reg[, (var_d_pop) :=
        get(i) / population]
  
  # Change in employment / population ratio
  reg[, (d_var_d_pop) :=
        get(var_d_pop) -
        shift(
          get(var_d_pop),
          type = "lag",
          n = 1
        ),
      by = area_fips]
  
  w_d_var_d_pop <- paste0(
    "w_",
    d_var_d_pop
  )
  
  q99 <- quantile(
    reg[[d_var_d_pop]],
    .99,
    na.rm = TRUE
  )
  
  q01 <- quantile(
    reg[[d_var_d_pop]],
    .01,
    na.rm = TRUE
  )
  
  reg[, (w_d_var_d_pop) :=
        get(d_var_d_pop)]
  
  reg[
    get(w_d_var_d_pop) >= q99,
    (w_d_var_d_pop) := q99
  ]
  
  reg[
    get(w_d_var_d_pop) <= q01,
    (w_d_var_d_pop) := q01
  ]
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