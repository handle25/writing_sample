################################################################################
# Created 8.8.2026 
# Author: Sophie Handley 
# Purpose: Weight dollar shifts in naics x year imports by labor shares with QCEW

################################################################################

rm(list = ls())

# qcewdata 
path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
setwd(path)

# read in country industry level data ------------------------------------------
# has location area_fips which is county level 
years_qcew <- c(1995, 2000, 2007, 2013)

full_qcew3_list <- vector("list", length(years_qcew))

for (i in seq_along(years_qcew)) {
  
  y <- years_qcew[i]
  
  qcew <- fread(paste0(path, "/qcew/clean/full_", y, ".csv"))
  
  # get NAICS3 level
  qcew_naics3 <- qcew[agglvl_code == 75]
  
  # collapse to county x industry x year
  qcew_naics3 <- qcew_naics3 |>
    fgroup_by(area_fips, industry_code, year) |>
    fsummarize(
      total_annual_wages = fsum(total_annual_wages),
      annual_avg_emplvl = fsum(annual_avg_emplvl),
      annual_avg_estabs_count = fsum(annual_avg_estabs_count)
    ) |>
    ungroup() |>
    data.table()
  
  full_qcew3_list[[i]] <- qcew_naics3
}

qcew_naics3 <- rbindlist(full_qcew3_list)

# trade data -> naics for naics level shock ------------------------------------ 
shock <- fread(paste0(path, "/output/Delta_M_naics3.csv"))

# Create QCEW employment weights -----------------------------------------------
# 1995 employment -> 1995-2000 shock stored at year 2000
# 2000 employment -> 2000-2007 shock stored at year 2007
# 2007 employment -> 2007-2013 shock stored at year 2013
# ------------------------------------------------------------------------------
qcew_naics3[, industry_code := as.integer(industry_code)]

qcew_base <- qcew_naics3[year %in% c(1995, 2000, 2007)]

qcew_base[, baseline_year := year]

#shift the year for weighting by beginning of period employment values 
qcew_base[year == 2007, year := 2013]
qcew_base[year == 2000, year := 2007]
qcew_base[year == 1995, year := 2000]

# Merge both trade shocks onto the same baseline employment data
qcew_rep <- merge(
  qcew_base,
  shock,
  by.x = c("year", "industry_code"),
  by.y = c("refYear", "naics3"),
  all.x = TRUE
)

# Baseline county employment
qcew_rep[, L_it := sum(annual_avg_emplvl, na.rm = TRUE),
         by = .(year, area_fips)]

# Baseline county-industry employment
qcew_rep[, L_ijt := annual_avg_emplvl]

# Baseline US employment in industry j
qcew_rep[, L_ujt := sum(annual_avg_emplvl, na.rm = TRUE),
         by = .(year, industry_code)]

# Industries with no trade match contribute zero
qcew_rep[is.na(Delta_M_US),  Delta_M_US := 0]
qcew_rep[is.na(Delta_M_OTH), Delta_M_OTH := 0]

# Same employment weights applied to both trade changes
qcew_rep[, IPW_US :=
           (L_ijt / L_ujt) * (Delta_M_US / L_it)]

qcew_rep[, IPW_OTH :=
           (L_ijt / L_ujt) * (Delta_M_OTH / L_it)]

# scale by 1000 for thousand dollars per worker hour 
qcew_rep[, IPW_US  := IPW_US / 1000]
qcew_rep[, IPW_OTH := IPW_OTH / 1000]

# Collapse across industries to county x period
instrument <- qcew_rep |>
  fgroup_by(area_fips, year) |>
  fsummarize(
    IPW_US  = fsum(IPW_US),
    IPW_OTH = fsum(IPW_OTH)
  ) |>
  data.table()
fwrite(instrument, file = paste0(path, "/output/final_ipw_naics3.csv"))
# Get employment outcome -------------------------------------------------------
# Use NAICS2 data since manufacturing is identified cleanly there
qcew_outcome <- rbindlist(full_qcew3_list)
qcew_outcome[, industry_code := as.integer(industry_code)]
qcew_outcome[,naics2:= floor(as.integer(industry_code/10))]

qcew_outcome <- qcew_outcome[
  year %in% c(1995, 2000, 2007, 2013)
]

# Total county employment
county_emp <- qcew_outcome |>
  fgroup_by(area_fips, year) |>
  fsummarize(
    total_emp = fsum(annual_avg_emplvl)
  ) |>
  data.table()

# Manufacturing county employment
county_manufac <- qcew_outcome[
  naics2 %in% c(31, 32, 33)
] |>
  fgroup_by(area_fips, year) |>
  fsummarize(
    manufac_emp = fsum(annual_avg_emplvl)
  ) |>
  data.table()

# Merge and construct manufacturing employment share
county_emp <- merge(
  county_emp,
  county_manufac,
  by = c("area_fips", "year"),
  all.x = TRUE
)

county_emp[is.na(manufac_emp), manufac_emp := 0]

county_emp[, sh_empl_mfg := manufac_emp / total_emp]


# Long differences: 1995-2000 and 2000-2007
setorder(county_emp, area_fips, year)

county_emp[, d_sh_empl_mfg :=
             sh_empl_mfg - shift(sh_empl_mfg),
           by = area_fips
]
# baseline manufacturing share control
county_emp[, l_shind_manuf :=
             shift(sh_empl_mfg),
           by = area_fips
]

# baseline employment weight as a temporary county analogue
county_emp[, baseline_emp :=
             shift(total_emp),
           by = area_fips
]

# Regressions ------------------------------------------------------------------
# Merge onto the two-period instrument

reg <- merge(
  county_emp,
  instrument,
  by = c("area_fips", "year")
)

# save file 
fwrite(reg, paste0(path, "/output/weighted_qcew.csv"))

# check regs -------------------------------------------------------------------
reg <- reg[year %in% c(2000, 2007), ]# Period indicator
reg[, t2 := as.integer(year == 2007)]
reg[, t2 := as.integer(year == 2007)]

# State FIPS for clustering
reg[, statefip := floor(as.integer(area_fips) / 1000)]

mod <- feols(
  d_sh_empl_mfg ~ t2 | IPW_US ~ IPW_OTH,
  data = reg,
  cluster = ~statefip
)
mod <- feols(
  d_sh_empl_mfg ~ as.factor(year) + l_shind_manuf |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

summary(mod, stage = 1)
summary(mod, stage = 2)

mod <- feols(
  d_sh_empl_mfg ~ t2 |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

summary(mod, stage = 1)
summary(mod, stage = 2)


