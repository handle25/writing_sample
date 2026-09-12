################################################################################
# Created 8.8.2026 
# Author: Sophie Handley 
# Purpose: Weight dollar shifts in naics x year imports by labor shares with QCEW

################################################################################

rm(list = ls())

# qcewdata 
path <- "D:/writing_sample/data"
setwd(path)

# read in country industry level data ------------------------------------------
# has location area_fips which is county level 
years_qcew <- c(1990, 1995, 2000, 2007, 2013)

qcew_naics3 <- fread(paste0(path, "/qcew/clean/new_full_qcew_1995_2025.csv"))
qcew_naics3[, area_fips := as.character(area_fips)]

# trade data -> naics for naics level shock ------------------------------------ 
# years 2000, 2007, 2013 
shock <- fread(paste0(path, "/output/Delta_M_naics3.csv"))

shock_10yr <- fread(paste0(path, "/output/Delta_M_naics3_1990_2000.csv"))

crosswalk <- read_excel("cz00eqvv1.xls") |> 
  data.table() |> 
  clean_names() |> 
  fselect(fips, commuting_zone_id_2000) |> 
  fmutate(
    fips = sprintf("%05d", as.integer(fips)),
    state = as.integer(substr(fips, 1, 2))
  ) |>
  fgroup_by(fips, commuting_zone_id_2000) |>
  fsummarize(state = flast(state))

cw_s <- copy(crosswalk) |> 
  fgroup_by(commuting_zone_id_2000) |> 
  fsummarize(state = flast(state))
crosswalk[, state := NULL ]


# population weights -----------------------------------------------------------
acs <- fread(paste0(path, "/output/lp_population.csv"))
acs[,area_fips := as.character(as.integer(area_fips))]
acs <- merge(acs, crosswalk, 
                     by.x = "area_fips", 
                     by.y = "fips",
                     all.x = TRUE)

acs <- acs |> fgroup_by(commuting_zone_id_2000, year) |> 
  fsummarize(population = fsum(population))

# Create QCEW employment weights -----------------------------------------------
# 1995 employment -> 1995-2000 shock stored at year 2000
# 2000 employment -> 2000-2007 shock stored at year 2007
# 2007 employment -> 2007-2013 shock stored at year 2013
# ------------------------------------------------------------------------------
qcew_naics3[, industry_code := as.integer(industry_code)]
# merge in crosswalk 
qcew_naics3 <- merge(qcew_naics3, 
                     crosswalk, 
                     by.x = "area_fips", 
                     by.y = "fips",
                     all.x = TRUE)

# collapse qcew to CZ level 
qcew_naics3 <- qcew_naics3 |>
  fgroup_by(year, industry_code, commuting_zone_id_2000) |>
  fsummarize(
    annual_avg_emplvl = fsum(annual_avg_emplvl, na.rm = TRUE),
    annual_avg_estabs_count = fsum(annual_avg_estabs_count, na.rm = TRUE),
    total_annual_wages = fsum(total_annual_wages, na.rm = TRUE)
  ) |>
  data.table()

# create industry shares -------------------------------------------------------

# Total employment in county
qcew_naics3[
  ,
  workplace_emp :=
    sum(annual_avg_emplvl, na.rm = TRUE),
  by = .(commuting_zone_id_2000, year)
]

# Industry employment share
qcew_naics3[
  ,
  industry_share :=
    annual_avg_emplvl / workplace_emp
]

# Overall industry concentration
county_concentration <- qcew_naics3[
  ,
  .(
    industry_hhi =
      sum(industry_share^2, na.rm = TRUE),
    
    industry_share_sd =
      sd(industry_share, na.rm = TRUE)
  ),
  by = .(commuting_zone_id_2000, year)
]


# Nonmanufacturing concentration ----------------------------------------------

# Keep nonmanufacturing industries only
county_concentration_nomanufac <- qcew_naics3[
  !(industry_code %in% 311:339)
]

# Total nonmanufacturing employment
county_concentration_nomanufac[
  ,
  nonmanuf_total_emp :=
    sum(annual_avg_emplvl, na.rm = TRUE),
  by = .(commuting_zone_id_2000, year)
]

# Industry share, renormalized within nonmanufacturing
county_concentration_nomanufac[
  ,
  industry_share_nomanufac :=
    annual_avg_emplvl / nonmanuf_total_emp
]

# Nonmanufacturing concentration
county_concentration_nomanufac <- county_concentration_nomanufac[
  ,
  .(
    industry_hhi_nomanufac =
      sum(industry_share_nomanufac^2, na.rm = TRUE),
    
    industry_share_sd_nomanufac =
      sd(industry_share_nomanufac, na.rm = TRUE)
  ),
  by = .(commuting_zone_id_2000, year)
]


# Merge concentration measures
county_concentration <- merge(
  county_concentration,
  county_concentration_nomanufac,
  by = c("commuting_zone_id_2000", "year"),
  all.x = TRUE
)

qcew_naics3 <- merge(
  qcew_naics3,
  county_concentration,
  by = c("commuting_zone_id_2000", "year"),
  all.x = TRUE
)

# ------------------------------------------------------------------------------
qcew_base <- qcew_naics3[year %in% c(1995, 2000, 2007)]
nrow(qcew_base)
qcew_base <- merge(
  qcew_base, acs,
  by = c("year", "commuting_zone_id_2000"),
  all.x = TRUE
)

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
         by = .(year, commuting_zone_id_2000)]

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

qcew_rep[, IPW_US_pop :=
           (L_ijt / L_ujt) * (Delta_M_US / population)]

qcew_rep[, IPW_OTH_pop :=
           (L_ijt / L_ujt) * (Delta_M_OTH / population)]

# scale by 1000 for thousand dollars per worker hour 
qcew_rep[, IPW_US  := IPW_US / 1000]
qcew_rep[, IPW_OTH := IPW_OTH / 1000]
qcew_rep[, IPW_US_pop  := IPW_US_pop / 1000]
qcew_rep[, IPW_OTH_pop := IPW_OTH_pop / 1000]

# Collapse across industries to county x period
instrument <- qcew_rep |>
  fgroup_by(commuting_zone_id_2000, year) |>
  fsummarize(
    IPW_US  = fsum(IPW_US),
    IPW_OTH = fsum(IPW_OTH), 
    IPW_US_pop  = fsum(IPW_US_pop),
    IPW_OTH_pop = fsum(IPW_OTH_pop), 
    industry_hhi = fmean(industry_hhi),
    industry_hhi_nomanufac = fmean(industry_hhi_nomanufac)
  ) |>
  data.table()
fwrite(instrument, file = paste0(path, "/output/final_ipw_naics3_CZ.csv"))

# 1990 version for 10-year shock -----------------------------------------------
qcew_base_10yr <- qcew_naics3[year %in% c(1990,2000,2007)]
qcew_base <- merge(
  qcew_base, acs,
  by = c("year", "commuting_zone_id_2000"),
  all.x = TRUE
)

qcew_base_10yr[, baseline_year := year]

# 1990 employment weights the 1990-2000 trade shock
#shift the year for weighting by beginning of period employment values 
qcew_base_10yr[year == 2007, year := 2013]
qcew_base_10yr[year == 2000, year := 2007]
qcew_base_10yr[year == 1990, year := 2000]

qcew_rep_10yr <- merge(
  qcew_base_10yr,
  shock_10yr,
  by.x = c("year", "industry_code"),
  by.y = c("refYear", "naics3"),
  all.x = TRUE
)

# Baseline county employment
qcew_rep_10yr[, L_it := sum(annual_avg_emplvl, na.rm = TRUE),
              by = .(year, commuting_zone_id_2000)]

# Baseline county-industry employment
qcew_rep_10yr[, L_ijt := annual_avg_emplvl]

# Baseline US employment in industry j
qcew_rep_10yr[, L_ujt := sum(annual_avg_emplvl, na.rm = TRUE),
              by = .(year, industry_code)]

# Industries with no trade match contribute zero
qcew_rep_10yr[is.na(Delta_M_US_10yr), Delta_M_US_10yr := 0]
qcew_rep_10yr[is.na(Delta_M_OTH_10yr), Delta_M_OTH_10yr := 0]

# Construct 10-year exposure
qcew_rep_10yr[, IPW_US_10yr :=
                (L_ijt / L_ujt) * (Delta_M_US_10yr / L_it)]

qcew_rep_10yr[, IPW_OTH_10yr :=
                (L_ijt / L_ujt) * (Delta_M_OTH_10yr / L_it)]

# Thousands of dollars per worker
qcew_rep_10yr[, IPW_US_10yr := IPW_US_10yr / 1000]
qcew_rep_10yr[, IPW_OTH_10yr := IPW_OTH_10yr / 1000]

instrument_10yr <- qcew_rep_10yr |>
  fgroup_by(commuting_zone_id_2000, year) |>
  fsummarize(
    IPW_US_10yr = fsum(IPW_US_10yr),
    IPW_OTH_10yr = fsum(IPW_OTH_10yr),
    industry_hhi_10yr = fmean(industry_hhi),
    industry_hhi_nomanufac_10yr = fmean(industry_hhi_nomanufac)
  ) |>
  data.table()

fwrite(
  instrument_10yr,
  paste0(path, "/output/final_ipw_naics3_10yr_CZ.csv")
)

# Get employment outcome -------------------------------------------------------
# Use NAICS2 data since manufacturing is identified cleanly there
qcew_outcome <- qcew_naics3
qcew_outcome[, industry_code := as.integer(industry_code)]
qcew_outcome[,naics2:= floor(as.integer(industry_code/10))]

qcew_outcome <- qcew_outcome[
  year %in% c(1995, 2000, 2007, 2013)
]

# Total county employment
county_emp <- qcew_outcome |>
  fgroup_by(commuting_zone_id_2000, year) |>
  fsummarize(
    workplace_emp = fsum(annual_avg_emplvl)
  ) |>
  data.table()

# Manufacturing county employment
county_manufac <- qcew_outcome[
  naics2 %in% c(31, 32, 33)
] |>
  fgroup_by(commuting_zone_id_2000, year) |>
  fsummarize(
    manufac_emp = fsum(annual_avg_emplvl)
  ) |>
  data.table()

# Merge and construct manufacturing employment share
county_emp <- merge(
  county_emp,
  county_manufac,
  by = c("commuting_zone_id_2000", "year"),
  all.x = TRUE
)

county_emp[is.na(manufac_emp), manufac_emp := 0]

county_emp[, sh_empl_mfg := manufac_emp / workplace_emp * 100 ]


# Long differences: 1995-2000 and 2000-2007
setorder(county_emp, commuting_zone_id_2000, year)

county_emp[, d_sh_empl_mfg :=
             sh_empl_mfg - shift(sh_empl_mfg),
           by = commuting_zone_id_2000
]
# baseline manufacturing share control
county_emp[, l_sh_empl_mfg :=
             shift(sh_empl_mfg),
           by = commuting_zone_id_2000
]

# baseline employment weight as a temporary county analogue
county_emp[, baseline_emp :=
             shift(workplace_emp),
           by = commuting_zone_id_2000
]

# Regressions ------------------------------------------------------------------
# Merge onto the two-period instrument
# instrument moves the years up a period so we need to keep all county_emp 
base <- merge(
  county_emp,
  instrument,
  by = c("commuting_zone_id_2000", "year"), 
  all.x = TRUE
)


base <- merge(
  base,
  instrument_10yr,
  by = c("commuting_zone_id_2000", "year"), 
  all.x = TRUE
)

# save file 
fwrite(base, paste0(path, "/output/weighted_qcew_CZ.csv"))

# check regs 2007 --------------------------------------------------------------
reg <- base[year %in% c(2000, 2007), ]# Period indicator
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

summary(mod, stage = 2)

mod <- feols(
  d_sh_empl_mfg ~ t2  |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

summary(mod, stage = 2)


mod <- feols(
  d_sh_empl_mfg ~ t2 +industry_hhi |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

summary(mod, stage = 2)



mod <- feols(
  industry_hhi_nomanufac ~ t2 |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

summary(mod, stage = 1)
summary(mod, stage = 2)


exit 


# check regs 2013 --------------------------------------------------------------
reg <- base[year %in% c(2007, 2013), ]# Period indicator
reg[, t2 := as.integer(year == 2013)]

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


