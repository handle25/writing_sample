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
# save
qcew_naics3 <- fread(paste0(path, "/qcew/clean/new_full_qcew_1995_2025.csv"))

# Industry concentration -------------------------------------------------------

qcew_naics3[, total_emp :=
              sum(annual_avg_emplvl, na.rm = TRUE),
            by = .(area_fips, year)]

qcew_naics3[, industry_share :=
              annual_avg_emplvl / total_emp]

county_concentration <- qcew_naics3[
  ,
  .(
    industry_hhi = sum(industry_share^2, na.rm = TRUE),
    industry_share_sd = sd(industry_share, na.rm = TRUE)
  ),
  by = .(area_fips, year)
]

qcew_naics3 <- merge(
  qcew_naics3,
  county_concentration,
  by = c("area_fips", "year"),
  all.x = TRUE
)

qcew_naics3[, industry_code := as.integer(industry_code)]
qcew_naics3[,area_fips := as.character(as.integer(area_fips))]

# trade data -> naics for naics level shock ------------------------------------ 
shock <- fread(paste0(path, "/output/Delta_M_naics3_lp.csv"))


acs <- fread(paste0(path, "/acs/population_1995_2023.csv"))
acs[, area_fips := as.character(area_fips)]

# Create QCEW employment weights -----------------------------------------------
# take one base period for now 
qcew_base <- qcew_naics3[year == 2000,]
qcew_base <- merge(qcew_base, acs, 
                   by = c("area_fips", "year"), 
                   all.x = TRUE)
setnames(
  qcew_base,
  c("industry_hhi", "industry_share_sd"),
  c("industry_hhi_base", "industry_share_sd_base")
)
# Baseline county employment
qcew_base[, L_it := sum(annual_avg_emplvl, na.rm = TRUE),
         by = .(area_fips)]

# Baseline county-industry employment
qcew_base[, L_ijt := annual_avg_emplvl]

# Baseline US employment in industry j
qcew_base[, L_ujt := sum(annual_avg_emplvl, na.rm = TRUE),
         by = .(industry_code)]

qcew_base <- qcew_base |>
  fselect(
    area_fips,
    industry_code,
    L_it, L_ijt, L_ujt,
    population,
    industry_hhi_base,
    industry_share_sd_base
  )

nrow(qcew_naics3)
qcew_naics3 <- merge(
  qcew_naics3, qcew_base, 
  by = c("area_fips", "industry_code")
)
nrow(qcew_naics3)

# Merge both trade shocks onto the same baseline employment data
qcew_rep <- merge(
  qcew_naics3,
  shock,
  by.x = c("year", "industry_code"),
  by.y = c("refYear", "naics3"),
  all.x = TRUE
)

# Industries with no trade match contribute zero
qcew_rep[is.na(Delta_M_US),  Delta_M_US := 0]
qcew_rep[is.na(Delta_M_OTH), Delta_M_OTH := 0]

# Same employment weights applied to both trade changes
qcew_rep[, IPW_US_pop :=
           (L_ijt / L_ujt) * (Delta_M_US / population)]

qcew_rep[, IPW_OTH_pop :=
           (L_ijt / L_ujt) * (Delta_M_OTH / population)]

qcew_rep[, IPW_US :=
           (L_ijt / L_ujt) * (Delta_M_US / L_it)]

qcew_rep[, IPW_OTH :=
           (L_ijt / L_ujt) * (Delta_M_OTH / L_it)]

# scale by 1000 for thousand dollars per worker hour 
qcew_rep[, IPW_US  := IPW_US / 1000]
qcew_rep[, IPW_OTH := IPW_OTH / 1000]
qcew_rep[, IPW_US_pop  := IPW_US_pop / 1000]
qcew_rep[, IPW_OTH_pop := IPW_OTH_pop / 1000]


# Collapse across industries to county x period
instrument <- qcew_rep |>
  fgroup_by(area_fips, year) |>
  fsummarize(
    IPW_US      = fsum(IPW_US, na.rm = TRUE),
    IPW_OTH     = fsum(IPW_OTH, na.rm = TRUE),
    IPW_US_pop  = fsum(IPW_US_pop, na.rm = TRUE),
    IPW_OTH_pop = fsum(IPW_OTH_pop, na.rm = TRUE),
    industry_hhi_base =
      fmean(industry_hhi_base, na.rm = TRUE),
    industry_share_sd_base =
      fmean(industry_share_sd_base, na.rm = TRUE)
  ) |>
  data.table()
fwrite(instrument, file = paste0(path, "/output/lp_final_ipw_naics3.csv"))

# Get employment outcome -------------------------------------------------------
# Use NAICS2 data since manufacturing is identified cleanly there
qcew_outcome <- fread(paste0(path, "/qcew/clean/new_full_qcew_1995_2025.csv"))
qcew_outcome <- merge(
  qcew_outcome,
  county_concentration,
  by = c("area_fips", "year"),
  all.x = TRUE
)
qcew_outcome[, industry_code := as.integer(industry_code)]
qcew_outcome[, area_fips := as.character(area_fips)]
qcew_outcome[,naics2:= floor(as.integer(industry_code/10))]


# Total county employment
county_emp <- qcew_outcome |>
  fgroup_by(area_fips, year) |>
  fsummarize(
    total_emp = fsum(annual_avg_emplvl),
    industry_hhi =
      fmean(industry_hhi, na.rm = TRUE),
    industry_share_sd =
      fmean(industry_share_sd, na.rm = TRUE)
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

# differences 
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

base <- merge(
  county_emp,
  instrument,
  by = c("area_fips", "year")
)

# save file 
fwrite(base, paste0(path, "/output/lp_weighted_qcew.csv"))
