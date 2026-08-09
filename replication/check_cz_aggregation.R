################################################################################
# Created 8.9.2026
# Author: Sophie Handley
#
# Purpose:
# Aggregate county-level QCEW data to 1990 Commuting Zones,
# construct NAICS3 China import exposure at the CZ level,
# and estimate the ADH-style IV specification.
################################################################################

rm(list = ls())

library(collapse)
library(data.table)
library(fixest)
library(haven)

path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
setwd(path)

# Read county -> CZ crosswalk

cz_cw <- data.table(
  read_stata(
    paste0(
      path,
      "/david_dorn/cw_cty_czone.dta"
    )
  )
)


setnames(cz_cw, "cty_fips", "area_fips")

# Read CZ -> state crosswalk


# Read QCEW NAICS3 data

years_qcew <- c(1995, 2000, 2007)

full_qcew3_list <- vector("list", length(years_qcew))

for (i in seq_along(years_qcew)) {
  
  y <- years_qcew[i]
  
  qcew <- fread(
    paste0(path, "/qcew/clean/full_", y, ".csv")
  )
  
  # NAICS3
  qcew_naics3 <- qcew[agglvl_code == 75]
  
  qcew_naics3 <- qcew_naics3 |>
    fgroup_by(area_fips, industry_code, year) |>
    fsummarize(
      total_annual_wages =
        fsum(total_annual_wages, na.rm = TRUE),
      
      annual_avg_emplvl =
        fsum(annual_avg_emplvl, na.rm = TRUE),
      
      annual_avg_estabs_count =
        fsum(annual_avg_estabs_count, na.rm = TRUE)
    ) |>
    data.table()
  
  full_qcew3_list[[i]] <- qcew_naics3
}

qcew_naics3 <- rbindlist(full_qcew3_list)
qcew_naics3[, area_fips := as.integer(area_fips)]

qcew_naics3[, industry_code := as.integer(industry_code)]
qcew_naics3[, statefip := floor(as.integer(area_fips) / 1000)]
# Merge counties to 1990 CZs

qcew_naics3 <- merge(
  qcew_naics3,
  cz_cw[, .(area_fips, czone)],
  by = "area_fips",
  all.x = TRUE
)

# diagnostic
sum(is.na(qcew_naics3$czone))

# drop counties that cannot be mapped
qcew_naics3 <- qcew_naics3[!is.na(czone)]

# Collapse county x NAICS3 to CZ x NAICS3

qcew_naics3_cz <- qcew_naics3 |>
  fgroup_by(czone, statefip, industry_code, year) |>
  fsummarize(
    total_annual_wages =
      fsum(total_annual_wages, na.rm = TRUE),
    
    annual_avg_emplvl =
      fsum(annual_avg_emplvl, na.rm = TRUE),
    
    annual_avg_estabs_count =
      fsum(annual_avg_estabs_count, na.rm = TRUE)
  ) |>
  data.table()

# Read NAICS3 trade shocks

shock <- fread(
  paste0(path, "/output/Delta_M_naics3.csv")
)

# Construct CZ employment weights

# 1995 employment -> 1995-2000 shock stored at 2000
# 2000 employment -> 2000-2007 shock stored at 2007

qcew_base <- qcew_naics3_cz[
  year %in% c(1995, 2000)
]

qcew_base[, baseline_year := year]

qcew_base[year == 2000, year := 2007]
qcew_base[year == 1995, year := 2000]

# Merge trade shock to CZ x NAICS3 baseline employment

qcew_rep <- merge(
  qcew_base,
  shock,
  by.x = c("year", "industry_code"),
  by.y = c("refYear", "naics3"),
  all.x = TRUE
)

# Construct IPW

# Baseline CZ employment
qcew_rep[, L_it :=
           sum(annual_avg_emplvl, na.rm = TRUE),
         by = .(year, statefip, czone)]

# CZ x industry employment
qcew_rep[, L_ijt := annual_avg_emplvl]

# U.S. employment in industry j
qcew_rep[, L_ujt :=
           sum(annual_avg_emplvl, na.rm = TRUE),
         by = .(year, industry_code)]

# Unmatched industries contribute zero trade shock
qcew_rep[is.na(Delta_M_US),
         Delta_M_US := 0]

qcew_rep[is.na(Delta_M_OTH),
         Delta_M_OTH := 0]

# Import exposure
qcew_rep[, IPW_US :=
           (L_ijt / L_ujt) *
           (Delta_M_US / L_it)]

qcew_rep[, IPW_OTH :=
           (L_ijt / L_ujt) *
           (Delta_M_OTH / L_it)]

# Thousands of dollars per worker
qcew_rep[, IPW_US := IPW_US / 1000]
qcew_rep[, IPW_OTH := IPW_OTH / 1000]

# Collapse IPW across industries to CZ x period

instrument <- qcew_rep |>
  fgroup_by(czone, statefip, year) |>
  fsummarize(
    IPW_US = fsum(IPW_US),
    IPW_OTH = fsum(IPW_OTH)
  ) |>
  data.table()

fwrite(
  instrument,
  paste0(path, "/output/final_ipw_naics3_cz.csv")
)

# Construct CZ manufacturing employment outcome

qcew_outcome <- copy(qcew_naics3_cz)

qcew_outcome[, naics2 :=
               floor(as.integer(industry_code) / 10)]

# Total CZ employment

cz_emp <- qcew_outcome |>
  fgroup_by(czone, year) |>
  fsummarize(
    total_emp =
      fsum(annual_avg_emplvl, na.rm = TRUE)
  ) |>
  data.table()

# Manufacturing CZ employment

cz_manufac <- qcew_outcome[
  naics2 %in% c(31, 32, 33)
] |>
  fgroup_by(czone, year) |>
  fsummarize(
    manufac_emp =
      fsum(annual_avg_emplvl, na.rm = TRUE)
  ) |>
  data.table()

# Manufacturing employment share

cz_emp <- merge(
  cz_emp,
  cz_manufac,
  by = c("czone", "year"),
  all.x = TRUE
)

cz_emp[
  is.na(manufac_emp),
  manufac_emp := 0
]

cz_emp[, sh_empl_mfg :=
         manufac_emp / total_emp]

# Long differences

setorder(
  cz_emp,
  czone,
  year
)

cz_emp[, d_sh_empl_mfg :=
         sh_empl_mfg -
         shift(sh_empl_mfg),
       by = czone]

# baseline manufacturing share
cz_emp[, l_shind_manuf :=
         shift(sh_empl_mfg),
       by = czone]

# baseline employment weight
cz_emp[, baseline_emp :=
         shift(total_emp),
       by = czone]

# Merge outcome and instrument

reg <- merge(
  cz_emp,
  instrument,
  by = c("czone", "year")
)



reg <- reg[
  year %in% c(2000, 2007)
]

reg[, t2 :=
      as.integer(year == 2007)]

# Add state for clustering



# IV regression

mod <- feols(
  d_sh_empl_mfg ~ t2 |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

summary(mod, stage = 1)
summary(mod, stage = 2)