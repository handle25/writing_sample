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
qcew_naics3[, industry_code := as.integer(industry_code)]
qcew_naics3[,area_fips := as.character(as.integer(area_fips))]

# trade data -> naics for naics level shock ------------------------------------ 
shock <- fread(paste0(path, "/output/Delta_M_naics3_lp.csv"))

# population weights -----------------------------------------------------------
acs <- rbind(fread(paste0(path, "/acs/co-est00int-tot.csv")), 
             fread(paste0(path,"/acs/co-est2020.csv")), fill = TRUE)

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

acs[, area_fips := as.character(paste0(
  sprintf("%02d", STATE),
  sprintf("%03d", COUNTY)
))]

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
acs[,area_fips := as.character(as.integer(area_fips))]

# Create QCEW employment weights -----------------------------------------------
# take one base period for now 
qcew_base <- qcew_naics3[year == 1995,]
qcew_base <- merge(qcew_base, acs, 
                   by = c("area_fips", "year"), 
                   all.x = TRUE)

# Baseline county employment
qcew_base[, L_it := sum(annual_avg_emplvl, na.rm = TRUE),
         by = .(area_fips)]

# Baseline county-industry employment
qcew_base[, L_ijt := annual_avg_emplvl]

# Baseline US employment in industry j
qcew_base[, L_ujt := sum(annual_avg_emplvl, na.rm = TRUE),
         by = .(industry_code)]

qcew_base <- qcew_base |> fselect(area_fips, 
                                  industry_code, 
                                  L_it, L_ijt, L_ujt, 
                                  population)

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
qcew_rep[, IPW_US :=
           (L_ijt / L_ujt) * (Delta_M_US / population)]

qcew_rep[, IPW_OTH :=
           (L_ijt / L_ujt) * (Delta_M_OTH / population)]

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
fwrite(instrument, file = paste0(path, "/output/lp_final_ipw_naics3.csv"))

# Get employment outcome -------------------------------------------------------
# Use NAICS2 data since manufacturing is identified cleanly there
qcew_outcome <- fread(paste0(path, "/qcew/clean/new_full_qcew_1995_2025.csv"))
qcew_outcome[, industry_code := as.integer(industry_code)]
qcew_outcome[, area_fips := as.character(area_fips)]
qcew_outcome[,naics2:= floor(as.integer(industry_code/10))]


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
