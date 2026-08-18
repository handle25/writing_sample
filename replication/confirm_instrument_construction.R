################################################################################
# Creation date: 8.8.2026 
# Author: Sophie Handley 
# Purpose: Confirm that QCEW can replace ADH data, confirm instrument construction
# Using ADH data, replicate instrument construction and replace their employment 
# data with granular QCEW data and replicate results 
################################################################################
rm(list = ls())

# qcewdata 
path <- "D:/writing_sample/data"
setwd(path)

# read in data -----------------------------------------------------------------
# has location area_fips which is county level 
years_qcew <- c(1995, 2000, 2007)

qcew_list <- vector("list", length(years_qcew))
denom_list <- vector("list", length(years_qcew))
full_qcew_list <- vector("list", length(years_qcew))
full_qcew6_list <- vector("list", length(years_qcew))

for (i in seq_along(years_qcew)) {
  
  y <- years_qcew[i]

qcew <- fread(paste0(path, "/qcew/clean/full_",y,".csv"))
qcew[,hierarchy:= nchar(industry_code)]

# get denominator of total employment 
denominator <- qcew[agglvl_code == 70,] |> 
  fgroup_by(year) |> 
  fsummarize(total_emp = fsum(annual_avg_emplvl))

# get only naics6 levels 
qcew_naics6 <- qcew[agglvl_code == 75,]

qcew <- qcew[agglvl_code == 74,]
qcew[industry_code == "31-33" , industry_code := 31]
qcew[industry_code == "44-45" , industry_code := 44]
qcew[industry_code == "48-49" , industry_code := 48]

qcew[,industry_code := as.integer(industry_code)]

# immediately collapse to area industry year level 
qcew <- qcew |> fgroup_by(area_fips, industry_code, year) |> 
  fsummarize(total_annual_wages = fsum(total_annual_wages), 
             annual_avg_emplvl = fsum(annual_avg_emplvl), 
             annual_avg_estabs_count = fsum(annual_avg_estabs_count)) |> 
  ungroup() |> data.table()


# immediately collapse to area industry year level 
qcew_naics6 <- qcew_naics6 |> fgroup_by(area_fips, industry_code, year) |> 
  fsummarize(total_annual_wages = fsum(total_annual_wages), 
             annual_avg_emplvl = fsum(annual_avg_emplvl), 
             annual_avg_estabs_count = fsum(annual_avg_estabs_count)) |> 
  ungroup() |> data.table()

qcew[,naics2 := floor(industry_code/1)]

# replicate figure 1 in their paper --------------------------------------------
qcew[,manufac := as.integer(naics2 %in% c(31, 32, 33))]
qcew[,total_emp := sum(annual_avg_emplvl), by = .(year)]
toplot <- qcew |> fgroup_by(manufac, year) |> 
  fsummarize(sector_emp = fsum(annual_avg_emplvl), 
             all_emp = fmean(total_emp)) |> 
  ungroup() |> 
  filter(manufac == 1)

toplot <- merge(toplot, denominator, by = c("year")) |> 
  fmutate(share = sector_emp / total_emp)

assign(paste0("full_", y), toplot)
qcew_list[[i]] <- toplot

assign(paste0("denom_", y), toplot)
denom_list[[i]] <- denominator

full_qcew_list[[i]] <- qcew
full_qcew6_list[[i]] <- qcew_naics6
}


toplot <- rbindlist(qcew_list)
qcew <- rbindlist(full_qcew_list)
qcew_naics6 <- rbindlist(full_qcew6_list)


# matches beautifully 
ggplot(data = toplot, aes(x = year, y = share))+
  geom_line()+ 
  scale_y_continuous(breaks = seq(.07, 0.15,by = .01))+
  theme_bw()
ggplot(data = toplot, aes(x = year, y = sector_emp))+
  geom_line()+ 
  theme_bw()


# trade data -> naics for naics level shock ------------------------------------ 
shock <- data.table(read_stata(paste0(path, "/112670-V1/Public-Release-Data/dta/sic87dd_trade_data.dta")))
fig1 <- data.table(read_stata(paste0(path, "/112670-V1/Public-Release-Data/dta/figure1_data.dta")))
crosswalk <- data.table(fread(paste0(path, "/my_crosswalk.csv"))) |> clean_names()

# merge in naics 

shock[, all_imports := sum(imports), by = .(year)]
fig1_share <- fig1[, .(year, cpsmanufemppop)]

plot_compare <- merge(
  toplot[, .(year, share)],
  fig1_share,
  by = "year"
)

plot_compare_long <- melt(
  plot_compare,
  id.vars = "year",
  measure.vars = c("share", "cpsmanufemppop"),
  variable.name = "var",
  value.name = "value"
)

ggplot(plot_compare_long, aes(x = year, y = value, linetype = var)) +
  geom_line() +
  theme_bw() +
  labs(
    x = NULL,
    y = "Share of Employment",
    linetype = NULL
  )



# Replicate instrument construction using their data ---------------------------
# use other country imports 
# Get Chinese imports for both USA and OTH
rep <- shock[importer %in% c("USA", "OTH")]
rep <- rep[year %in% c(1995, 2000, 2007)]

# Merge SIC -> NAICS
rep <- merge(rep, crosswalk, by.x = "sic87dd", by.y = "sic87")
# Convert mapped NAICS6 to NAICS3
rep[, naics3 := floor(as.integer(naics12) / 1000)]

# Collapse to importer x NAICS3 x year
rep <- rep[exporter == "CHN"] |>
  fgroup_by(importer, naics3, year) |>
  fsummarize(imports = fsum(imports)) |>
  data.table()

setorder(rep, importer, naics3, year)

rep[, Delta_M := imports - shift(imports),
    by = .(importer, naics3)]

rep <- rep[year %in% c(2000, 2007)]

rep <- dcast(
  rep,
  naics3 + year ~ importer,
  value.var = "Delta_M"
)

setnames(rep,
         old = c("USA", "OTH"),
         new = c("Delta_M_US", "Delta_M_OTH"))

# Create QCEW employment weights -----------------------------------------------
# 1995 employment -> 1991-2000 shock stored at year 2000
# 2000 employment -> 2000-2007 shock stored at year 2007
# ------------------------------------------------------------------------------

qcew_naics6 <- rbindlist(full_qcew6_list)
qcew_naics6[, industry_code := as.integer(industry_code)]

qcew_base <- qcew_naics6[year %in% c(1995, 2000)]

qcew_base[, baseline_year := year]

qcew_base[year == 2000, year := 2007]
qcew_base[year == 1995, year := 2000]

# Merge both trade shocks onto the same baseline employment data
qcew_rep <- merge(
  qcew_base,
  rep,
  by.x = c("year", "industry_code"),
  by.y = c("year", "naics3"),
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

# Collapse across industries to county x period
instrument <- qcew_rep |>
  fgroup_by(area_fips, year) |>
  fsummarize(
    IPW_US  = fsum(IPW_US),
    IPW_OTH = fsum(IPW_OTH)
  ) |>
  data.table()

# Get employment outcome -------------------------------------------------------
# Use NAICS2 data since manufacturing is identified cleanly there
qcew_outcome <- rbindlist(full_qcew6_list)
qcew_outcome[, industry_code := as.integer(industry_code)]
qcew_outcome[,naics2:= floor(as.integer(industry_code/10))]

qcew_outcome <- qcew_outcome[
  year %in% c(1995, 2000, 2007)
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

# Period indicator
reg[, t2 := as.integer(year == 2007)]

# State FIPS for clustering
reg[, statefip := floor(as.integer(area_fips) / 1000)]

mod <- feols(
  d_sh_empl_mfg ~ t2 | IPW_US ~ IPW_OTH,
  data = reg,
  cluster = ~statefip
)
mod <- feols(
  d_sh_empl_mfg ~ t2 + l_shind_manuf |
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


