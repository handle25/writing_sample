
rm(list = ls())
library(collapse) 
library(readxl)
library(data.table)
library(fixest)
library(sf)
library(haven)
library(tigris)
library(ggplot2)
library(janitor)
library(dplyr)
# qcewdata 
path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
setwd(path)


# lodes <- fread(paste0(path, "/lodes/clean/mi_full_2002_2020.csv"))
# 
# # has location county and census block
# crosswalk <- fread(paste0(path, "/nhgis_blk2000_co2015/nhgis_blk2000_co2015.csv"))
# 
# # need to go from lodes census block -> crosswalk census block -> county -> qcew county
# crosswalk[,state:=floor(blk2000ge/1e13)]
# crosswalk <- crosswalk[state == 26,]
# 
# # merge on block group for home and workplace
# lodes <- merge(lodes, crosswalk, by.x = "h_geocode", by.y = "blk2000ge")
# setnames(lodes, "co2015ge", "h_county")
# lodes <- merge(lodes, crosswalk, by.x = "w_geocode", by.y = "blk2000ge")
# setnames(lodes, "co2015ge", "w_county")

# # read in data -----------------------------------------------------------------
# # has location area_fips which is county level 
# qcew_list <- vector("list", length(1995:2014))
# denom_list <- vector("list", length(1995:2014))
# full_qcew_list <- vector("list", length(1995:2014))
# full_qcew6_list <- vector("list", length(1995:2014))
# 
# for (y in 1995:2014){
#   
#   qcew <- fread(paste0(path, "/qcew/clean/full_",y,".csv"))
#   qcew[,hierarchy:= nchar(industry_code)]
#   
#   # get denominator of total employment 
#   denominator <- qcew[agglvl_code == 70,] |> 
#     fgroup_by(year) |> 
#     fsummarize(total_emp = fsum(annual_avg_emplvl))
#   
#   # get only naics6 levels 
#   qcew_naics6 <- qcew[agglvl_code == 78,]
#   
#   qcew <- qcew[agglvl_code == 74,]
#   qcew[industry_code == "31-33" , industry_code := 31]
#   qcew[industry_code == "44-45" , industry_code := 44]
#   qcew[industry_code == "48-49" , industry_code := 48]
#   
#   qcew[,industry_code := as.integer(industry_code)]
#   
#   # immediately collapse to area industry year level 
#   qcew <- qcew |> fgroup_by(area_fips, industry_code, year) |> 
#     fsummarize(total_annual_wages = fsum(total_annual_wages), 
#                annual_avg_emplvl = fsum(annual_avg_emplvl), 
#                annual_avg_estabs_count = fsum(annual_avg_estabs_count)) |> 
#     ungroup() |> data.table()
#   
#   
#   # immediately collapse to area industry year level 
#   qcew_naics6 <- qcew_naics6 |> fgroup_by(area_fips, industry_code, year) |> 
#     fsummarize(total_annual_wages = fsum(total_annual_wages), 
#                annual_avg_emplvl = fsum(annual_avg_emplvl), 
#                annual_avg_estabs_count = fsum(annual_avg_estabs_count)) |> 
#     ungroup() |> data.table()
#   
#   qcew[,naics2 := floor(industry_code/1)]
#   
#   ################################################################################
#   # replicate figure 1 in their paper ############################################
#   qcew[,manufac := as.integer(naics2 %in% c(31, 32, 33))]
#   qcew[,total_emp := sum(annual_avg_emplvl), by = .(year)]
#   toplot <- qcew |> fgroup_by(manufac, year) |> 
#     fsummarize(sector_emp = fsum(annual_avg_emplvl), 
#                all_emp = fmean(total_emp)) |> 
#     ungroup() |> 
#     filter(manufac == 1)
#   
#   toplot <- merge(toplot, denominator, by = c("year")) |> 
#     fmutate(share = sector_emp / total_emp)
#   
#   assign(paste0("full_", y), toplot)
#   qcew_list[[y - 1994]] <- toplot
#   assign(paste0("denom_", y), toplot)
#   denom_list[[y - 1994]] <- denominator
#   full_qcew_list[[y - 1994]] <- qcew
#   full_qcew6_list[[y - 1994]] <- qcew_naics6
# }
# 
# 
# toplot <- rbindlist(qcew_list)
# qcew <- rbindlist(full_qcew_list)
# qcew_naics6 <- rbindlist(full_qcew6_list)
# 
# 
# # matches beautifully 
# ggplot(data = toplot, aes(x = year, y = share))+
#   geom_line()+ 
#   scale_y_continuous(breaks = seq(.07, 0.15,by = .01))+
#   theme_bw()
# ggplot(data = toplot, aes(x = year, y = sector_emp))+
#   geom_line()+ 
#   theme_bw()

################################################################################

# trade data -> naics for naics level shock ------------------------------------ 
dd <- data.table(read_stata(paste0(path, "/112670-V1/Public-Release-Data/dta/workfile_china.dta")))

years <- c(1991, 1995, 2000, 2007, 2013)
shock_list <- vector("list", length(years))

for (i in seq_along(years)) {
  
  y <- years[i]
  
  shock <- fread(
    paste0(path, "/comtrade/TradeData_8_7_2026_", y, ".csv"),
    fill = TRUE,
    quote = "\""
  ) |>
    fselect(
      reporterCode, reporterISO, flowCode, partnerISO, refYear, refPeriodId,
      freqCode, cifvalue, fobvalue, cmdCode, primaryValue
    )
  
  shock_list[[i]] <- shock
}

#get all HS 6 imports 
shock <- rbindlist(shock_list)
                   
#take only imports
shock <- shock[flowCode == "M",]
shock[, chars := nchar(cmdCode)]
shock <- shock[chars %in% c(5,6), ]
shock[, cmdCode := as.integer(cmdCode)]

crosswalk <- fread(paste0(path, "/hssicnaics_20181015/hs_sic_naics_exports_89_117_20180927.csv"))
crosswalk[, hs6 := as.integer(substr(commodity, 1, 6))]

new <- merge(shock, crosswalk, by.x = c("refYear", "cmdCode"), by.y = c("year", "hs6"), all.x = TRUE)
new[,sic := as.integer(sic)]
nrow(new)
new <- new[sic >= 2011 & sic <= 3999,]
nrow(new)
new[reporterISO != "USA", reporterISO := "OTH"]
new <- new[reporterISO == "USA",]
new <- new |> 
  fgroup_by(naics, refYear, naicsX, partnerISO) |> 
  fsummarize(imports = fsum(primaryValue)) |> 
  fmutate(naicsx = as.integer(naicsX))




################################################################################
sh <- dcast(
  new,
  naics + refYear + naicsX + naicsx ~ partnerISO,
  value.var = "imports"
)
sh <- sh |> fgroup_by(refYear) |> 
  fsummarize(china = fsum(CHN), 
             world = fsum(W00)) |> 
  fmutate(china_share = china / world )



# trade data -> naics for naics level shock ------------------------------------ 
shock <- data.table(read_stata(paste0(path, "/112670-V1/Public-Release-Data/dta/sic87dd_trade_data.dta")))
shock[exporter != "CHN", exporter := "ROW"]

shock<- shock[importer == "USA",] |> 
  fgroup_by(year, exporter) |> 
  fsummarize(imports = fsum(imports))

sh_old <- dcast(
  shock,
  year ~ exporter,
  value.var = "imports"
)

sh_old[, china_share_old := CHN / (CHN + ROW)]

compare <- merge(
  sh,
  sh_old,
  by.x = "refYear",
  by.y = "year"
)

compare[, .(refYear, china_share, china_share_old)]
ggplot(compare, aes(x = refYear)) +
  geom_line(aes(y = china_share, linetype = "Comtrade")) +
  geom_line(aes(y = china_share_old, linetype = "ADH")) +
  geom_point(aes(y = china_share, shape = "Comtrade")) +
  geom_point(aes(y = china_share_old, shape = "ADH")) +
  theme_bw() +
  labs(x = NULL, y = "Chinese Share of U.S. Imports",
       linetype = NULL, shape = NULL)

crosswalk <- data.table(fread(paste0(path, "/my_crosswalk.csv"))) |> clean_names()

# merge in naics 
shock <- merge(shock, crosswalk, by.x = "sic87dd", by.y = "sic87")
# shock[, naics2 := substr(naics12, 1, 2)]
shock[, all_imports := sum(imports), by = .(year)]
shock[, total_imports := sum(imports, na.rm = TRUE),
      by = .(importer, naics12, year)]

# Chinese imports only
china_share_old <- shock[exporter == "CHN"] |>
  fgroup_by(importer, naics12, year) |>
  fsummarize(
    china_imports = fsum(imports),
    total_imports = fmean(total_imports)
  ) |>
  fmutate(
    china_share_old = china_imports / total_imports
  ) |>
  data.table()

################################################################################
# replicate their measures 
# use other country imports 
# Get Chinese imports for both USA and OTH
rep <- shock[importer %in% c("USA", "OTH")]
rep <- rep[year %in% c(1991, 2000, 2007)]

# Merge SIC -> NAICS
rep <- merge(rep, crosswalk, by.x = "sic87dd", by.y = "sic87")

# Collapse to importer x NAICS6 x year Chinese imports
rep <- rep[exporter == "CHN"] |>
  fgroup_by(importer, naics12, year) |>
  fsummarize(imports = fsum(imports)) |>
  data.table()

# Get long differences within importer x industry
setorder(rep, importer, naics12, year)

rep[, Delta_M := imports - shift(imports),
    by = .(importer, naics12)]

# Keep only the two long-difference observations
rep <- rep[year %in% c(2000, 2007)]

# Put USA and OTH changes in separate columns
rep <- dcast(
  rep,
  naics12 + year ~ importer,
  value.var = "Delta_M"
)
################################################################################


################################################################################
# replicate their measures 
# use other country imports 
# Get Chinese imports for both USA and OTH
rep <- shock[importer %in% c("USA", "OTH")]
rep <- rep[year %in% c(1991, 2000, 2007)]

# Merge SIC -> NAICS
rep <- merge(rep, crosswalk, by.x = "sic87dd", by.y = "sic87")

# Collapse to importer x NAICS6 x year Chinese imports
rep <- rep[exporter == "CHN"] |>
  fgroup_by(importer, naics12, year) |>
  fsummarize(imports = fsum(imports)) |>
  data.table()

# Get long differences within importer x industry
setorder(rep, importer, naics12, year)

rep[, Delta_M := imports - shift(imports),
    by = .(importer, naics12)]

# Keep only the two long-difference observations
rep <- rep[year %in% c(2000, 2007)]

# Put USA and OTH changes in separate columns
rep <- dcast(
  rep,
  naics12 + year ~ importer,
  value.var = "Delta_M"
)

setnames(rep,
         old = c("USA", "OTH"),
         new = c("Delta_M_US", "Delta_M_OTH"))

# -------------------------------------------------------------------------
# Baseline QCEW employment weights
# 1995 employment -> 1991-2000 shock stored at year 2000
# 2000 employment -> 2000-2007 shock stored at year 2007
# -------------------------------------------------------------------------

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
  by.y = c("year", "naics12"),
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
qcew_outcome[,naics2:= floor(as.integer(industry_code/10000))]

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




lodes <- fread(paste0(path, "/lodes/clean/mi_full_2002_2020.csv"))

# has location county and census block
crosswalk <- fread(paste0(path, "/nhgis_blk2000_co2015/nhgis_blk2000_co2015.csv"))

# need to go from lodes census block -> crosswalk census block -> county -> qcew county
crosswalk[,state:=floor(blk2000ge/1e13)]
crosswalk <- crosswalk[state == 26,]

# merge on block group for home and workplace
lodes <- merge(lodes, crosswalk, by.x = "h_geocode", by.y = "blk2000ge")
setnames(lodes, "co2015ge", "h_county")
lodes <- merge(lodes, crosswalk, by.x = "w_geocode", by.y = "blk2000ge")
setnames(lodes, "co2015ge", "w_county")
