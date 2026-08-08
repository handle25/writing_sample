################################################################################
# Created 8.8.2026 
# Author: Sophie Handley 
# Purpose: Create dollar shift in trade at the country x year x industry level 
#   - Naics x year level shifts in trade 
#   - Includes both OTH country and US to be instrumented 
#   - output: Delta_M.csv to be weighted by employment shares using QCEW
################################################################################
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

# Create a 1:1 crosswalk hs10 -> hs6 -> naics -----------------------------------------
weights <- data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_91n/imp_detl_yearly_91n.dta")))
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_95n/imp_detl_yearly_95n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_100n/imp_detl_yearly_100n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_105n/imp_detl_yearly_105n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_107n/imp_detl_yearly_107n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_113n/imp_detl_yearly_113n.dta"))), fill = TRUE)

weights[, hs6 := floor(commodity / 10000)]

cw <- weights |>
  fgroup_by(year, hs6, naics) |>
  fsummarize(
    import_value = fsum(gen_val_yr, na.rm = TRUE)
  ) |>
  data.table()

setorder(cw, year, hs6, -import_value)
  
cw_1to1 <- cw[, .SD[1], by = .(year, hs6)]

# Bring in trade data at hs6 level, merge to naics6 ---------------------------- 
years <- c(1995, 2000, 2007, 2013)
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

# Merge HS6 trade data to dominant NAICS6
new <- merge(
  shock,
  cw_1to1,
  by.x = c("refYear", "cmdCode"),
  by.y = c("year", "hs6"),
  all.x = TRUE
)

new[, naics := as.integer(naics)]
new[,naics2 := floor(naics/10000)]
#filter to only manufacturing 
new <- new[naics2 %in% c(31, 32, 33)]

# Drop trade observations that do not map to a NAICS industry
new <- new[!is.na(naics)]

# Collapse comparison countries into OTH
new[reporterISO != "USA", reporterISO := "OTH"]

# Keep imports from China only
new <- new[partnerISO == "CHN"]

# Collapse to importer x NAICS6 x year
rep <- new |>
  fgroup_by(reporterISO, naics, refYear) |>
  fsummarize(
    imports = fsum(primaryValue, na.rm = TRUE)
  ) |>
  data.table()

# Long differences in Chinese imports within importer x industry
setorder(rep, reporterISO, naics, refYear)

rep[, Delta_M :=
      imports - shift(imports),
    by = .(reporterISO, naics)
]

# Keep the endpoint observations
rep <- rep[refYear %in% c(1995, 2000, 2007, 2013)]

# Put USA and OTH shocks in separate columns
rep <- dcast(
  rep,
  naics + refYear ~ reporterISO,
  value.var = "Delta_M"
)

setnames(
  rep,
  old = c("USA", "OTH"),
  new = c("Delta_M_US", "Delta_M_OTH")
)

# save the dollar shift in trade 
fwrite(rep, file = paste0(path, "/output/Delta_M.csv"))



