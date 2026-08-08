################################################################################
# Created 8.8.2026 to match manufacturing shares in China syndrome paper with new 
# data to make sure we hae the correct measure and extend out the shocks into 2013
# the second china shock wave. 
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

# Create a 1:1 crosswalk hs10 -> naics -----------------------------------------
weights <- data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_113n/imp_detl_yearly_113n.dta")))
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_105n/imp_detl_yearly_105n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_95n/imp_detl_yearly_95n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_91n/imp_detl_yearly_91n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_107n/imp_detl_yearly_107n.dta"))), fill = TRUE)

weights[, hs6 := floor(commodity / 10000)]

cw <- weights |>
  fgroup_by(year, hs6, sic) |>
  fsummarize(
    import_value = fsum(gen_val_yr, na.rm = TRUE)
  ) |>
  data.table()

setorder(cw, year, hs6, -import_value)

cw_1to1 <- cw[, .SD[1], by = .(year, hs6)]


# trade data hs10 -> naics for naics level shock ------------------------------- 
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

new <- merge(shock, cw_1to1, by.x = c("refYear", "cmdCode"), by.y = c("year", "hs6"), all.x = TRUE)
new[,sic := as.integer(sic)]
nrow(new)
new <- new[sic >= 2011 & sic <= 3999,]
nrow(new)
new[reporterISO != "USA", reporterISO := "OTH"]
new <- new[reporterISO == "USA",]
new <- new |> 
  fgroup_by( refYear, sic, partnerISO) |> 
  fsummarize(imports = fsum(primaryValue)) |> 
  fmutate(sic = as.integer(sic))

################################################################################
sh <- dcast(
  new,
  refYear + sic ~ partnerISO,
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


