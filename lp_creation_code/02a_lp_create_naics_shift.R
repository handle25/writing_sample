################################################################################
# Created 8.8.2026 
# Author: Sophie Handley 
# Purpose: Create dollar shift in trade at the country x year x industry level 
#   - Naics x year level shifts in trade 
#   - Includes both OTH country and US to be instrumented 
#   - output: Delta_M.csv to be weighted by employment shares using QCEW
################################################################################
rm(list = ls())

# qcewdata 
path <- "D:/writing_sample/data"
setwd(path)

# Create a 1:1 crosswalk hs10 -> hs6 -> naics ----------------------------------
# import the weights to take the largest naics value by hs6 code to map hs6 -> naics
cw_1to1 <- fread(paste0(path, "/peter_schott/weights_extended_collapsed.csv"))
cw <- read_stata(
  "dataverse_files/rtp/rtp/data/analysis/m_flow_hs10_fm_new.dta"
) |>
  data.table()

cw <- cw |>
  fselect(hs10, naics_str, m_val)

# Restore HS10 to exactly 10 digits
cw[, hs10 := sprintf("%010.0f", hs10)]

# Create HS6 correctly
cw[, hs6 := substr(hs10, 1, 6)]

# One NAICS mapping per HS6
cw <- cw |>
  fgroup_by(hs6) |>
  fsummarize(naics = flast(naics_str)) |>
  data.table()

# Repeat fixed crosswalk for 2018-2026
cw_new <- rbindlist(
  lapply(2018:2026, function(y) {
    tmp <- copy(cw)
    tmp[, year := y]
    tmp
  })
)

cw_new[, hs6 := as.integer(hs6)]

cw_1to1 <- rbind(
  cw_1to1[year <= 2017],
  cw_new,
  fill = TRUE
)

setorder(cw_1to1, year, hs6)

# 95 % agreement in hs6 -> naics after 2011 
# test <- cw |> fgroup_by(hs6) |> 
#   fsummarize(naics_new = as.character(flast(naics)))
# test[,hs6 := as.integer(hs6)]
# test <- merge(cw_1to1, 
#               test, 
#               by = "hs6")
# test[, same := naics == naics_new]
# mean(test[year > 2013,same])

# Bring in trade data at hs6 level, merge to naics6 ---------------------------- 
years <- c(1995:2025)
shock_list <- vector("list", length(years))

for (i in seq_along(years)) {
  
  y <- years[i]
  
  shock <- fread(
    paste0(path, "/comtrade/TradeData_8_15_2026_", y, ".csv"),
    fill = TRUE,
    quote = "\"",
    integer64 = "double"
  ) |>
    fselect(
      reporterCode, reporterISO, flowCode, partnerISO, refYear, refPeriodId,
      freqCode, cifvalue, fobvalue, cmdCode, primaryValue
    )
  shock[, cifvalue := as.numeric(cifvalue)]
  shock[, fobvalue := as.numeric(fobvalue)]
  shock[, primaryValue := as.numeric(primaryValue)]
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
new[,naics3 := floor(naics/1000)]
#filter to only manufacturing 
new <- new[naics2 %in% c(31, 32, 33)]

# Drop trade observations that do not map to a NAICS industry
new <- new[!is.na(naics)]

# Collapse comparison countries into OTH
new[reporterISO != "USA", reporterISO := "OTH"]

# Keep imports from China only
new <- new[partnerISO %in% c("CHN"), ]

# Collapse to importer x NAICS6 x year
rep <- new |>
  fgroup_by(reporterISO, naics3, refYear) |>
  fsummarize(
    imports = fsum(primaryValue, na.rm = TRUE)
  ) |>
  data.table()

# Annual differences in Chinese imports within importer x industry
setorder(rep, reporterISO, naics3, refYear)

rep[, Delta_M := imports - shift(imports, type = "lag", n = 1),
    by = .(reporterISO, naics3)]


# Put USA and OTH shocks in separate columns
rep <- dcast(
  rep,
  naics3 + refYear ~ reporterISO,
  value.var = "Delta_M"
)

setnames(
  rep,
  old = c("USA", "OTH"),
  new = c("Delta_M_US", "Delta_M_OTH")
)

# save the dollar shift in trade 
fwrite(rep, file = paste0(path, "/output/Delta_M_naics3_lp.csv"))



