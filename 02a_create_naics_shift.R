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
path <- "D:/writing_sample/data/qcew"
setwd(path)

# Create a 1:1 crosswalk hs10 -> hs6 -> naics ----------------------------------
# import the weights to take the largest naics value by hs6 code to map hs6 -> naics
weights <- fread(paste0(path, "/peter_schott/weights.csv"))

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
    quote = "\"",
    integer64 = "double"
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
new[,naics3 := floor(naics/1000)]
#filter to only manufacturing 
new <- new[naics2 %in% c(31, 32, 33)]

# Drop trade observations that do not map to a NAICS industry
new <- new[!is.na(naics)]

# Collapse comparison countries into OTH
new[reporterISO != "USA", reporterISO := "OTH"]

# Keep imports from China only
new <- new[partnerISO == "CHN", ]

# Collapse to importer x NAICS6 x year
rep <- new |>
  fgroup_by(reporterISO, naics3, refYear) |>
  fsummarize(
    imports = fsum(primaryValue, na.rm = TRUE)
  ) |>
  data.table()

# Long differences in Chinese imports within importer x industry
setorder(rep, reporterISO, naics3, refYear)

rep_wide <- dcast(
  rep,
  reporterISO + naics3 ~ refYear,
  value.var = "imports"
)

rep_wide[, Delta_M_2000 := `2000` - `1995`]
rep_wide[, Delta_M_2007 := `2007` - `2000`]
rep_wide[, Delta_M_2013 := `2013` - `2007`]

rep <- melt(
  rep_wide,
  id.vars = c("reporterISO", "naics3"),
  measure.vars = c("Delta_M_2000", "Delta_M_2007", "Delta_M_2013"),
  variable.name = "refYear",
  value.name = "Delta_M"
)

rep[, refYear := as.integer(sub("Delta_M_", "", refYear))]

# Keep the endpoint observations
rep <- rep[refYear %in% c(1995, 2000, 2007, 2013)]

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
fwrite(rep, file = paste0(path, "/output/Delta_M_naics3.csv"))



