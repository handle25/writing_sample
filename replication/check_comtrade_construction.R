################################################################################
# Creation date: 8.8.2026
# Author: Sophie Handley
#
# Purpose:
# 1. Reconstruct ADH trade measures from raw UN Comtrade data.
# 2. Validate the reconstructed series against ADH's released trade data.
# 3. Once validated, extend the trade series beyond 2007 for the main analysis.
#
# Strategy:
# - Use Schott's detailed HS10 U.S. import data to construct a year-specific
#   HS6 -> SIC crosswalk.
# - Map UN Comtrade HS6 imports into SIC industries.
# - Restrict to the ADH manufacturing SIC universe.
# - Compare aggregate Chinese import penetration with the ADH series.
################################################################################

rm(list = ls())


# qcewdata 
path <- "D:/writing_sample/data"
figs <- "D:/writing_sample/figures"
setwd(path)

# Create a 1:1 crosswalk hs10 -> sic -------------------------------------------

weights <- fread(paste0(path, "/peter_schott/weights.csv"))

weights[, hs6 := floor(commodity / 10000)]

cw <- weights |>
  fgroup_by(year, hs6, sic) |>
  fsummarize(
    import_value = fsum(gen_val_yr, na.rm = TRUE)
  ) |>
  data.table()

setorder(cw, year, hs6, -import_value)

cw_1to1 <- cw[, .SD[1], by = .(year, hs6)]


# trade data hs10 -> sic for sic level shock ----------------------------------- 
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

# Aggregate Chinese share of ALL U.S. imports, before SIC mapping/filtering
check_all <- shock[reporterISO == "USA" & partnerISO %in% c("CHN", "W00"),
                   .(imports = sum(primaryValue, na.rm = TRUE)),
                   by = .(refYear, partnerISO)]

check_all <- dcast(
  check_all,
  refYear ~ partnerISO,
  value.var = "imports"
)

check_all[, china_share_all := CHN / W00]

shock[, cmdCode := as.integer(cmdCode)]

new <- merge(shock, cw_1to1, by.x = c("refYear", "cmdCode"), by.y = c("year", "hs6"), all.x = TRUE)
new[,sic := as.integer(sic)]
nrow(new)
# Restrict to manufacturing SIC industries used in the ADH trade data
new <- new[sic >= 2011 & sic <= 3999,]
nrow(new)
new[reporterISO != "USA", reporterISO := "OTH"]
new <- new[reporterISO == "USA",]
new <- new |> 
  fgroup_by( refYear, sic, partnerISO) |> 
  fsummarize(imports = fsum(primaryValue)) |> 
  fmutate(sic = as.integer(sic))

sh <- dcast(
  new,
  refYear + sic ~ partnerISO,
  value.var = "imports"
)
sh <- sh |> fgroup_by(refYear) |> 
  fsummarize(china = fsum(CHN), 
             world = fsum(W00)) |> 
  fmutate(china_share = china / world)

# trade data -> naics for sic level shock --------------------------------------
# Load ADH released trade data and construct the same aggregate measure
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


compare <- merge(
  compare,
  check_all[, .(refYear, china_share_all)],
  by = "refYear"
)

ggplot(compare, aes(x = refYear)) +
  geom_line(aes(y = china_share_old, linetype = "ADH manufacturing")) +
  geom_point(aes(y = china_share_old, shape = "ADH manufacturing")) +
  geom_text(aes(y = china_share_old, 
                label = round(china_share_old, 3)),
            vjust = -0.7) +
  
  geom_line(aes(y = china_share, linetype = "Comtrade manufacturing")) +
  geom_point(aes(y = china_share, shape = "Comtrade manufacturing")) +
  geom_text(aes(y = china_share,
                label = round(china_share, 3)),
            vjust = -0.7) +
  
  geom_line(aes(y = china_share_all, linetype = "Comtrade all imports")) +
  geom_point(aes(y = china_share_all, shape = "Comtrade all imports")) +
  geom_text(aes(y = china_share_all,
                label = round(china_share_all, 3)),
            vjust = 1.5) +
  
  theme_bw() +
  labs(
    x = NULL,
    y = "Chinese Share of U.S. Imports",
    linetype = NULL,
    shape = NULL
  )

ggsave(paste0(figs, "/trade_ts_comparison.pdf"))



################################################################################
# Plot full Comtrade time series
################################################################################

# Find all Comtrade files
comtrade_files <- list.files(
  paste0(path, "/comtrade"),
  pattern = "^TradeData_8_15_2026_[0-9]{4}\\.csv$",
  full.names = TRUE
)

# Read all years
comtrade_all <- rbindlist(
  lapply(comtrade_files, function(file) {
    
    fread(
      file,
      fill = TRUE,
      quote = "\""
    ) |>
      fselect(
        reporterCode, reporterISO, flowCode, partnerISO, refYear,
        refPeriodId, freqCode, cifvalue, fobvalue, cmdCode, primaryValue
      )
    
  }),
  fill = TRUE
)

# Keep imports only
comtrade_all <- comtrade_all[
  flowCode == "M"
]

# Chinese and world imports into the U.S.
comtrade_share <- comtrade_all[
  reporterISO == "USA" &
    partnerISO %in% c("CHN", "W00"),
  .(
    imports = sum(primaryValue, na.rm = TRUE)
  ),
  by = .(refYear, partnerISO)
]

# Put China and world imports in separate columns
comtrade_share <- dcast(
  comtrade_share,
  refYear ~ partnerISO,
  value.var = "imports"
)

# Chinese share of total U.S. imports
comtrade_share[
  ,
  china_share_all := CHN / W00
]

# Check series
comtrade_share[
  order(refYear),
  .(refYear, china_share_all)
]

comtrade_share[,china_share_all := china_share_all * 100]

# Plot
ggplot(
  comtrade_share,
  aes(
    x = refYear,
    y = china_share_all
  )
) +
  scale_x_continuous(
    breaks = seq(1990, 2025, by = 5)
  )+
  geom_line() +
  geom_point() +
  theme_bw() +
  geom_text(
    data = comtrade_share[refYear %in% c(1991, 1995, 2000, 2007, 2011, 2017, 2025)],
    aes(label = round(china_share_all, 0)),
    vjust = -0.7,
    hjust = -.3, 
    size = 3
  ) +
  labs(
    x = NULL,
    y = "Chinese Share of U.S. Imports",
    title = "Chinese Share of U.S. Imports: UN Comtrade",
  )

ggsave(paste0(figs, "/trade_ts_full.pdf"))
