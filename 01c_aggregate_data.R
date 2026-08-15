path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
# using U.S. HS10 import values to select the dominant SIC within each HS6
weights <- data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_91n/imp_detl_yearly_91n.dta")))
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_95n/imp_detl_yearly_95n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_100n/imp_detl_yearly_100n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_107n/imp_detl_yearly_107n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_113n/imp_detl_yearly_113n.dta"))), fill = TRUE)
fwrite(weights, paste0(path, "/peter_schott/weights.csv"))


# using U.S. HS10 import values to select the dominant SIC within each HS6
weights <- data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_91n/imp_detl_yearly_91n.dta")))
for (y in c(96:99)){
  weights <- rbind(weights, data.table(
    read_stata(paste0(path,
                      "/peter_schott/imp_detl_yearly_",
                      y,"n/imp_detl_yearly_", 
                      y, "n.dta"))), fill = TRUE)  
}
for (y in c(0:9)){
  weights <- rbind(weights, data.table(
    read_stata(paste0(path,
                      "/peter_schott/imp_detl_yearly_10",
                      y,"n/imp_detl_yearly_10", 
                      y, "n.dta"))), fill = TRUE)  
}
for (y in c(10:17)){
  weights <- rbind(weights, data.table(
    read_stata(paste0(path,
                      "/peter_schott/imp_detl_yearly_1",
                      y,"n/imp_detl_yearly_1", 
                      y, "n.dta"))), fill = TRUE)  
}


weights[, hs6 := floor(commodity / 10000)]

cw <- weights |>
  fgroup_by(year, hs6, naics) |>
  fsummarize(
    import_value = fsum(gen_val_yr, na.rm = TRUE)
  ) |>
  data.table()

setorder(cw, year, hs6, -import_value)

cw_1to1 <- cw[, .SD[1], by = .(year, hs6)]

fwrite(cw_1to1, paste0(path, "/peter_schott/weights_extended_collapsed.csv"))
fwrite(weights, paste0(path, "/peter_schott/weights_full.csv"))
  
# lodes data collapsed in 01b --------------------------------------------------

states <- tolower(state.abb)
years <- c(2002, 2007, 2013)

lodes_list <- list()

for (s in states) {
  for (y in years) {
    
    dt <- fread(
      paste0(
        path,
        "/lodes/clean_no_crosswalk/lodes_",
        s,
        "_",
        y,
        "_collapsed_no_crosswalk.csv"
      )
    )
    
    lodes_list[[length(lodes_list) + 1]] <- dt
  }
}

lodes <- rbindlist(lodes_list, fill = TRUE)

# sanity check
lodes[, .N, by = .(county, year)][N > 1]

fwrite(
  lodes,
  paste0(path, "/output/lodes_collapsed_all_no_crosswalk.csv")
)
