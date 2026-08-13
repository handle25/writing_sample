path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
# using U.S. HS10 import values to select the dominant SIC within each HS6
weights <- data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_91n/imp_detl_yearly_91n.dta")))
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_95n/imp_detl_yearly_95n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_100n/imp_detl_yearly_100n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_107n/imp_detl_yearly_107n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_113n/imp_detl_yearly_113n.dta"))), fill = TRUE)
fwrite(weights, paste0(path, "/peter_schott/weights.csv"))
  
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
