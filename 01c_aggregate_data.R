path <- "D:/writing_sample/data"
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


# aggregate lodes for lps ------------------------------------------------------

states <- tolower(state.abb)
years <- c(2002:2023)

lodes_list <- list()

for (s in states) {
 
    dt <- fread(
      paste0(path,"/lodes/clean_lp_full/new_clean_lp_full/clean_lp_full_",s,".csv"))
    
    lodes_list[[length(lodes_list) + 1]] <- dt
}

lodes_baseline <- rbindlist(lodes_list, fill = TRUE)

lodes_2000 <- list()
for (s in states) {
  dt <- read_excel(
    paste0(
      path,
      "/lodes/clean_lp_full/census_2000_commuting/2kresco_", s, ".xls"
    ), skip = 3) |> 
    data.table()
  lodes_2000[[length(lodes_2000) + 1]] <- dt
}
lodes <- rbindlist(lodes_2000, fill = TRUE) |> clean_names()

# Construct 5-digit county FIPS
lodes[, county := as.integer(paste0(
  sprintf("%02d", as.integer(res_state)),
  sprintf("%03d", as.integer(res_county))
))]

lodes[, w_county := as.integer(paste0(
  sprintf("%02d", as.integer(work_state)),
  sprintf("%03d", as.integer(work_county))
))]

# Outside-county indicator
lodes[, outside := fifelse(county == w_county, 0, 1)]

# Outside jobs
lodes[, outside_jobs := count * outside]

# Collapse to home county
outside <- lodes[,
    .(total_jobs = sum(count, na.rm = TRUE),
    outside_jobs = sum(outside_jobs, na.rm = TRUE)
  ), by = county]

outside[, outside_d_jobs := outside_jobs / total_jobs]
outside[, year := 2000]
outside_2001 <- copy(outside) 
outside_2001[, year := 2001]

outside <- rbind(outside, outside_2001)
lodes <- rbind(lodes_baseline, outside, fill = TRUE)

fwrite(
  lodes,
  paste0(path, "/output/lp_new_lodes_collapsed_all_no_crosswalk.csv")
)



