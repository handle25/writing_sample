################################################################################
# Process QCEW data: 
#   Data go from individual county x yera files, appended together into a single 
# year file and then to a master file. 
# 
# Output: new_full_qcew_1995_2025.csv, county x industry x year 
################################################################################

# qcewdata 
path <- "D:/writing_sample/data/qcew"
setwd(path)

years <- c(1990:2025)

for (y in years) {
  
  qcew_list <- list()
  
  year_dir <- list.dirs(path)[grep(as.character(y), list.dirs(path))]
  
  setwd(year_dir[2])
  
  files <- list.files()
  
  for (i in seq(files)) {
    
    raw <- fread(files[i])
    raw[, year := y]   # add year while you are here
    
    qcew_list[[length(qcew_list) + 1]] <- raw
    
    rm(raw) 
    
    print(paste("Finished", y, files[i]))
  }
  
  dt <- rbindlist(qcew_list, fill = TRUE)
  
  # save
  fwrite(
    dt,
    paste0(path, "/clean/new_full_", y, ".csv")
  )
  
  # clear memory before next year
  rm(dt, qcew_list)
  gc()
  
  setwd(path)
}

qcew_naics3_list <- list()
qcew_naics2_list <- list()

for (y in years) {
  
  raw <- fread(paste0(path, "/clean/new_full_", y, ".csv"))
  
  for (lvl in c(75, 74)) {
    
    qcew_temp <- raw[agglvl_code == lvl]
    
    if ("annual_avg_estabs" %in% names(qcew_temp)) {
      setnames(qcew_temp, "annual_avg_estabs", "annual_avg_estabs_count")
    }
    
    qcew_temp <- qcew_temp |>
      fgroup_by(area_fips, industry_code, year) |>
      fsummarize(
        total_annual_wages = fsum(total_annual_wages),
        annual_avg_emplvl = fsum(annual_avg_emplvl),
        annual_avg_estabs_count = fsum(annual_avg_estabs_count)
      ) |>
      ungroup() |>
      data.table()
    
    if (lvl == 75) qcew_naics3_list[[length(qcew_naics3_list) + 1]] <- qcew_temp
    if (lvl == 74) qcew_naics2_list[[length(qcew_naics2_list) + 1]] <- qcew_temp
  }
}

qcew_naics3 <- rbindlist(qcew_naics3_list, fill = TRUE)
qcew_naics2 <- rbindlist(qcew_naics2_list, fill = TRUE)

fwrite(qcew_naics3, paste0(path, "/clean/new_full_qcew_naics3_1990_2025.csv"))
fwrite(qcew_naics2, paste0(path, "/clean/new_full_qcew_naics2_1990_2025.csv"))

rm(qcew_naics3, qcew_naics2, qcew_naics3_list, qcew_naics2_list)
gc()

setwd(path)