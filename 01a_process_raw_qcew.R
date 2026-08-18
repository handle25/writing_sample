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

years <- seq(1995,2025)

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


qcew_list <- list()

for (y in years) {
  
  raw <- fread(paste0(path, "/clean/new_full_", y, ".csv"))
  qcew_naics3 <- raw[agglvl_code == 75]
  if ("annual_avg_estabs" %in% names(qcew_naics3)) {
    setnames(qcew_naics3, "annual_avg_estabs", "annual_avg_estabs_count")
  }
  
  # collapse to county x industry x year
  qcew_naics3 <- qcew_naics3 |>
    fgroup_by(area_fips, industry_code, year) |>
    fsummarize(
      total_annual_wages = fsum(total_annual_wages),
      annual_avg_emplvl = fsum(annual_avg_emplvl),
      annual_avg_estabs_count = fsum(annual_avg_estabs_count)
    ) |>
    ungroup() |>
    data.table()
  
  qcew_list[[length(qcew_list) + 1]] <- qcew_naics3
}

dt <- rbindlist(qcew_list, fill = TRUE)

# save
fwrite(
  dt,
  paste0(path, "/clean/new_full_qcew_1995_2025.csv")
)

# clear memory before next year
rm(dt, qcew_list)
gc()

setwd(path)


