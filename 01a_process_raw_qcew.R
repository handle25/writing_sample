
# qcewdata 
path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data/qcew"
setwd(path)

years <- seq(1995,2014)


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
    paste0(path, "/clean/full_", y, ".csv")
  )
  
  # clear memory before next year
  rm(dt, qcew_list)
  gc()
  
  setwd(path)
}

qcew_list <- list()

for (y in years) {
  
  raw <- fread(paste0(path, "/clean/full_", y, ".csv"))
  
  qcew_list[[length(qcew_list) + 1]] <- raw
}

dt <- rbindlist(qcew_list, fill = TRUE)

# save
fwrite(
  dt,
  paste0(path, "/clean/full_qcew_1990_2023.csv")
)

# clear memory before next year
rm(dt, qcew_list)
gc()

setwd(path)

subset <- dt[year %in% c(1995, 1996, 1997)]

