rm(list = ls())

library(data.table)
library(lehdr)

path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data/lodes"

states <- tolower(state.abb)
years <- c(2002, 2007, 2013)

for (s in states) {
  for (y in years) {
    
    lodes <- tryCatch(
      grab_lodes(
        state = s,
        year = y,
        lodes_type = "od",
        download_dir = path
      ),
      error = function(e) NULL
    )
    
    if (!is.null(lodes)) {
      
      lodes <- data.table(lodes)
      lodes[, `:=`(state = s, year = y)]
      
      fwrite(
        lodes,
        paste0(path, "/lodes_", s, "_", y, ".csv")
      )
      
      rm(lodes)
      gc()
      
      print(paste("Saved:", s, y))
    }
  }
}



# has location county and census block 
crosswalk <- fread(paste0(path, "/../nhgis_blk2000_co2015/nhgis_blk2000_co2015.csv"))

# need to go from lodes census block -> crosswalk census block -> county -> qcew county 
crosswalk[,state:=floor(blk2000ge/1e13)]
crosswalk <- crosswalk[state == 26,]


years <- c(2002, 2007, 2013) 
for (i in 1:length(years)){
  year <- years[i]
  files <- list.files(path, pattern = paste0("^lodes_.*", year, ".csv$"), full.names = TRUE)
  
  file_list <- list()
  
  for (i in seq_along(files)) {
    
    dt <- fread(files[i])
    
    file_list[[i]] <- dt
    
  }
  
  
  full <- rbindlist(file_list, fill = TRUE)
  
  
  fwrite(
    full,
    paste0(path, "/clean/lodes_", year, ".csv")
  ) 
  
}

