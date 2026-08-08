
rm(list = ls())

library(censusapi)
library(tidycensus)
library(data.table)
library(collapse)
library(janitor)
library(tidyr)
library(dplyr)
library(stringr)
library(jsonlite)
library(httr)
library(lehdr)
library(tidygeocoder)


path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data/lodes"

states <- tolower(state.abb) 
states <- c()
years <- seq(2002, 2020)

available <- list()
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


mi_files <- list.files(path, pattern = "^lodes_mi_.*\\.csv$", full.names = TRUE)

file_list <- list()

for (i in seq_along(mi_files)) {
  
  dt <- fread(mi_files[i])
  
  file_list[[i]] <- dt
  
}

mi <- rbindlist(file_list, fill = TRUE)
# save
fwrite(
  mi,
  paste0(path, "/clean/mi_full_2002_2020.csv")
)
