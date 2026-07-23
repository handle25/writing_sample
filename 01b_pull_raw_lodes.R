
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


path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data/lodes"

states <- tolower(state.abb) 
years <- seq(2002, 2020)

available <- list()

for (s in states) {
  for (y in years) {
    
    x <- tryCatch(
      grab_lodes(
        state = s,
        year = y,
        lodes_type = "od",
        download_dir = path
      ),
      error = function(e) NULL
    )
    
    if (!is.null(x)) {
      available[[length(available) + 1]] <- x
      print(paste("Success:", s, y))
    } else {
      print(paste("Missing:", s, y))
    }
  }
}
