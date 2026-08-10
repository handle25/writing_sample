rm(list = ls())

path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data/lodes"

states <- c("mi", "oh", "in")
years <- c(2002, 2007, 2013)


for (s in states) {
  
  for (y in years) {
    
    try_year <- y
    lodes <- NULL
    
    # same fallback rule as main pipeline
    while (is.null(lodes) && try_year <= y + 5) {
      
      lodes <- tryCatch(
        grab_lodes(
          state = s,
          year = try_year,
          lodes_type = "od",
          state_part = "main",
          download_dir = path
        ),
        error = function(e) {
          print(paste("Failed:", s, try_year))
          NULL
        }
      )
      
      if (is.null(lodes)) {
        try_year <- try_year + 1
      }
    }
    
    if (is.null(lodes)) {
      print(paste(
        "No usable year found for",
        s,
        "starting at",
        y
      ))
      next
    }
    
    lodes <- as.data.table(lodes)
    
    # useful identifiers so you know requested vs actual year
    lodes[, target_year := y]
    lodes[, year := try_year]
    lodes[, state_str := s]
    
    fwrite(
      lodes,
      paste0(
        path,
        "/full_test/lodes_",
        s, "_",
        y,
        "_full.csv"
      )
    )
    
    print(paste(
      "Saved:", s,
      "target =", y,
      "actual =", try_year,
      "N =", nrow(lodes)
    ))
    
    rm(lodes)
    gc()
  }
}



for (s in states) {
  lodes_list <- list()
  
  
  for (y in years) {
    
    dt <- fread(
      paste0(
        path,
        "/full_test/lodes_",
        s, "_",
        y,
        "_full.csv"
      )
    )
    
    dt[, state_str := s]
    
    lodes_list[[length(lodes_list) + 1]] <- dt
  }
  lodes <- rbindlist(lodes_list, fill = TRUE)
  
  fwrite(lodes, paste0(path, "/full_test/lodes_subset_", s, "_test.csv"))
}


