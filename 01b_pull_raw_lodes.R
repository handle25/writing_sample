rm(list = ls())

library(data.table)
library(lehdr)
library(bit64)

path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data/lodes"

states <- tolower(state.abb)
years <- c(2002, 2007, 2013)

# make sure output directory exists
dir.create(
  paste0(path, "/clean"),
  showWarnings = FALSE
)

# load crosswalk once
crosswalk <- fread(
  paste0(
    path,
    "/../nhgis_blk2000_co2015/nhgis_blk2000_co2015.csv"
  )
)

# make block GEOID type match LODES
crosswalk[, blk2000ge := as.integer64(blk2000ge)]

for (s in states) {
  
  for (y in years) {
    
    try_year <- y
    lodes <- NULL
    
    # keep trying later years, up to 5 years ahead
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
    
    # if nothing worked within 5 years, skip
    if (is.null(lodes)) {
      print(
        paste(
          "No usable year found for",
          s,
          "starting at",
          y
        )
      )
      next
    }
    
    print(
      paste(
        "Using:",
        s,
        try_year,
        "for target year",
        y
      )
    )
    
    lodes <- as.data.table(lodes)
    
    # convert block GEOIDs to integer64
    lodes[, h_geocode := as.integer64(h_geocode)]
    lodes[, w_geocode := as.integer64(w_geocode)]
    
    # actual LODES year used
    lodes[, year := try_year]
    
    # home block -> county
    lodes <- merge(
      lodes,
      crosswalk,
      by.x = "h_geocode",
      by.y = "blk2000ge"
    )
    
    setnames(
      lodes,
      "co2015ge",
      "h_county"
    )
    
    # workplace block -> county
    lodes <- merge(
      lodes,
      crosswalk,
      by.x = "w_geocode",
      by.y = "blk2000ge"
    )
    
    setnames(
      lodes,
      "co2015ge",
      "w_county"
    )
    
    # jobs where home county != workplace county
    lodes[
      ,
      outside := fifelse(
        h_county != w_county,
        S000,
        0
      )
    ]
    
    # collapse to home county x year
    outside <- lodes[
      ,
      .(
        all_jobs = sum(S000),
        outside_jobs = sum(outside)
      ),
      by = .(h_county, year)
    ]
    
    # share of employed residents working outside their county
    outside[
      ,
      outside_d_jobs := outside_jobs / all_jobs
    ]
    
    # preserve the target period requested
    outside[, target_year := y]
    
    # save only collapsed file
    fwrite(
      outside,
      paste0(
        path,
        "/clean/lodes_",
        s,
        "_",
        y,
        "_collapsed.csv"
      )
    )
    
    rm(lodes, outside)
    gc()
    
    print(
      paste(
        "Finished:",
        s,
        "target =",
        y,
        "actual =",
        try_year
      )
    )
  }
}































# 
# for (s in states) {
#   for (y in years) {
#     
#     lodes <- tryCatch(
#       grab_lodes(
#         state = s,
#         year = y,
#         lodes_type = "od",
#         download_dir = path
#       ),
#       error = function(e) NULL
#     )
#     
#     if (!is.null(lodes)) {
#       
#       lodes <- data.table(lodes)
#       lodes[, `:=`(state = s, year = y)]
#       
#       # fwrite(
#       #   lodes,
#       #   paste0(path, "/lodes_", s, "_", y, ".csv")
#       # )
#       
#       # has location county and census block 
#       crosswalk <- fread(paste0(path, "/../nhgis_blk2000_co2015/nhgis_blk2000_co2015.csv"))
#       
#       # need to go from lodes census block -> crosswalk census block -> county -> qcew county 
#       crosswalk[,state:=floor(blk2000ge/1e13)]
#       
#       # merge on block group for home and workplace 
#       lodes <- merge(lodes, crosswalk, by.x = "h_geocode", by.y = "blk2000ge")
#       setnames(lodes, "co2015ge", "h_county")
#       lodes <- merge(lodes, crosswalk, by.x = "w_geocode", by.y = "blk2000ge")
#       setnames(lodes, "co2015ge", "w_county")
#       
#       lodes[,outside := ifelse(h_county != w_county, S000, 0)]
#       
#       outside <- lodes |> fgroup_by(h_county, year) |>
#         fsummarise(all_jobs = fsum(S000),
#                    outside_jobs = fsum(outside)) 
#       
#       outside[,outside_d_jobs := outside_jobs / all_jobs]
#       
#       
#       rm(lodes)
#       gc()
#       
#       print(paste("Saved:", s, y))
#     }
#   }
# }
# 
# 
# 
# # has location county and census block 
# crosswalk <- fread(paste0(path, "/../nhgis_blk2000_co2015/nhgis_blk2000_co2015.csv"))
# 
# # need to go from lodes census block -> crosswalk census block -> county -> qcew county 
# crosswalk[,state:=floor(blk2000ge/1e13)]
# 
# 
# years <- c(2002, 2007, 2013) 
# for (i in 1:length(years)){
#   year <- years[i]
#   files <- list.files(path, pattern = paste0("^lodes_.*", year, ".csv$"), full.names = TRUE)
#   
#   file_list <- list()
#   
#   for (i in seq_along(files)) {
#     
#     lodes <- fread(files[i])
#     
#     # collapse to a sum of all jobs outside of the county 
#     
#     # merge on block group for home and workplace 
#     lodes <- merge(lodes, crosswalk, by.x = "h_geocode", by.y = "blk2000ge")
#     setnames(lodes, "co2015ge", "h_county")
#     lodes <- merge(lodes, crosswalk, by.x = "w_geocode", by.y = "blk2000ge")
#     setnames(lodes, "co2015ge", "w_county")
#     
#     lodes[,outside := ifelse(h_county != w_county, S000, 0)]
#     
#     outside <- lodes |> fgroup_by(h_county, year) |>
#       fsummarise(all_jobs = fsum(S000),
#                  outside_jobs = fsum(outside)) 
#     
#     outside[,outside_d_jobs := outside_jobs / all_jobs]
#     
#     file_list[[i]] <- outside
#     
#   }
#   
#   full <- rbindlist(file_list, fill = TRUE)
# 
#   fwrite(
#     full,
#     paste0(path, "/clean/lodes_", year, "_collapsed.csv")
#   ) 
# }
# 
