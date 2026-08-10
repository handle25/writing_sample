################################################################################
# Created 8.10.2026
#
# Author: Sophie Handley
#
# Purpose:
# Pull LODES OD data, construct county-level commuting outcomes directly from
# block GEOIDs, and collapse to county x year.
#
# No external block-to-county crosswalk is used.
################################################################################

rm(list = ls())

library(data.table)
library(collapse)
library(lehdr)

path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data/lodes"

states <- tolower(state.abb)
years <- c(2002, 2007, 2013)

# output folder ---------------------------------------------------------------

dir.create(
  paste0(path, "/clean_no_crosswalk"),
  showWarnings = FALSE
)

# pull and collapse -----------------------------------------------------------

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
    
    # actual LODES year used
    lodes[, year := try_year]
    
    # county FIPS directly from block GEOID -----------------------------------
    
    lodes[, county :=
            as.integer(substr(as.character(h_geocode), 1, 5))]
    
    lodes[, w_county :=
            as.integer(substr(as.character(w_geocode), 1, 5))]
    
    # outside-county indicator ------------------------------------------------
    
    lodes[, outside :=
            fifelse(county == w_county, 0, 1)]
    
    # outside-county jobs ------------------------------------------------------
    
    lodes[, outside_jobs :=
            S000 * outside]
    
    lodes[, outside_goods_jobs :=
            SI01 * outside]
    
    lodes[, outside_trade_jobs :=
            SI02 * outside]
    
    lodes[, outside_servc_jobs :=
            SI03 * outside]
    
    # collapse to home county x year ------------------------------------------
    
    outside <- lodes |>
      fgroup_by(county, year) |>
      fsummarize(
        total_jobs = fsum(S000),
        total_goods_jobs = fsum(SI01),
        total_trade_jobs = fsum(SI02),
        total_servc_jobs = fsum(SI03),
        
        outside_jobs = fsum(outside_jobs),
        outside_goods_jobs = fsum(outside_goods_jobs),
        outside_trade_jobs = fsum(outside_trade_jobs),
        outside_servc_jobs = fsum(outside_servc_jobs)
      ) |>
      data.table()
    
    # shares working outside county ------------------------------------------
    
    outside[, outside_d_jobs :=
              outside_jobs / total_jobs]
    
    outside[, outside_d_goods_jobs :=
              outside_goods_jobs / total_goods_jobs]
    
    outside[, outside_d_trade_jobs :=
              outside_trade_jobs / total_trade_jobs]
    
    outside[, outside_d_servc_jobs :=
              outside_servc_jobs / total_servc_jobs]
    
    # preserve requested period and source state ------------------------------
    
    outside[, target_year := y]
    
    outside[, state_str := s]
    
    # save state x target year file ------------------------------------------
    
    fwrite(
      outside,
      paste0(
        path,
        "/clean_no_crosswalk/lodes_",
        s,
        "_",
        y,
        "_collapsed_no_crosswalk.csv"
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

################################################################################
# Combine all collapsed files
################################################################################

files <- list.files(
  paste0(path, "/clean_no_crosswalk"),
  pattern = "_collapsed_no_crosswalk\\.csv$",
  full.names = TRUE
)

lodes_all <- rbindlist(
  lapply(files, fread),
  fill = TRUE
)

# check uniqueness ------------------------------------------------------------

dupes <- lodes_all[
  ,
  .N,
  by = .(county, year)
][N > 1]

print(dupes)

# save national collapsed dataset --------------------------------------------

fwrite(
  lodes_all,
  paste0(
    path,
    "/../output/lodes_collapsed_all_no_crosswalk.csv"
  )
)

################################################################################
# Basic checks
################################################################################

print(table(lodes_all$year))
print(table(lodes_all$state_str))

print(summary(lodes_all$outside_d_jobs))
print(summary(lodes_all$outside_d_goods_jobs))
print(summary(lodes_all$outside_d_trade_jobs))
print(summary(lodes_all$outside_d_servc_jobs))

################################################################################