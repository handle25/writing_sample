################################################################################
# Created 8.10.2026
#
# Author: Sophie Handley
#
# Purpose:
# Pull LODES OD data, construct county-level commuting outcomes directly from
# block GEOIDs, and collapse to county x year.
#
# Outcomes include:
# - overall jobs
# - industry groups
# - age groups
# - earnings groups
#
# No external block-to-county crosswalk is used.
################################################################################

rm(list = ls())

path <- "D:/writing_sample/data"

states <- tolower(state.abb)
years <- c(2002, 2007, 2013)

################################################################################
# Output folder
################################################################################

dir.create(
  paste0(path, "/clean_no_crosswalk"),
  showWarnings = FALSE
)

################################################################################
# Pull and collapse
################################################################################

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
    
    ############################################################################
    # County FIPS directly from block GEOID
    ############################################################################
    
    lodes[, county :=
            as.integer(substr(as.character(h_geocode), 1, 5))]
    
    lodes[, w_county :=
            as.integer(substr(as.character(w_geocode), 1, 5))]
    
    ############################################################################
    # Outside-county indicator
    ############################################################################
    
    lodes[, outside :=
            fifelse(county == w_county, 0, 1)]
    
    ############################################################################
    # Outside-county jobs
    ############################################################################
    
    # overall
    lodes[, outside_jobs :=
            S000 * outside]
    
    # industry groups
    lodes[, outside_goods_jobs :=
            SI01 * outside]
    
    lodes[, outside_trade_jobs :=
            SI02 * outside]
    
    lodes[, outside_servc_jobs :=
            SI03 * outside]
    
    # age groups
    lodes[, outside_age29_jobs :=
            SA01 * outside]
    
    lodes[, outside_age30_54_jobs :=
            SA02 * outside]
    
    lodes[, outside_age55_jobs :=
            SA03 * outside]
    
    # earnings groups
    lodes[, outside_earn1250_jobs :=
            SE01 * outside]
    
    lodes[, outside_earn1251_3333_jobs :=
            SE02 * outside]
    
    lodes[, outside_earn3333_jobs :=
            SE03 * outside]
    
    ############################################################################
    # Collapse to home county x year
    ############################################################################
    
    outside <- lodes |>
      fgroup_by(county, year) |>
      fsummarize(
        
        # total jobs
        total_jobs = fsum(S000),
        
        # total industry jobs
        total_goods_jobs = fsum(SI01),
        total_trade_jobs = fsum(SI02),
        total_servc_jobs = fsum(SI03),
        
        # total age-group jobs
        total_age29_jobs = fsum(SA01),
        total_age30_54_jobs = fsum(SA02),
        total_age55_jobs = fsum(SA03),
        
        # total earnings-group jobs
        total_earn1250_jobs = fsum(SE01),
        total_earn1251_3333_jobs = fsum(SE02),
        total_earn3333_jobs = fsum(SE03),
        
        # outside overall
        outside_jobs = fsum(outside_jobs),
        
        # outside industry
        outside_goods_jobs = fsum(outside_goods_jobs),
        outside_trade_jobs = fsum(outside_trade_jobs),
        outside_servc_jobs = fsum(outside_servc_jobs),
        
        # outside age
        outside_age29_jobs = fsum(outside_age29_jobs),
        outside_age30_54_jobs = fsum(outside_age30_54_jobs),
        outside_age55_jobs = fsum(outside_age55_jobs),
        
        # outside earnings
        outside_earn1250_jobs = fsum(outside_earn1250_jobs),
        outside_earn1251_3333_jobs = fsum(outside_earn1251_3333_jobs),
        outside_earn3333_jobs = fsum(outside_earn3333_jobs)
      ) |>
      data.table()
    
    ############################################################################
    # Shares working outside county
    ############################################################################
    
    # overall
    outside[, outside_d_jobs :=
              outside_jobs / total_jobs]
    
    # industry
    outside[, outside_d_goods_jobs :=
              outside_goods_jobs / total_goods_jobs]
    
    outside[, outside_d_trade_jobs :=
              outside_trade_jobs / total_trade_jobs]
    
    outside[, outside_d_servc_jobs :=
              outside_servc_jobs / total_servc_jobs]
    
    # age
    outside[, outside_d_age29_jobs :=
              outside_age29_jobs / total_age29_jobs]
    
    outside[, outside_d_age30_54_jobs :=
              outside_age30_54_jobs / total_age30_54_jobs]
    
    outside[, outside_d_age55_jobs :=
              outside_age55_jobs / total_age55_jobs]
    
    # earnings
    outside[, outside_d_earn1250_jobs :=
              outside_earn1250_jobs / total_earn1250_jobs]
    
    outside[, outside_d_earn1251_3333_jobs :=
              outside_earn1251_3333_jobs /
              total_earn1251_3333_jobs]
    
    outside[, outside_d_earn3333_jobs :=
              outside_earn3333_jobs / total_earn3333_jobs]
    
    ############################################################################
    # Preserve requested period and source state
    ############################################################################
    
    outside[, target_year := y]
    
    outside[, state_str := s]
    
    ############################################################################
    # Save state x target year file
    ############################################################################
    
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

################################################################################
# Check uniqueness
################################################################################

dupes <- lodes_all[
  ,
  .N,
  by = .(county, year)
][N > 1]

print(dupes)

################################################################################
# Save national collapsed dataset
################################################################################

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

# overall
print(summary(lodes_all$outside_d_jobs))

# industry
print(summary(lodes_all$outside_d_goods_jobs))
print(summary(lodes_all$outside_d_trade_jobs))
print(summary(lodes_all$outside_d_servc_jobs))

# age
print(summary(lodes_all$outside_d_age29_jobs))
print(summary(lodes_all$outside_d_age30_54_jobs))
print(summary(lodes_all$outside_d_age55_jobs))

# earnings
print(summary(lodes_all$outside_d_earn1250_jobs))
print(summary(lodes_all$outside_d_earn1251_3333_jobs))
print(summary(lodes_all$outside_d_earn3333_jobs))

################################################################################