################################################################################
# Created 8.10.2026
#
# Author: Sophie Handley
#
# Purpose:
# Pull annual LODES OD data, construct county-level commuting outcomes directly
# from block GEOIDs, collapse to county x year, and save one panel per state.
#
# Intended for annual / local projection analysis.
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

################################################################################
# Paths
################################################################################
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")
data_path <- paste0(path, "/lodes")
path <- data_path
output_dir <- paste0(path, "/raw")

# States and years -------------------------------------------------------------
states <- tolower(state.abb)
years <- 2002:2025
states <- "ca"

# Pull and collapse ------------------------------------------------------------
for (s in states) {
  
  print(paste("Starting state:", s))
  
  state_output_file <- paste0(output_dir, "/aux_county_od_lodes_", s, ".csv")
  state_list <- vector("list", length(years))
  k <- 1
  
  for (y in years) {
    
    print(paste("Starting:", s, y))
    
    year_result <- tryCatch({
      
      # Pull block-level OD data
      lodes <- grab_lodes(
        state=s,
        year=y,
        lodes_type="od",
        state_part="aux",
        download_dir=path
      )
      
      lodes <- as.data.table(lodes)
      
      # Home and workplace county FIPS
      lodes[, `:=`(
        year=y,
        county=as.integer(substr(as.character(h_geocode), 1, 5)),
        w_county=as.integer(substr(as.character(w_geocode), 1, 5))
      )]
      
      # Collapse block OD -> county OD
      lodes <- lodes[, .(
        S000=sum(S000, na.rm=TRUE),
        
        # Industry
        SI01=sum(SI01, na.rm=TRUE),
        SI02=sum(SI02, na.rm=TRUE),
        SI03=sum(SI03, na.rm=TRUE),
        
        # Age
        SA01=sum(SA01, na.rm=TRUE),
        SA02=sum(SA02, na.rm=TRUE),
        SA03=sum(SA03, na.rm=TRUE),
        
        # Earnings
        SE01=sum(SE01, na.rm=TRUE),
        SE02=sum(SE02, na.rm=TRUE),
        SE03=sum(SE03, na.rm=TRUE)
        
      ), by=.(county, w_county, year)]
      
      lodes[, state_str := s]
      
      gc()
      
      lodes
      
    }, error=function(e) {
      
      print(paste("ERROR - skipping:", s, y, "|", conditionMessage(e)))
      
      gc()
      
      NULL
    })
    
    if (!is.null(year_result)) {
      
      state_list[[k]] <- year_result
      k <- k + 1
      
      print(paste("Finished:", s, y))
    }
  }
  
  # Remove empty list elementsc
  state_list <- state_list[
    !vapply(state_list, is.null, logical(1))
  ]
  
  if (length(state_list) > 0) {
    
    lodes_state <- rbindlist(
      state_list,
      use.names=TRUE,
      fill=TRUE
    )
    
    print(paste(
      "Years successfully pulled for", s, ":",
      paste(sort(unique(lodes_state$year)), collapse=", ")
    ))
    
    # Check uniqueness of county OD cells
    duplicates <- lodes_state[, .N, by=.(county, w_county, year)][N > 1]
    
    if (nrow(duplicates) > 0) {
      warning(paste("Duplicate county OD cells found for:", s))
    }
    
    # Save reusable county-to-county OD panel
    fwrite(
      lodes_state,
      state_output_file
    )
    
    print(paste("Saved state panel:", s))
    
    rm(lodes_state, state_list, duplicates)
    gc()
    
  } else {
    
    print(paste("No usable years found for:", s))
    
    rm(state_list)
    gc()
  }
}

# Pull and collapse ------------------------------------------------------------
## 2000 ------------------------------------------------------------------------
base_url <- paste0(
  "https://www2.census.gov/programs-surveys/decennial/",
  "tables/2000/county-to-county-worker-flow-files/"
)

states <- tolower(c(state.abb, "dc"))

dir.create(
  paste0(path, "/census_2000_commuting"),
  showWarnings = FALSE
)

for (st in states) {
  
  url <- paste0(
    base_url,
    "2kresco_", st, ".xls"
  )
  
  dest <- paste0(
    path,
    "/census_2000_commuting/2kresco_", st, ".xls"
  )
  
  try(
    download.file(
      url,
      destfile = dest,
      mode = "wb"
    )
  )
}

# save all files into one appended file ----------------------------------------
files <- list.files(
  output_dir,
  pattern="^aux_county_od_lodes_.*\\.csv$",
  full.names=TRUE
)

lodes_2002_2025 <- rbindlist(lapply(files,fread),use.names=TRUE,fill=TRUE)

full_lodes <- rbindlist(
  list(lodes_2002_2025),
  use.names=TRUE,
  fill=TRUE
)

setorder(full_lodes,county,year,w_county)

fwrite(
  full_lodes,
  paste0(output_dir,"/aux_full_lodes_data_1990-2025.csv")
)
