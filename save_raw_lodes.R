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

data_path <- "D:/writing_sample/data/lodes"
output_dir <- paste0(path, "/raw")

# States and years -------------------------------------------------------------
states <- tolower(state.abb)
years <- 2002:2025
# Pull and collapse ------------------------------------------------------------
for (s in states) {
  
  print(paste("Starting state:", s))
  
  state_output_file <- paste0(output_dir, "/county_od_lodes_", s, ".csv")
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
        state_part="main",
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
  
  # Remove empty list elements
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

## 2000 ------------------------------------------------------------------------
files_2000 <- list.files(
  paste0(path, "/census_2000_commuting"),
  pattern="\\.xls$",
  full.names=TRUE
)

# Read and combine all state files ---------------------------------------------
usa_2000 <- rbindlist(lapply(files_2000, function(f) {
  
  x <- read_excel(f, skip=5, col_names=FALSE)
  x <- as.data.table(x)
  
  # Residence state/county, workplace state/county, worker count
  x <- x[, .(
    res_state=as.integer(...1),
    res_county=as.integer(...2),
    work_state=as.integer(...6),
    work_county=as.integer(...7),
    workers=as.numeric(...11)
  )]
  
  x[!is.na(res_state) & !is.na(res_county) &
      !is.na(work_state) & !is.na(work_county) &
      !is.na(workers)]
}))

usa_2000[, `:=`(
  county=as.integer(sprintf("%02d%03d",res_state,res_county)),
  w_county=as.integer(sprintf("%02d%03d",work_state,work_county)),
  year=2000
)]

setnames(usa_2000,"workers","S000")
usa_2000 <- usa_2000[, .(S000=sum(S000,na.rm=TRUE)), by=.(county,w_county,year)]

## 1990 ------------------------------------------------------------------------
options(timeout = 600)
url <- "https://www2.census.gov/programs-surveys/commuting/datasets/1990/worker-flow/usresco.txt"

usa_1990 <- read.fwf(
  url,
  widths = c(
    2, 1, 3, 1, 3, 1, 4, 1, 5, 1,
    3, 1, 3, 1, 3, 1, 4, 1, 5, 1,
    9, 1, 30, 28
  ),
  col.names = c(
    "res_state", "x1",
    "res_county", "x2",
    "res_mcd", "x3",
    "res_msa", "x4",
    "res_mcd_fips", "x5",
    "work_state", "x6",
    "work_county", "x7",
    "work_mcd", "x8",
    "work_msa", "x9",
    "work_mcd_fips", "x10",
    "workers", "x11",
    "res_name",
    "work_name"
  ),
  colClasses = "character"
)

usa_1990 <- as.data.table(usa_1990)
usa_1990[, paste0("x", 1:11) := NULL]

usa_1990[, workers := as.numeric(trimws(workers))]

# Convert geography codes to numeric
usa_1990[, res_state_num   := as.integer(trimws(res_state))]
usa_1990[, res_county_num  := as.integer(trimws(res_county))]
usa_1990[, work_state_num  := as.integer(trimws(work_state))]
usa_1990[, work_county_num := as.integer(trimws(work_county))]

usa_1990[, `:=`(
  county=as.integer(sprintf("%02d%03d",res_state_num,res_county_num)),
  w_county=as.integer(sprintf("%02d%03d",work_state_num,work_county_num)),
  year=1990
)]

setnames(usa_1990,"workers","S000")
usa_1990 <- usa_1990[, .(S000=sum(S000,na.rm=TRUE)), by=.(county,w_county,year)]

# save all files into one appended file ----------------------------------------
files <- list.files(
  output_dir,
  pattern="^county_od_lodes_.*\\.csv$",
  full.names=TRUE
)

lodes_2002_2025 <- rbindlist(lapply(files,fread),use.names=TRUE,fill=TRUE)

full_lodes <- rbindlist(
  list(lodes_2002_2025,usa_2000,usa_1990),
  use.names=TRUE,
  fill=TRUE
)

setorder(full_lodes,county,year,w_county)

stopifnot(nrow(usa_2000[, .N, by=.(county,w_county,year)][N > 1]) == 0)
stopifnot(nrow(usa_1990[, .N, by=.(county,w_county,year)][N > 1]) == 0)

usa_2000[]
usa_1990[]


fwrite(
  full_lodes,
  paste0(output_dir,"/full_lodes_data_1990-2025.csv")
)
