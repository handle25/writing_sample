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

library(data.table)
library(collapse)
library(lehdr)

################################################################################
# Paths
################################################################################

path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data/lodes"
path <- "D:/writing_sample/data/lodes/clean_lp_full"
output_dir <- paste0(path, "/new_clean_lp_full")

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

################################################################################
# States and years
################################################################################

states <- tolower(state.abb)

# Run California and Texas separately

years <- 2002:2025

################################################################################
# Pull and collapse
################################################################################

for (s in states) {
  
  print(paste("Starting state:",s))
  
  state_output_file <- paste0(output_dir,"/clean_lp_full_",s,".csv")
  state_list <- vector("list",length(years))
  k <- 1
  
  for (y in years) {
    print(paste("Starting:",s,y))
    year_result <- tryCatch({
      
      lodes <- grab_lodes(
        state = s,
        year = y,
        lodes_type = "od",
        state_part = "main",
        download_dir = path
      )
      
      lodes <- as.data.table(lodes)
      
      lodes[, year := y]
      
      lodes[, county := as.integer(substr(as.character(h_geocode), 1, 5))]
      lodes[, w_county := as.integer(substr(as.character(w_geocode), 1, 5))]
      
      # Outside-county indicator
      
      lodes[, outside := fifelse(county == w_county, 0, 1)]
      # Overall
      lodes[, outside_jobs := S000 * outside]
      
      # Industry groups
      lodes[, outside_goods_jobs := SI01 * outside]
      lodes[, outside_trade_jobs := SI02 * outside]
      lodes[, outside_servc_jobs := SI03 * outside]
      
      # Age groups
      lodes[, outside_age29_jobs := SA01 * outside]
      lodes[, outside_age30_54_jobs := SA02 * outside]
      lodes[, outside_age55_jobs := SA03 * outside]
      
      # Earnings groups
      lodes[, outside_earn1250_jobs := SE01 * outside]
      lodes[, outside_earn1251_3333_jobs := SE02 * outside]
      lodes[, outside_earn3333_jobs := SE03 * outside]
      
      # Collapse to home county x year
      
      outside <- lodes |>
        fgroup_by(county, year) |>
        fsummarize(
          
          # Total jobs
          total_jobs = fsum(S000),
          
          # Total industry jobs
          total_goods_jobs = fsum(SI01),
          total_trade_jobs = fsum(SI02),
          total_servc_jobs = fsum(SI03),
          
          # Total age-group jobs
          total_age29_jobs = fsum(SA01),
          total_age30_54_jobs = fsum(SA02),
          total_age55_jobs = fsum(SA03),
          
          # Total earnings-group jobs
          total_earn1250_jobs = fsum(SE01),
          total_earn1251_3333_jobs = fsum(SE02),
          total_earn3333_jobs = fsum(SE03),
          
          # Outside overall
          outside_jobs = fsum(outside_jobs),
          
          # Outside industry
          outside_goods_jobs = fsum(outside_goods_jobs),
          outside_trade_jobs = fsum(outside_trade_jobs),
          outside_servc_jobs = fsum(outside_servc_jobs),
          
          # Outside age
          outside_age29_jobs = fsum(outside_age29_jobs),
          outside_age30_54_jobs = fsum(outside_age30_54_jobs),
          outside_age55_jobs = fsum(outside_age55_jobs),
          
          # Outside earnings
          outside_earn1250_jobs = fsum(outside_earn1250_jobs),
          outside_earn1251_3333_jobs = fsum(outside_earn1251_3333_jobs),
          outside_earn3333_jobs = fsum(outside_earn3333_jobs)
        ) |>
        data.table()
      
      # Overall
      outside[, outside_d_jobs := outside_jobs / total_jobs]
      
      # Industry
      outside[, outside_d_goods_jobs := outside_goods_jobs / total_goods_jobs]
      outside[, outside_d_trade_jobs := outside_trade_jobs / total_trade_jobs]
      outside[, outside_d_servc_jobs := outside_servc_jobs / total_servc_jobs]
      
      # Age
      outside[, outside_d_age29_jobs := outside_age29_jobs / total_age29_jobs]
      outside[, outside_d_age30_54_jobs := outside_age30_54_jobs / total_age30_54_jobs]
      outside[, outside_d_age55_jobs := outside_age55_jobs / total_age55_jobs]
      
      # Earnings
      outside[, outside_d_earn1250_jobs := outside_earn1250_jobs / total_earn1250_jobs]
      outside[, outside_d_earn1251_3333_jobs := outside_earn1251_3333_jobs / total_earn1251_3333_jobs]
      outside[, outside_d_earn3333_jobs := outside_earn3333_jobs / total_earn3333_jobs]
     
      outside[, state_str := s]
      
      ##########################################################################
      # Clear raw LODES before returning collapsed data
      ##########################################################################
      
      rm(lodes)
      
      gc()
      
      outside
      
    }, error = function(e) {
      
      print(paste("ERROR - skipping:",s,y,"|", conditionMessage(e)))
      
      gc()
      
      NULL
    })
    
  
    if (!is.null(year_result)) {
      
      state_list[[k]] <- year_result
      
      k <- k + 1
      
      print(paste("Finished:",s,s))
    }
  }
 
  state_list <- state_list[
    !vapply(
      state_list,
      is.null,
      logical(1)
    )
  ]
  
  if (length(state_list) > 0) {
    
    lodes_state <- rbindlist(
      state_list,
      use.names = TRUE,
      fill = TRUE
    )
    
    print(paste("Years successfully pulled for",s,":",
        paste(sort(unique(lodes_state$year)),collapse = ", ")))
    
    fwrite(
      lodes_state,
      state_output_file
    )
    
    print(paste("Saved state panel:",s))
    rm(lodes_state,state_list)
    
    gc()
    
  } else {
    print(paste("No usable years found for:",s))
    rm(state_list)
    
    gc()
  }
}

# Finished

print("LODES state-level annual pull complete.")
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

# Residence county FIPS
usa_1990[, county :=
           res_state_num * 1000 + res_county_num
]

# Outside county of residence
usa_1990[, outside :=
           fifelse(
             res_state_num == work_state_num &
               res_county_num == work_county_num,
             0,
             1
           )
]


# # drop spacer columns
# usa_1990[, paste0("x", 1:11) := NULL]
# # numeric worker count
# usa_1990[, workers := as.numeric(trimws(workers))]
# 
# # residence county FIPS
# usa_1990[, county :=
#            as.integer(paste0(res_state, res_county))]
# 
# # working outside county of residence
# usa_1990[, outside :=
#            fifelse(
#              res_state == work_state &
#                res_county == work_county,
#              0,
#              1
#            )]

usa_1990[, outside_jobs := workers * outside]

outside_1990 <- usa_1990 |>
  fgroup_by(county) |>
  fsummarize(
    total_jobs   = fsum(workers),
    outside_jobs = fsum(outside_jobs)
  ) |>
  data.table()

outside_1990[, outside_d_jobs :=
               outside_jobs / total_jobs]

outside_1990[, year := 1990]

outside_1990 <- outside_1990[
  ,
  .(
    county,
    year,
    total_jobs,
    outside_jobs,
    outside_d_jobs
  )
]


fwrite(
  outside_1990,
  paste0(
    output_dir,
    "/1990_commuting_collapsed.csv"
  )
)
