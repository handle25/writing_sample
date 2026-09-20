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

# Paths
path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data/lodes"
path <- "D:/writing_sample/data/lodes/clean_lp_full"
output_dir <- paste0(path, "/new_clean_lp_full")
shock <- fread(paste0(path, "/../../output/lp_transformed_reg.csv")) |> 
  fselect(year, IPW_US, IPW_OTH, area_fips) 

url <- "https://www2.census.gov/geo/docs/reference/county_adjacency/county_adjacency2010.txt"

neighbors <- fread(url, sep="\t", fill=TRUE, header=FALSE,
                   col.names=c("county_name", "county", "neighbor_name", "neighbor"))

neighbors[, county := nafill(county, type="locf")]
neighbors <- neighbors[, .(county, neighbor)]
neighbors[, neighbor_merge := 1 ]

# States and years
states <- tolower(state.abb)
years <- 2002:2025

# Pull and collapse

for (s in states) {
  
  print(paste("Starting state:",s))
  
  state_output_file <- paste0(output_dir,"/measure_clean_lp_full_",s,".csv")
  state_list <- vector("list",length(years))
  k <- 1
  
  for (y in years) {
    print(paste("Starting:",s,y))
    
    shock_year <- shock[year==y,]
    
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
      
      # Collapse to home county-county x year
      
      outside <- lodes |>
        fgroup_by(county, w_county, year) |>
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
          total_earn3333_jobs = fsum(SE03)
        ) |>
        data.table()
      
      # merge on the neighboring counties 
      outside <- merge(outside, shock_year, 
                       by.x = c("county", "year"),
                       by.y = c("area_fips", "year"), 
                       all.x = T)
      setnames(outside, c("IPW_US", "IPW_OTH"), c("county_IPW_US", "county_IPW_OTH"))
      
      outside <- merge(outside, shock_year, 
                       by.x = c("w_county", "year"),
                       by.y = c("area_fips", "year"), 
                       all.x = T)
      setnames(outside, c("IPW_US", "IPW_OTH"), c("neighbor_IPW_US", "neighbor_IPW_OTH"))
      
      outside[, neighbor_less_exposure := 
                as.integer(neighbor_IPW_US < county_IPW_US)]
      
      # External commuting network ---------------------------------------------------
      external_network_shock <- merge(outside, neighbors,
                                      by.x=c("county", "w_county"),
                                      by.y=c("county", "neighbor"), all.x=TRUE)
      
      external_network_shock[is.na(neighbor_merge), neighbor_merge := 0L]
      external_network_shock <- external_network_shock[county != w_county]
      
      external_network_shock <- external_network_shock[, .(
        # All jobs
        ex_mean_dest_IPW_US = weighted.mean(neighbor_IPW_US, total_jobs, na.rm=TRUE),
        ex_mean_dest_IPW_OTH = weighted.mean(neighbor_IPW_OTH, total_jobs, na.rm=TRUE),
        ex_share_less_exposed = weighted.mean(neighbor_less_exposure, total_jobs, na.rm=TRUE),
        ex_share_neighbor_commuters = weighted.mean(neighbor_merge, total_jobs, na.rm=TRUE),
        
        # Goods jobs
        ex_goods_mean_dest_IPW_US = weighted.mean(neighbor_IPW_US, total_goods_jobs, na.rm=TRUE),
        ex_goods_mean_dest_IPW_OTH = weighted.mean(neighbor_IPW_OTH, total_goods_jobs, na.rm=TRUE),
        ex_goods_share_less_exposed = weighted.mean(neighbor_less_exposure, total_goods_jobs, na.rm=TRUE),
        ex_goods_share_neighbor_commuters = weighted.mean(neighbor_merge, total_goods_jobs, na.rm=TRUE),
        
        # Service jobs
        ex_servc_mean_dest_IPW_US = weighted.mean(neighbor_IPW_US, total_servc_jobs, na.rm=TRUE),
        ex_servc_mean_dest_IPW_OTH = weighted.mean(neighbor_IPW_OTH, total_servc_jobs, na.rm=TRUE),
        ex_servc_share_less_exposed = weighted.mean(neighbor_less_exposure, total_servc_jobs, na.rm=TRUE),
        ex_servc_share_neighbor_commuters = weighted.mean(neighbor_merge, total_servc_jobs, na.rm=TRUE)
      ), by=county]
      
      
      # Full employment network ------------------------------------------------------
      full_network_shock <- merge(outside, neighbors,
                                  by.x=c("county", "w_county"),
                                  by.y=c("county", "neighbor"), all.x=TRUE)
      
      full_network_shock[is.na(neighbor_merge), neighbor_merge := 0L]
      
      full_network_shock <- full_network_shock[, .(
        # All jobs
        fn_mean_dest_IPW_US = weighted.mean(neighbor_IPW_US, total_jobs, na.rm=TRUE),
        fn_mean_dest_IPW_OTH = weighted.mean(neighbor_IPW_OTH, total_jobs, na.rm=TRUE),
        fn_share_less_exposed = weighted.mean(neighbor_less_exposure, total_jobs, na.rm=TRUE),
        fn_share_neighbor_commuters = weighted.mean(neighbor_merge, total_jobs, na.rm=TRUE),
        
        # Goods jobs
        fn_goods_mean_dest_IPW_US = weighted.mean(neighbor_IPW_US, total_goods_jobs, na.rm=TRUE),
        fn_goods_mean_dest_IPW_OTH = weighted.mean(neighbor_IPW_OTH, total_goods_jobs, na.rm=TRUE),
        fn_goods_share_less_exposed = weighted.mean(neighbor_less_exposure, total_goods_jobs, na.rm=TRUE),
        fn_goods_share_neighbor_commuters = weighted.mean(neighbor_merge, total_goods_jobs, na.rm=TRUE),
        
        # Service jobs
        fn_servc_mean_dest_IPW_US = weighted.mean(neighbor_IPW_US, total_servc_jobs, na.rm=TRUE),
        fn_servc_mean_dest_IPW_OTH = weighted.mean(neighbor_IPW_OTH, total_servc_jobs, na.rm=TRUE),
        fn_servc_share_less_exposed = weighted.mean(neighbor_less_exposure, total_servc_jobs, na.rm=TRUE),
        fn_servc_share_neighbor_commuters = weighted.mean(neighbor_merge, total_servc_jobs, na.rm=TRUE)
      ), by=county]
      
      outside <- merge(full_network_shock, 
                       external_network_shock,
                       by = "county", all = T)
      outside[, `:=`(year=y, state_str=s)]
      
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

# Construct county FIPS --------------------------------------------------------
usa_2000[, county := res_state * 1000 + res_county]
usa_2000[, w_county := work_state * 1000 + work_county]

# Collapse to county x work-county ---------------------------------------------
outside_2000 <- usa_2000[, .(
  total_jobs=sum(workers, na.rm=TRUE)
), by=.(county, w_county)]

outside_2000[, year := 2000]

# Merge home and destination shocks --------------------------------------------
shock_year <- shock[year == 2000]

outside_2000 <- merge(outside_2000, shock_year,
                      by.x=c("county", "year"),
                      by.y=c("area_fips", "year"), all.x=TRUE)
setnames(outside_2000, c("IPW_US", "IPW_OTH"), c("county_IPW_US", "county_IPW_OTH"))

outside_2000 <- merge(outside_2000, shock_year,
                      by.x=c("w_county", "year"),
                      by.y=c("area_fips", "year"), all.x=TRUE)
setnames(outside_2000, c("IPW_US", "IPW_OTH"), c("neighbor_IPW_US", "neighbor_IPW_OTH"))

outside_2000[, neighbor_less_exposure :=
               as.integer(neighbor_IPW_US < county_IPW_US)]

# External commuting network ---------------------------------------------------
external_network_shock <- merge(outside_2000, neighbors,
                                by.x=c("county", "w_county"),
                                by.y=c("county", "neighbor"), all.x=TRUE)

external_network_shock[is.na(neighbor_merge), neighbor_merge := 0L]
external_network_shock <- external_network_shock[county != w_county]

external_network_shock <- external_network_shock[, .(
  ex_mean_dest_IPW_US = weighted.mean(neighbor_IPW_US, total_jobs, na.rm=TRUE),
  ex_mean_dest_IPW_OTH = weighted.mean(neighbor_IPW_OTH, total_jobs, na.rm=TRUE),
  ex_share_less_exposed = weighted.mean(neighbor_less_exposure, total_jobs, na.rm=TRUE),
  ex_share_neighbor_commuters = weighted.mean(neighbor_merge, total_jobs, na.rm=TRUE)
), by=county]

# Full employment network ------------------------------------------------------
full_network_shock <- merge(outside_2000, neighbors,
                            by.x=c("county", "w_county"),
                            by.y=c("county", "neighbor"), all.x=TRUE)

full_network_shock[is.na(neighbor_merge), neighbor_merge := 0L]

full_network_shock <- full_network_shock[, .(
  # All jobs
  fn_mean_dest_IPW_US = weighted.mean(neighbor_IPW_US, total_jobs, na.rm=TRUE),
  fn_mean_dest_IPW_OTH = weighted.mean(neighbor_IPW_OTH, total_jobs, na.rm=TRUE),
  fn_share_less_exposed = weighted.mean(neighbor_less_exposure, total_jobs, na.rm=TRUE),
  fn_share_neighbor_commuters = weighted.mean(neighbor_merge, total_jobs, na.rm=TRUE)
), by=county]

# Final county x year file -----------------------------------------------------
outside_2000 <- merge(full_network_shock, external_network_shock,
                      by="county", all=TRUE)

outside_2000[, year := 2000]

fwrite(
  outside_2000,
  paste0(output_dir, "/2000_commuting_measures.csv")
)


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

# Residence and workplace county FIPS ------------------------------------------
usa_1990[, county := res_state_num * 1000 + res_county_num]
usa_1990[, w_county := work_state_num * 1000 + work_county_num]

# Collapse to county x work-county ---------------------------------------------
outside_1990 <- usa_1990[
  !is.na(county) & !is.na(w_county),
  .(total_jobs = sum(workers, na.rm=TRUE)),
  by=.(county, w_county)
]

outside_1990[, year := 1990]

# Merge home and destination shocks --------------------------------------------
shock_year <- shock[year == 1990]

outside_1990 <- merge(outside_1990, shock_year,
                      by.x=c("county", "year"),
                      by.y=c("area_fips", "year"), all.x=TRUE)
setnames(outside_1990, c("IPW_US", "IPW_OTH"), c("county_IPW_US", "county_IPW_OTH"))

outside_1990 <- merge(outside_1990, shock_year,
                      by.x=c("w_county", "year"),
                      by.y=c("area_fips", "year"), all.x=TRUE)
setnames(outside_1990, c("IPW_US", "IPW_OTH"), c("neighbor_IPW_US", "neighbor_IPW_OTH"))

outside_1990[, neighbor_less_exposure :=
               as.integer(neighbor_IPW_US < county_IPW_US)]

# External commuting network ---------------------------------------------------
external_network_shock <- merge(outside_1990, neighbors,
                                by.x=c("county", "w_county"),
                                by.y=c("county", "neighbor"), all.x=TRUE)

external_network_shock[is.na(neighbor_merge), neighbor_merge := 0L]
external_network_shock <- external_network_shock[county != w_county]

external_network_shock <- external_network_shock[, .(
  ex_mean_dest_IPW_US = weighted.mean(neighbor_IPW_US, total_jobs, na.rm=TRUE),
  ex_mean_dest_IPW_OTH = weighted.mean(neighbor_IPW_OTH, total_jobs, na.rm=TRUE),
  ex_share_less_exposed = weighted.mean(neighbor_less_exposure, total_jobs, na.rm=TRUE),
  ex_share_neighbor_commuters = weighted.mean(neighbor_merge, total_jobs, na.rm=TRUE)
), by=county]

# Full employment network ------------------------------------------------------
full_network_shock <- merge(outside_1990, neighbors,
                            by.x=c("county", "w_county"),
                            by.y=c("county", "neighbor"), all.x=TRUE)

full_network_shock[is.na(neighbor_merge), neighbor_merge := 0L]

full_network_shock <- full_network_shock[, .(
  fn_mean_dest_IPW_US = weighted.mean(neighbor_IPW_US, total_jobs, na.rm=TRUE),
  fn_mean_dest_IPW_OTH = weighted.mean(neighbor_IPW_OTH, total_jobs, na.rm=TRUE),
  fn_share_less_exposed = weighted.mean(neighbor_less_exposure, total_jobs, na.rm=TRUE),
  fn_share_neighbor_commuters = weighted.mean(neighbor_merge, total_jobs, na.rm=TRUE)
), by=county]

# County-level output -----------------------------------------------------------
outside_1990 <- merge(full_network_shock, external_network_shock,
                      by="county", all=TRUE)

outside_1990[, year := 1990]

fwrite(
  outside_1990,
  paste0(output_dir, "/1990_commuting_measures.csv")
)