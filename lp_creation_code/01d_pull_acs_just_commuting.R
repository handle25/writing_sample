################################################################################
# ACS 1-year detailed tables: commuting variables, 2005-2024
# Note: standard 2020 ACS 1-year estimates were not released, so 2020 is skipped.
################################################################################

rm(list = ls())

path <- "D:/writing_sample/data"

library(tidycensus)
library(data.table)

mykey <- "60478a05082dfaab8197a88cf6e696b52abd48bd"
Sys.setenv(CENSUS_API_KEY = mykey)

################################################################################
# Years and states
################################################################################

years <- c(2005:2019, 2021:2024)

states <- c(
  "01","02","04","05","06","08","09","10","11","12",
  "13","15","16","17","18","19","20","21","22","23",
  "24","25","26","27","28","29","30","31","32","33",
  "34","35","36","37","38","39","40","41","42","44",
  "45","46","47","48","49","50","51","53","54","55","56"
)

################################################################################
# Pull data
################################################################################

dt_list <- list()
k <- 1

for (y in years) {
  
  for (s in states) {
    
    print(paste("Starting:", y, s))
    
    dt <- tryCatch(
      
      get_acs(
        geography = "county",
        state = s,
        year = y,
        survey = "acs1",
        output = "wide",
        
        variables = c(
          
          ######################################################################
          # Workers / work from home
          ######################################################################
          
          # Workers 16 years and over
          workers_total = "B08301_001",
          
          # Worked from home
          workers_wfh = "B08301_021",
          
          ######################################################################
          # Workers who did not work from home
          ######################################################################
          
          # Universe for B08303:
          # Workers 16 years and over who did not work from home
          workers_not_wfh = "B08303_001",
          
          ######################################################################
          # Travel time to work
          ######################################################################
          
          commute_lt5 = "B08303_002",
          commute_5_9 = "B08303_003",
          commute_10_14 = "B08303_004",
          commute_15_19 = "B08303_005",
          commute_20_24 = "B08303_006",
          commute_25_29 = "B08303_007",
          commute_30_34 = "B08303_008",
          commute_35_39 = "B08303_009",
          commute_40_44 = "B08303_010",
          commute_45_59 = "B08303_011",
          commute_60_89 = "B08303_012",
          commute_90plus = "B08303_013",
          
          ######################################################################
          # Aggregate travel time
          ######################################################################
          
          # Aggregate travel time to work in minutes
          aggregate_travel_time = "B08013_001"
        )
      ) |>
        data.table(),
      
      error = function(e) {
        
        print(
          paste(
            "Failed:",
            y,
            s,
            "|",
            conditionMessage(e)
          )
        )
        
        NULL
      }
    )
    
    ##########################################################################
    # Store successful pulls
    ##########################################################################
    
    if (!is.null(dt)) {
      
      dt[, year := y]
      dt[, state := s]
      
      dt_list[[k]] <- dt
      k <- k + 1
      
      print(paste("Finished:", y, s))
    }
  }
}

################################################################################
# Bind
################################################################################

acs_1y_commuting <- rbindlist(
  dt_list,
  use.names = TRUE,
  fill = TRUE
)

################################################################################
# County FIPS
################################################################################

acs_1y_commuting[, area_fips := as.integer(GEOID)]

################################################################################
# Construct mean travel time
################################################################################

acs_1y_commuting[
  workers_not_wfhE > 0,
  mean_travel_time_to_work_minutes :=
    aggregate_travel_timeE / workers_not_wfhE
]

################################################################################
# Construct WFH share
################################################################################

acs_1y_commuting[
  workers_totalE > 0,
  wfh_share :=
    workers_wfhE / workers_totalE
]

################################################################################
# Harmonized commute-time bins
################################################################################

acs_1y_commuting[
  ,
  commute_lt10 :=
    commute_lt5E + commute_5_9E
]

acs_1y_commuting[
  ,
  commute_10_14_harmonized :=
    commute_10_14E
]

acs_1y_commuting[
  ,
  commute_15_19_harmonized :=
    commute_15_19E
]

acs_1y_commuting[
  ,
  commute_20_24_harmonized :=
    commute_20_24E
]

acs_1y_commuting[
  ,
  commute_25_29_harmonized :=
    commute_25_29E
]

acs_1y_commuting[
  ,
  commute_30_34_harmonized :=
    commute_30_34E
]

acs_1y_commuting[
  ,
  commute_35_44 :=
    commute_35_39E + commute_40_44E
]

acs_1y_commuting[
  ,
  commute_45_59_harmonized :=
    commute_45_59E
]

acs_1y_commuting[
  ,
  commute_60plus :=
    commute_60_89E + commute_90plusE
]

################################################################################
# Commute-time shares
################################################################################

commute_vars <- c(
  "commute_lt10",
  "commute_10_14_harmonized",
  "commute_15_19_harmonized",
  "commute_20_24_harmonized",
  "commute_25_29_harmonized",
  "commute_30_34_harmonized",
  "commute_35_44",
  "commute_45_59_harmonized",
  "commute_60plus"
)

for (v in commute_vars) {
  
  newvar <- paste0(v, "_share")
  
  acs_1y_commuting[
    workers_not_wfhE > 0,
    (newvar) := get(v) / workers_not_wfhE
  ]
}

################################################################################
# Check WFH / non-WFH consistency
################################################################################

acs_1y_commuting[
  ,
  workers_check :=
    workers_totalE -
    workers_wfhE -
    workers_not_wfhE
]

print(summary(acs_1y_commuting$workers_check))

################################################################################
# Save
################################################################################

fwrite(
  acs_1y_commuting,
  paste0(
    path,
    "/acs/acs_1y_2005_2024_commuting.csv"
  )
)

print("ACS 1-year 2005-2024 commuting pull complete.")
