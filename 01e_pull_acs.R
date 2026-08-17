rm(list = ls())
path <- "D:/writing_sample/data"
mykey <- '8E96FC26-CD61-45D4-8F1B-2A6727156311'
mykey <- "60478a05082dfaab8197a88cf6e696b52abd48bd"
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
Sys.setenv(CENSUS_KEY = mykey)
################################################################################
# Paths
################################################################################



################################################################################
# Variable metadata
################################################################################

var_names <- data.table(
  load_variables(
    2023,
    "acs5/subject"
  )
)

################################################################################
# Functions
################################################################################

getname <- function(var) {
  
  var_names[
    grep(
      substr(var, 1, nchar(var) - 1),
      var_names$name
    ),
    label
  ]
}

################################################################################
# Years and states
################################################################################

years <- c(
  2009:2024
)

states <- c(
  "01","02","04","05","06","08","09","10","11","12",
  "13","15","16","17","18","19","20","21","22","23",
  "24","25","26","27","28","29","30","31","32","33",
  "34","35","36","37","38","39","40","41","42","44",
  "45","46","47","48","49","50","51","53","54","55","56")

################################################################################
# Variables of interest
################################################################################

age <- paste0(
  var_names[
    grep(
      "Estimate!!Total!!Total population!!AGE!!",
      label,
      ignore.case = TRUE
    ),
    name
  ],
  "E"
)

comm <- paste0(
  var_names[
    grep(
      "Estimate!!Total!!Workers 16 years and over who did not work from home!!TRAVEL TIME TO WORK!!",
      label,
      ignore.case = TRUE
    ),
    name
  ],
  "E"
)

educ <- paste0(
  var_names[
    grep(
      "Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 18 to 24 years",
      label,
      ignore.case = TRUE
    ),
    name
  ],
  "E"
)

tran <- paste0(
  var_names[
    grep(
      "Estimate!!Total!!Workers 16 years and over!!MEANS OF TRANSPORTATION TO WORK",
      label,
      ignore.case = TRUE
    ),
    name
  ],
  "E"
)

industry <- paste0(
  var_names[
    grep(
      "Estimate!!Total!!Full-time, year-round civilian employed population 16 years and over",
      label,
      ignore.case = TRUE
    ),
    name
  ],
  "E"
)

income <- paste0(
  var_names[
    grep(
      "Estimate!!Total!!Population 16 years and over with earnings!!FULL-TIME, YEAR-ROUND WORKERS WITH EARNINGS!!",
      label,
      ignore.case = TRUE
    ),
    name
  ],
  "E"
)

vehicle <- paste0(
  var_names[
    grep(
      "Estimate!!Total!!VEHICLES AVAILABLE!!Workers 16 years and over in households",
      label,
      ignore.case = TRUE
    ),
    name
  ],
  "E"
)

not_wfh <- paste0(
  var_names[
    grep(
      "^Estimate!!Total!!Workers 16 years and over who did not work from home$",
      label,
      ignore.case = TRUE
    ),
    name
  ],
  "E"
)

################################################################################
# Variables to pull
################################################################################

vars_pull <- unique(
  c(
    "S0101_C01_001E",
    "S0801_C03_046E",
    "S2403_C02_020E",
    
    # age
    age,

    vehicle, not_wfh, 
    
    # modes of transportation
    tran,
    
    # commute times
    comm,
    
    # industry
    industry,
    
    # income
    income,
    
    # population
    "S0102PR_C01_001E",
    
    # name
    "NAME",
    
    # education
    educ
  )
)

################################################################################
# Output folder
################################################################################

dir.create(
  paste0(path, "/data/acs"),
  showWarnings = FALSE,
  recursive = TRUE
)

################################################################################
# Pull by state
################################################################################

for (j in seq_along(states)) {
  
  state <- states[j]
  
  print(
    paste(
      "Starting state:",
      state
    )
  )
  
  ##############################################################################
  # List of years for current state
  ##############################################################################
  
  year_list <- vector(
    "list",
    length(years)
  )
  
  k <- 1
  
  ##############################################################################
  # Pull each year
  ##############################################################################
  
  for (i in seq_along(years)) {
    
    y <- years[i]
    
    dt <- tryCatch(
      
      getCensus(
        name = "acs/acs5/subject",
        vintage = y,
        key = mykey, 
        vars = vars_pull,
        region = "county:*",
        regionin = paste0(
          "state:",
          state
        )
      ) |>
        data.table(),
      
      error = function(e) {
        
        print(
          paste(
            "Failed:",
            state,
            y,
            "|",
            conditionMessage(e)
          )
        )
        
        NULL
      }
    )
    
    ############################################################################
    # Add successful year to list
    ############################################################################
    
    if (!is.null(dt)) {
      
      dt[, year := y]
      dt[, state := state]
      
      year_list[[k]] <- dt
      
      k <- k + 1
      
      print(
        paste(
          "Finished:",
          state,
          y
        )
      )
    }
  }
  
  ##############################################################################
  # Remove empty years
  ##############################################################################
  
  year_list <- year_list[
    !vapply(
      year_list,
      is.null,
      logical(1)
    )
  ]
  
  ##############################################################################
  # If no years worked, skip state
  ##############################################################################
  
  if (length(year_list) == 0) {
    
    print(
      paste(
        "No usable years for state:",
        state
      )
    )
    
    next
  }
  
  ##############################################################################
  # Bind all years for this state
  ##############################################################################
  
  dt_state <- rbindlist(
    year_list,
    use.names = TRUE,
    fill = TRUE
  )
  
  rm(year_list)
  
  ##############################################################################
  # Cleaning
  ##############################################################################
  
  torep <- names(dt_state)[
    grep(
      "^S",
      names(dt_state)
    )
  ]
  
  var_names[
    ,
    hierarchy := str_count(
      label,
      "!!"
    )
  ]
  
  for (v in torep) {
    
    label_match <- getname(v)
    
    if (length(label_match) == 0) {
      next
    }
    
    split <- strsplit(
      label_match[1],
      "!!",
      fixed = TRUE
    )[[1]]
    
    raw <- split[
      length(split)
    ]
    
    new_name <- clean_names(
      data.frame(
        x = raw
      )
    ) |>
      names()
    
    new_name <- sub(
      "^x$",
      make_clean_names(raw),
      new_name
    )
    
    setnames(
      dt_state,
      old = v,
      new = make_clean_names(raw)
    )
  }
  
  ##############################################################################
  # Clean all remaining column names
  ##############################################################################
  
  dt_state <- clean_names(
    dt_state
  ) |>
    data.table()
  
  ##############################################################################
  # Drop completely missing columns
  ##############################################################################
  
  all_na_cols <- names(dt_state)[
    vapply(
      dt_state,
      function(x) all(is.na(x)),
      logical(1)
    )
  ]
  
  if (length(all_na_cols) > 0) {
    
    dt_state[
      ,
      (all_na_cols) := NULL
    ]
  }
  
  ##############################################################################
  # Replace negative Census missing-value codes with zero
  # Numeric columns only
  ##############################################################################
  
  numeric_cols <- names(dt_state)[
    vapply(
      dt_state,
      is.numeric,
      logical(1)
    )
  ]
  
  dt_state[
    ,
    (numeric_cols) := lapply(
      .SD,
      function(x) {
        fifelse(
          !is.na(x) & x < 0,
          0,
          x
        )
      }
    ),
    .SDcols = numeric_cols
  ]
  
  ##############################################################################
  # Save one state-level panel
  ##############################################################################
  
  fwrite(
    dt_state,
    paste0(
      path,
      "/acs/lp_acs_",
      state,
      ".csv"
    )
  )
  
  print(
    paste(
      "Saved state:",
      state,
      "| years:",
      paste(
        sort(unique(dt_state$year)),
        collapse = ", "
      )
    )
  )
  
  ##############################################################################
  # Clear state from memory
  ##############################################################################
  
  rm(
    dt,
    dt_state
  )
  
  gc()
}

################################################################################
# Finished
################################################################################

print("ACS state pulls complete.")


states <- c(
  "01","02","04","05","06","08","09","10","11","12",
  "13","15","16","17","18","19","20","21","22","23",
  "24","25","26","27","28","29","30","31","32","33",
  "34","35","36","37","38","39","40","41","42","44",
  "45","46","47","48","49","50","51","53","54","55","56"
)

dt_list <- vector("list", length(states))

for (i in seq_along(states)) {
  
  dt_list[[i]] <- fread(
    paste0(path, "/acs/lp_acs_", states[i], ".csv")
  )
  
}

acs_all <- rbindlist(
  dt_list,
  use.names = TRUE,
  fill = TRUE
)

fwrite(
  acs_all,
  paste0(path, "/acs/lp_acs_all.csv")
)
