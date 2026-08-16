##########################################################################
# Created 8.16.2026
##########################################################################

rm(list = ls())

################################################################################
# Paths
################################################################################

path <- "D:/writing_sample/data/irs"

states <- tolower(state.abb)
flows <- c("i", "o")


################################################################################
# 2001-2003
################################################################################

for (y in 2001:2003) {
  
  dt_list <- vector(
    "list",
    length(states) * length(flows)
  )
  
  k <- 1
  
  for (state in states) {
    
    for (f in flows) {
      
      ##########################################################################
      # Flow
      ##########################################################################
      
      if (f == "i") {
        
        flow_dir <- paste0(
          y, "to", y + 1,
          "CountyMigrationInflow"
        )
        
        flow_name <- "inflow"
        
      } else {
        
        flow_dir <- paste0(
          y, "to", y + 1,
          "CountyMigrationOutflow"
        )
        
        flow_name <- "outflow"
      }
      
      
      ##########################################################################
      # Directory
      ##########################################################################
      
      flow_path <- paste0(
        path, "/",
        
        # Outer directory
        y, "to", y + 1,
        "countymigration/",
        
        # Inner directory
        y, "to", y + 1,
        "CountyMigration/",
        
        # Inflow/outflow directory
        flow_dir
      )
      
      
      ##########################################################################
      # Find file
      ##########################################################################
      
      file <- list.files(
        flow_path,
        pattern = paste0(
          state,
          f,
          "\\.xls$"
        ),
        full.names = TRUE,
        ignore.case = TRUE
      )
      
      
      ##########################################################################
      # Check that exactly one file was found
      ##########################################################################
      
      if (length(file) != 1) {
        
        stop(
          paste(
            "Expected 1 file but found",
            length(file),
            "for year",
            y,
            "state",
            state,
            "flow",
            flow_name,
            "\nDirectory:",
            flow_path
          )
        )
      }
      
      
      ##########################################################################
      # Read
      ##########################################################################
      
      dt <- read_excel(
        file,
        skip = 7,
        col_names = FALSE
      ) |>
        data.table()
      
      # Keep only relevant columns
      dt <- dt[, 1:9]
      
      
      ##########################################################################
      # Rename
      ##########################################################################
      
      setnames(
        dt,
        old = names(dt),
        new = c(
          "base_state",
          "base_county",
          "other_state",
          "other_county",
          "state_abb",
          "desc",
          "returns",
          "exemptions",
          "agi"
        )
      )
      
      
      ##########################################################################
      # Keep US migration summary rows
      ##########################################################################
      
      dt <- dt[other_state == 97]
      
      
      ##########################################################################
      # Migration type
      #
      # 1 = same-state migration
      # 2 = different-state migration
      # 3 = total US migration
      ##########################################################################
      
      dt[, type := NA_integer_]
      
      dt[
        grepl(
          "Same St",
          desc,
          ignore.case = TRUE
        ),
        type := 1L
      ]
      
      dt[
        grepl(
          "Diff St",
          desc,
          ignore.case = TRUE
        ),
        type := 2L
      ]
      
      dt[
        grepl(
          "Tot Mig-US$",
          desc,
          ignore.case = TRUE
        ) |
          grepl(
            "Total Mig - US$",
            desc,
            ignore.case = TRUE
          ),
        type := 3L
      ]
      
      
      ##########################################################################
      # Keep only summary rows
      ##########################################################################
      
      dt <- dt[!is.na(type)]
      
      
      ##########################################################################
      # Drop state aggregate
      ##########################################################################
      
      dt <- dt[
        as.integer(base_county) != 0
      ]
      
      
      ##########################################################################
      # County FIPS
      ##########################################################################
      
      dt[, area_fips := paste0(
        sprintf(
          "%02d",
          as.integer(base_state)
        ),
        sprintf(
          "%03d",
          as.integer(base_county)
        )
      )]
      
      
      ##########################################################################
      # Reshape wide
      ##########################################################################
      
      dt_wide <- dcast(
        dt,
        area_fips ~ type,
        value.var = c(
          "returns",
          "exemptions",
          "agi"
        )
      )
      
      
      ##########################################################################
      # Flow and year
      ##########################################################################
      
      dt_wide[, flow := flow_name]
      dt_wide[, year := y]
      
      
      ##########################################################################
      # Save to list
      ##########################################################################
      
      dt_list[[k]] <- dt_wide
      
      print(
        paste(
          "Finished:",
          y,
          state,
          flow_name
        )
      )
      
      k <- k + 1
    }
  }
  
  
  ##############################################################################
  # Combine states and flows for year
  ##############################################################################
  
  irs_fill <- rbindlist(
    dt_list,
    use.names = TRUE,
    fill = TRUE
  )
  
  
  ##############################################################################
  # Save year
  ##############################################################################
  
  fwrite(
    irs_fill,
    paste0(
      path,
      "/lp_irs_migration_",
      y,
      ".csv"
    )
  )
  
  print(
    paste(
      "Saved year:",
      y
    )
  )
}

################################################################################
# 2004-2010
################################################################################

states <- tolower(state.abb)
flows <- c("i", "o")

for (y in 4:10) {
  
  y_0 <- sprintf("%02.0f", y)
  y_1 <- sprintf("%02.0f", y + 1)
  
  dt_list <- vector(
    "list",
    length(states) * length(flows)
  )
  
  k <- 1
  
  for (state in states) {
    
    for (f in flows) {
      
      ##########################################################################
      # Flow
      ##########################################################################
      
      if (f == "i") {
        flow_name <- "inflow"
      } else {
        flow_name <- "outflow"
      }
      
      
      ##########################################################################
      # Directory
      ##########################################################################
      
      year_path <- paste0(
        path,
        "/county",
        y_0,
        y_1
      )
      
      
      ##########################################################################
      # Find state-flow file
      ##########################################################################
      
      file <- list.files(
        year_path,
        pattern = paste0(
          f,
          state,
          "\\.csv$"
        ),
        full.names = TRUE,
        ignore.case = TRUE
      )
      
      
      ##########################################################################
      # Check
      ##########################################################################
      
      if (length(file) != 1) {
        stop(
          paste(
            "Expected 1 file but found",
            length(file),
            "for",
            y,
            state,
            flow_name
          )
        )
      }
      
      
      ##########################################################################
      # Read
      ##########################################################################
      
      dt <- fread(file)
      
      
      ##########################################################################
      # Keep relevant columns
      ##########################################################################
      
      dt <- dt[, 1:9]
      
      
      ##########################################################################
      # Rename
      ##########################################################################
      
      setnames(
        dt,
        old = names(dt),
        new = c(
          "base_state",
          "base_county",
          "other_state",
          "other_county",
          "state_abb",
          "desc",
          "returns",
          "exemptions",
          "agi"
        )
      )
      
      
      ##########################################################################
      # Keep US migration summary rows
      ##########################################################################
      
      dt <- dt[other_state == 97]
      
      
      ##########################################################################
      # Migration type
      ##########################################################################
      
      dt[, type := NA_integer_]
      
      dt[
        grepl("Same St", desc, ignore.case = TRUE),
        type := 1L
      ]
      
      dt[
        grepl("Diff St", desc, ignore.case = TRUE),
        type := 2L
      ]
      
      dt[
        grepl("Tot Mig-US$", desc, ignore.case = TRUE) |
          grepl("Total Mig - US$", desc, ignore.case = TRUE),
        type := 3L
      ]
      
      # Keep only summary rows
      dt <- dt[!is.na(type)]
      
      
      ##########################################################################
      # Drop state aggregate
      ##########################################################################
      
      dt <- dt[
        as.integer(base_county) != 0
      ]
      
      
      ##########################################################################
      # County FIPS
      ##########################################################################
      
      dt[, area_fips := paste0(
        sprintf("%02d", as.integer(base_state)),
        sprintf("%03d", as.integer(base_county))
      )]
      
      
      ##########################################################################
      # Reshape wide
      ##########################################################################
      
      dt_wide <- dcast(
        dt,
        area_fips ~ type,
        value.var = c(
          "returns",
          "exemptions",
          "agi"
        )
      )
      
      
      ##########################################################################
      # Flow and year
      ##########################################################################
      
      dt_wide[, flow := flow_name]
      dt_wide[, year := 2000 + y]
      
      
      ##########################################################################
      # Save to list
      ##########################################################################
      
      dt_list[[k]] <- dt_wide
      
      print(
        paste(
          "Finished:",
          2000 + y,
          state,
          flow_name
        )
      )
      
      k <- k + 1
    }
  }
  
  
  ##############################################################################
  # Combine states and flows
  ##############################################################################
  
  irs_fill <- rbindlist(
    dt_list,
    use.names = TRUE,
    fill = TRUE
  )
  
  
  ##############################################################################
  # Save year
  ##############################################################################
  
  fwrite(
    irs_fill,
    paste0(
      path,
      "/lp_irs_migration_",
      2000 + y,
      ".csv"
    )
  )
}

################################################################################
# Post-2010
################################################################################

flows <- c("inflow", "outflow")

for (y in 2011:2020) {
  
  y_0 <- sprintf("%02d", y %% 100)
  y_1 <- sprintf("%02d", (y + 1) %% 100)
  
  dt_list <- vector("list", length(flows))
  
  for (i in seq_along(flows)) {
    
    f <- flows[i]
    
    dt <- fread(
      paste0(
        path,
        "/county",
        f,
        y_0,
        y_1,
        ".csv"
      )
    )
    
    setnames(
      dt,
      old = names(dt),
      new = c(
        "base_state",
        "base_county",
        "other_state",
        "other_county",
        "state_abb",
        "desc",
        "returns",
        "exemptions",
        "agi"
      )
    )
    
    # Keep US migration summary rows
    dt <- dt[other_state == 97]
    
    # Migration type
    # 1 = same-state migration
    # 2 = different-state migration
    # 3 = total US migration
    dt[, type := NA_integer_]
    
    dt[
      grepl("Same State", desc),
      type := 1L
    ]
    
    dt[
      grepl("Different State", desc),
      type := 2L
    ]
    
    dt[
      grepl("Total Migration-US$", desc) |
        grepl("Total Migration - US$", desc),
      type := 3L
    ]
    
    # Drop state aggregate
    dt <- dt[base_county != 0]
    
    # Keep only classified rows
    dt <- dt[!is.na(type)]
    
    # Construct 5-digit county FIPS
    dt[, area_fips := paste0(
      sprintf("%02d", as.integer(base_state)),
      sprintf("%03d", as.integer(base_county))
    )]
    
    # Reshape wide
    dt_wide <- dcast(
      dt,
      area_fips ~ type,
      value.var = c(
        "returns",
        "exemptions",
        "agi"
      )
    )
    
    # Flow and year
    dt_wide[, flow := f]
    dt_wide[, year := y]
    
    dt_list[[i]] <- dt_wide
    
    print(
      paste(
        "Finished:",
        y,
        f
      )
    )
  }
  
  # Combine inflow and outflow
  irs_fill <- rbindlist(
    dt_list,
    use.names = TRUE,
    fill = TRUE
  )
  
  fwrite(
    irs_fill,
    paste0(
      path,
      "/lp_irs_migration_",
      y,
      ".csv"
    )
  )
}

#####################################################################
# Aggregate data
#####################################################################

files <- list.files(
  path,
  pattern = "^lp_irs_migration_[0-9]{4}\\.csv$",
  full.names = TRUE
)

dt <- rbindlist(
  lapply(files, fread),
  use.names = TRUE,
  fill = TRUE
)

irs_wide <- dcast(
  dt,
  area_fips + year ~ flow,
  value.var = c(
    "returns_1",
    "returns_2",
    "returns_3",
    "exemptions_1",
    "exemptions_2",
    "exemptions_3",
    "agi_1",
    "agi_2",
    "agi_3"
  )
)

fwrite(
  irs_wide,
  paste0(path, "/lp_irs_migration_full.csv")
)