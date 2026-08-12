rm(list = ls())


################################################################################
# Paths
################################################################################

path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data/irs"

################################################################################
# 2002
################################################################################

states_2002 <- tolower(state.abb)

flows <- c("i", "o")

dt_list_2002 <- vector(
  "list",
  length(states_2002) * length(flows)
)

k <- 1

for (state in states_2002) {
  
  for (f in flows) {
    
    if (f == "i") {
      
      flow_dir <- "2002to2003CountyMigrationInflow"
      flow_name <- "inflow"
      
    } else {
      
      flow_dir <- "2002to2003CountyMigrationOutflow"
      flow_name <- "outflow"
      
    }
    
    dt <- read_excel(
      paste0(
        path,
        "/2002to2003CountyMigration/",
        flow_dir,
        "/co203",
        state,
        f,
        ".xls"
      ),
      skip = 7,
      col_names = FALSE
    ) |>
      data.table()
    
    dt <- dt[, 1:9]
    
    # Rename columns by position
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
    
    dt[grepl("Same St", desc), type := 1L]
    dt[grepl("Diff St", desc), type := 2L]
    dt[
      grepl("Tot Mig-US$", desc) |
        grepl("Total Mig - US$", desc),
      type := 3L
    ]
    
    # Keep only summary rows
    dt <- dt[!is.na(type)]
    
    # Drop state aggregate
    dt <- dt[base_county != "000"]
    
    # County FIPS
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
    
    # Flow indicator
    dt_wide[, flow := flow_name]
    
    # Year
    dt_wide[, year := 2002]
    
    # Save to list
    dt_list_2002[[k]] <- dt_wide
    
    print(
      paste(
        "Finished:",
        state,
        2002,
        flow_name
      )
    )
    
    k <- k + 1
  }
}

################################################################################
# Combine inflow and outflow
################################################################################

irs_2002 <- rbindlist(
  dt_list_2002,
  use.names = TRUE,
  fill = TRUE
)

################################################################################
# Save
################################################################################

fwrite(
  irs_2002,
  paste0(path, "/irs_migration_2002.csv")
)



################################################################################
# 2007
################################################################################

states_2007 <- paste0(
  toupper(substr(state.abb, 1, 1)),
  tolower(substr(state.abb, 2, 2))
)
flows <- c("i", "o")

dt_list_2007 <- vector(
  "list",
  length(states_2007) * length(flows)
)

k <- 1

for (state in states_2007) {
  
  for (f in flows) {
    
    if (f == "i") {
      flow_name <- "inflow"
    } else {
      flow_name <- "outflow"
    }
    
    dt <- read_excel(
      paste0(
        path,
        "/county0708/",
        "co0708",
        f, 
        state,
        ".xls"
      ),
      skip = 7,
      col_names = FALSE
    ) |>
      data.table()
    
    # Keep only the 9 relevant columns
    dt <- dt[, 1:9]
    
    # Rename columns by position
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
    
    dt[grepl("Same St", desc), type := 1L]
    dt[grepl("Diff St", desc), type := 2L]
    dt[grepl("Tot Mig-US$", desc) |
         grepl("Total Mig - US$", desc),
       type := 3L]
    
    # Keep only summary rows
    dt <- dt[!is.na(type)]
    
    # Drop state aggregate
    dt <- dt[base_county != "000"]
    
    # County FIPS
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
    
    # Add flow indicator
    dt_wide[, flow := flow_name]
    
    # Year
    dt_wide[, year := 2007]
    
    # Save to list
    dt_list_2007[[k]] <- dt_wide
    
    print(
      paste(
        "Finished:",
        state,
        2007,
        flow_name
      )
    )
    
    k <- k + 1
  }
}

################################################################################
# Combine inflow and outflow
################################################################################

irs_2007 <- rbindlist(
  dt_list_2007,
  use.names = TRUE,
  fill = TRUE
)

################################################################################
# Save
################################################################################

fwrite(
  irs_2007,
  paste0(path, "/irs_migration_2007.csv")
)

################################################################################
# 2012
################################################################################
flows <- c("inflow", "outflow")

dt_list <- vector("list", length(flows))

for (i in seq_along(flows)) {
  
  f <- flows[i]
  
  dt <- fread(
    paste0(path, "/county", f, "1314.csv")
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
  
  dt[grepl("Same State", desc), type := 1L]
  dt[grepl("Different State", desc), type := 2L]
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
    sprintf("%02d", base_state),
    sprintf("%03d", base_county)
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
  dt_wide[, year := 2013]
  
  dt_list[[i]] <- dt_wide
  
  print(paste("Finished:", f))
}

# Combine inflow and outflow
irs_2013 <- rbindlist(
  dt_list,
  use.names = TRUE,
  fill = TRUE
)

fwrite(
  irs_2013,
  paste0(path, "/irs_migration_2013.csv")
)