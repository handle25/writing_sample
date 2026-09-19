##########################################################################
# Created 8.16.2026

# Data downloaded from https://www.irs.gov/statistics/soi-tax-stats-migration-data 
##########################################################################

rm(list = ls())
# Paths
path <- "D:/writing_sample/data/irs"

states <- tolower(state.abb)
flows <- c("i", "o")

plan(multisession, workers = 4)

states <- tolower(state.abb)
flows <- c("i", "o")

# 1990-1991 --------------------------------------------------------------------

states <- tolower(state.abb)
flows <- c("i", "o")

future_lapply(1990:1991, function(y) {
  
  dt_list <- vector("list", length(states) * length(flows))
  k <- 1
  
  for (state in states) {
    for (f in flows) {
      
      # flow -------------------------------------------------------------------
      if (f == "i") {
        flow_dir <- paste0(y, "to", y + 1, "CountyMigrationInflow")
        flow_name <- "inflow"
      } else {
        flow_dir <- paste0(y, "to", y + 1, "CountyMigrationOutflow")
        flow_name <- "outflow"
      }
      
      # directory --------------------------------------------------------------
      flow_path <- paste0(
        path, "/", y, "to", y + 1, "countymigration/",
        y, "to", y + 1, "CountyMigration/", flow_dir
      )
      
      # filename ---------------------------------------------------------------
      year_code <- paste0(sprintf("%02d", y %% 100), sprintf("%02d", (y + 1) %% 100))
      file_pattern <- paste0("^C", year_code, state, f, "\\.txt$")
      
      file <- list.files(flow_path, pattern=file_pattern, full.names=TRUE, ignore.case=TRUE)
      
      if (length(file) != 1) {
        stop(paste("Expected 1 file but found", length(file),
                   "for", y, state, flow_name))
      }
      
      # read -------------------------------------------------------------------
      x <- readLines(file, warn=FALSE)
      
      # keep IRS county total flow ---------------------------------------------
      x <- x[grepl("^[0-9]{2}\\s+[0-9]{3}\\s+.*County Non-Migrants", x)] # this line changes
      
      # extract variables ------------------------------------------------------
      dt <- data.table(
        base_state=as.integer(substr(x, 1, 2)),
        base_county=as.integer(substr(x, 4, 6)),
        returns_3=as.numeric(trimws(substr(x, 40, 48))),
        exemptions_3=as.numeric(trimws(substr(x, 57, 64)))
      )
      
      # county FIPS ------------------------------------------------------------
      dt[, area_fips := paste0(sprintf("%02d", base_state), sprintf("%03d", base_county))]
      
      # flow and year ----------------------------------------------------------
      dt[, `:=`(flow=flow_name, year=y)]
      
      # keep variables ---------------------------------------------------------
      dt <- dt[, .(area_fips, returns_3, exemptions_3, flow, year)]
      
      # save to list -----------------------------------------------------------
      dt_list[[k]] <- dt
      print(paste("Finished:", y, state, flow_name))
      k <- k + 1
    }
  }
  
  # combine and save -----------------------------------------------------------
  irs_fill <- rbindlist(dt_list, use.names=TRUE, fill=TRUE)
  fwrite(irs_fill, paste0(path, "/new_lp_irs_nonmigration_", y, ".csv"))
  
  print(paste("Saved year:", y))
})

# 1992-1994 --------------------------------------------------------------------

states <- tolower(state.abb)
flows <- c("i", "o")

future_lapply(1992:1994, function(y) {
  
  dt_list <- vector("list", length(states) * length(flows))
  k <- 1
  
  for (state in states) {
    for (f in flows) {
      
      # flow -------------------------------------------------------------------
      if (f == "i") {
        flow_dir <- paste0(y, "to", y + 1, "CountyMigrationInflow")
        flow_name <- "inflow"
      } else {
        flow_dir <- paste0(y, "to", y + 1, "CountyMigrationOutflow")
        flow_name <- "outflow"
      }
      
      # directory --------------------------------------------------------------
      flow_path <- paste0(
        path, "/", y, "to", y + 1, "countymigration/",
        y, "to", y + 1, "CountyMigration/", flow_dir
      )
      
      # filename ---------------------------------------------------------------
      if (y == 1992) {
        year_code <- "9293"
        file_pattern <- paste0("^C", year_code, state, f, "\\.xls")
      } else {
        year_code <- paste0(sprintf("%02d", y %% 100), (y + 1) %% 10)
        file_pattern <- paste0("^co", year_code, state, f, ".*\\.xls$")
      }
      
      file <- list.files(flow_path, pattern=file_pattern, full.names=TRUE, ignore.case=TRUE)
      
      if (length(file) != 1) {
        stop(paste(
          "Expected 1 file but found", length(file),
          "for year", y, "state", state, "flow", flow_name,
          "\nExpected pattern:", file_pattern,
          "\nDirectory:", flow_path
        ))
      }
      
      # read -------------------------------------------------------------------
      dt <- read_excel(file, skip=7, col_names=FALSE) |> data.table()
      dt <- dt[, 1:9]
      
      setnames(dt, names(dt), c(
        "base_state", "base_county", "other_state", "other_county",
        "state_abb", "desc", "returns", "exemptions", "agi"
      ))
      
      cols <- c("base_state", "base_county", "other_state", "other_county",
                "returns", "exemptions", "agi")
      dt[, (cols) := lapply(.SD, as.numeric), .SDcols=cols]
      
      # keep county non-migrants -----------------------------------------------------
      dt <- dt[grepl("County\\s+Non-Migrant", desc, ignore.case=TRUE)] # this line changes 
      
      # county FIPS ------------------------------------------------------------
      dt[, area_fips := paste0(sprintf("%02d", base_state), sprintf("%03d", base_county))]
      
      # keep variables ---------------------------------------------------------
      dt <- dt[, .(
        area_fips,
        returns_3=returns,
        exemptions_3=exemptions,
        agi_3=agi
      )]
      
      # flow and year ----------------------------------------------------------
      dt[, `:=`(flow=flow_name, year=y)]
      
      # save to list -----------------------------------------------------------
      dt_list[[k]] <- dt
      print(paste("Finished:", y, state, flow_name))
      k <- k + 1
    }
  }
  
  # combine and save -----------------------------------------------------------
  irs_fill <- rbindlist(dt_list, use.names=TRUE, fill=TRUE)
  fwrite(irs_fill, paste0(path, "/new_lp_irs_nonmigration_", y, ".csv"))
  
  print(paste("Saved year:", y))
})

# 1995-2003 --------------------------------------------------------------------

future_lapply(1995:2003, function(y) {
  
  dt_list <- vector("list", length(states) * length(flows))
  k <- 1
  
  for (state in states) {
    for (f in flows) {
      
      # flow -------------------------------------------------------------------
      if (f == "i") {
        flow_dir <- paste0(y, "to", y + 1, "CountyMigrationInflow")
        flow_name <- "inflow"
      } else {
        flow_dir <- paste0(y, "to", y + 1, "CountyMigrationOutflow")
        flow_name <- "outflow"
      }
      
      # directory --------------------------------------------------------------
      flow_path <- paste0(
        path, "/", y, "to", y + 1, "countymigration/",
        y, "to", y + 1, "CountyMigration/", flow_dir
      )
      
      # filename ---------------------------------------------------------------------
      if (y <= 2000) {
        year_code <- paste0(sprintf("%02d", y %% 100), (y + 1) %% 10)
      } else if (y <= 2002) {
        year_code <- paste0(y %% 10, sprintf("%02d", (y + 1) %% 100))
      } else {
        year_code <- paste0(sprintf("%02d", y %% 100), sprintf("%02d", (y + 1) %% 100))
      }
      
      file_pattern <- paste0("^co", year_code, state, f, ".*\\.xls$")
      
      file <- list.files(flow_path, pattern=file_pattern, full.names=TRUE, ignore.case=TRUE)
      
      if (length(file) != 1) {
        stop(paste(
          "Expected 1 file but found", length(file),
          "for year", y, "state", state, "flow", flow_name,
          "\nExpected pattern:", file_pattern,
          "\nDirectory:", flow_path
        ))
      }
      
      # read -------------------------------------------------------------------
      dt <- read_excel(file, skip=7, col_names=FALSE) |> data.table()
      dt <- dt[, 1:9]
      
      setnames(dt, names(dt), c(
        "base_state", "base_county", "other_state", "other_county",
        "state_abb", "desc", "returns", "exemptions", "agi"
      ))
      
      cols <- c("base_state", "base_county", "other_state", "other_county",
                "returns", "exemptions", "agi")
      dt[, (cols) := lapply(.SD, as.numeric), .SDcols=cols]
      
      # keep total domestic US migration ---------------------------------------
      dt <- dt[((base_county == other_county) & (base_state == other_state)),] # this line changes 
      
      # county FIPS ------------------------------------------------------------
      dt[, area_fips := paste0(sprintf("%02d", base_state), sprintf("%03d", base_county))]
      
      # keep variables ---------------------------------------------------------
      dt <- dt[, .(
        area_fips,
        returns_3=returns,
        exemptions_3=exemptions,
        agi_3=agi
      )]
      
      # flow and year ----------------------------------------------------------
      dt[, `:=`(flow=flow_name, year=y)]
      
      # save to list -----------------------------------------------------------
      dt_list[[k]] <- dt
      print(paste("Finished:", y, state, flow_name))
      k <- k + 1
    }
  }
  
  # combine and save -----------------------------------------------------------
  irs_fill <- rbindlist(dt_list, use.names=TRUE, fill=TRUE)
  fwrite(irs_fill, paste0(path, "/new_lp_irs_nonmigration_", y, ".csv"))
  
  print(paste("Saved year:", y))
})

# 2004-2010 --------------------------------------------------------------------

for (y in 4:10) {
  
  y_0 <- sprintf("%02d", y)
  y_1 <- sprintf("%02d", y + 1)
  
  dt_list <- vector("list", length(states) * length(flows))
  k <- 1
  
  for (state in states) {
    for (f in flows) {
      
      # flow -------------------------------------------------------------------
      if (f == "i") {
        flow_name <- "inflow"
      } else {
        flow_name <- "outflow"
      }
      
      # directory --------------------------------------------------------------
      year_path <- paste0(path, "/county", y_0, y_1)
      
      # filename ---------------------------------------------------------------
      if (y <= 6) {
        file_pattern <- paste0("^co", y_0, y_1, state, f, ".*\\.xls$")
      } else {
        file_pattern <- paste0("^co", y_0, y_1, f, state, ".*\\.xls$")
      }
      
      file <- list.files(year_path, pattern=file_pattern, full.names=TRUE, ignore.case=TRUE)
      
      if (length(file) != 1) {
        stop(paste(
          "Expected 1 file but found", length(file),
          "for year", 2000 + y, "state", state, "flow", flow_name,
          "\nExpected pattern:", file_pattern,
          "\nDirectory:", year_path
        ))
      }
      
      # read -------------------------------------------------------------------
      dt <- read_excel(file, skip=7, col_names=FALSE) |> data.table()
      dt <- dt[, 1:9]
      
      setnames(dt, names(dt), c(
        "base_state", "base_county", "other_state", "other_county",
        "state_abb", "desc", "returns", "exemptions", "agi"
      ))
      
      cols <- c("base_state", "base_county", "other_state", "other_county",
                "returns", "exemptions", "agi")
      dt[, (cols) := lapply(.SD, as.numeric), .SDcols=cols]
      
      # keep total domestic US migration ---------------------------------------
      dt <- dt[((base_county == other_county) & (base_state == other_state)),] # this line changes 
      
      # county FIPS ------------------------------------------------------------
      dt[, area_fips := paste0(sprintf("%02d", base_state), sprintf("%03d", base_county))]
      
      # keep variables ---------------------------------------------------------
      dt <- dt[, .(
        area_fips,
        returns_3=returns,
        exemptions_3=exemptions,
        agi_3=agi
      )]
      
      # flow and year ----------------------------------------------------------
      dt[, `:=`(flow=flow_name, year=2000 + y)]
      
      # save to list -----------------------------------------------------------
      dt_list[[k]] <- dt
      print(paste("Finished:", 2000 + y, state, flow_name))
      k <- k + 1
    }
  }
  
  # combine and save -----------------------------------------------------------
  irs_fill <- rbindlist(dt_list, use.names=TRUE, fill=TRUE)
  fwrite(irs_fill, paste0(path, "/new_lp_irs_nonmigration_", 2000 + y, ".csv"))
  
  print(paste("Saved year:", 2000 + y))
}

# 2011-2021 --------------------------------------------------------------------
flows <- c("inflow", "outflow")

for (y in 2011:2021) {
  
  y_0 <- sprintf("%02d", y %% 100)
  y_1 <- sprintf("%02d", (y + 1) %% 100)
  
  dt_list <- vector("list", length(flows))
  
  for (i in seq_along(flows)) {
    
    f <- flows[i]
    
    # read ---------------------------------------------------------------------
    dt <- fread(paste0(path, "/county", f, y_0, y_1, ".csv"))
    
    setnames(dt, names(dt), c(
      "base_state", "base_county", "other_state", "other_county",
      "state_abb", "desc", "returns", "exemptions", "agi"
    ))
    
    cols <- c("base_state", "base_county", "other_state", "other_county",
              "returns", "exemptions", "agi")
    dt[, (cols) := lapply(.SD, as.numeric), .SDcols=cols]
    
    # keep total domestic US migration -----------------------------------------
    dt <- dt[((base_county == other_county) & (base_state == other_state)),] # this line changes 
    
    # county FIPS --------------------------------------------------------------
    dt[, area_fips := paste0(sprintf("%02d", base_state), sprintf("%03d", base_county))]
    
    # keep variables -----------------------------------------------------------
    dt <- dt[, .(
      area_fips,
      returns_3=returns,
      exemptions_3=exemptions,
      agi_3=agi
    )]
    
    # flow and year ------------------------------------------------------------
    dt[, `:=`(flow=f, year=y)]
    
    # save to list -------------------------------------------------------------
    dt_list[[i]] <- dt
    print(paste("Finished:", y, f))
  }
  
  # combine and save -----------------------------------------------------------
  irs_fill <- rbindlist(dt_list, use.names=TRUE, fill=TRUE)
  fwrite(irs_fill, paste0(path, "/new_lp_irs_nonmigration_", y, ".csv"))
  
  print(paste("Saved year:", y))
}

# aggregate data ---------------------------------------------------------------

dt <- rbindlist(lapply(1990:2021, function(y) {
  fread(paste0(path, "/new_lp_irs_nonmigration_", y, ".csv"))
}), use.names=TRUE, fill=TRUE)

# check one observation per county-year-flow ----------------------------------
dups <- dt[, .N, by=.(area_fips, year, flow)][N != 1]
print(dups)
if (nrow(dups)) stop("Duplicate county-year-flow rows remain")

# check inflow and outflow copies agree ---------------------------------------
check <- dcast(dt, area_fips + year ~ flow, value.var=c("returns_3", "exemptions_3", "agi_3"))

check[, mismatch :=
        (!is.na(returns_3_inflow) & !is.na(returns_3_outflow) & returns_3_inflow != returns_3_outflow) |
        (!is.na(exemptions_3_inflow) & !is.na(exemptions_3_outflow) & exemptions_3_inflow != exemptions_3_outflow) |
        (!is.na(agi_3_inflow) & !is.na(agi_3_outflow) & agi_3_inflow != agi_3_outflow)
]

# print(check[mismatch == TRUE])
# if (check[, any(mismatch)]) stop("Inflow and outflow non-migrant values disagree")

# keep one copy ---------------------------------------------------------------
irs_nonmigration <- dt[flow == "inflow", .(
  area_fips, year,
  returns=returns_3,
  exemptions=exemptions_3,
  agi=agi_3
)]

fwrite(irs_nonmigration, paste0(path, "/new_lp_irs_nonmigration_full.csv"))