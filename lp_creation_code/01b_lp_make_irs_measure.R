##########################################################################
# Created 9.12.2026
# Data downloaded from https://www.irs.gov/statistics/soi-tax-stats-migration-data 
##########################################################################

rm(list = ls())
path <- "D:/writing_sample/data/irs"

states <- tolower(state.abb)

flows <- c("i", "o")

for (y_0 in 2000:2010) {

  y_1 <- y_0 + 1
  file_y_0 <- y_0 - 2000
  file_y_1 <- y_1 - 2000

  county_conditions <- fread(paste0(path, "/../output/lp_shock_exposure.csv"))
  county_conditions <- county_conditions[year == y_0]

  if (y_0 <= 1997) county_conditions[area_fips == "12086", area_fips := "12025"]

  to_change <- names(county_conditions)
  home_county_conditions <- copy(county_conditions)
  dest_county_conditions <- copy(county_conditions)

  setnames(home_county_conditions, paste0("home_", to_change))
  setnames(dest_county_conditions, paste0("dest_", to_change))

  home_county_conditions[, home_area_fips := sprintf("%05d", as.integer(home_area_fips))]
  dest_county_conditions[, dest_area_fips := sprintf("%05d", as.integer(dest_area_fips))]

  flow_list <- vector("list", length(flows))

  for (f in flows) {

    state_list <- vector("list", length(states))

    for (i in seq_along(states)) {

      state <- states[i]

      if (f == "i") {
        flow_name <- "inflow"
        flow_dir <- "CountyMigrationInflow"
      } else {
        flow_name <- "outflow"
        flow_dir <- "CountyMigrationOutflow"
      }

      if (y_0 <= 2003) {
        base <- paste0(path, "/", y_0, "to", y_1, "countymigration/", y_0, "to", y_1,
                       "CountyMigration/", y_0, "to", y_1, flow_dir)
        
        if (y_0 == 2000) {
          file_pattern <- paste0("^co.*", state, f, "r\\.xls$")
        } else {
          file_pattern <- paste0("^co.*", state, f, "\\.xls$")
        }
        
      } else {
        
        yy0 <- sprintf("%02d", y_0 %% 100)
        yy1 <- sprintf("%02d", y_1 %% 100)
        base <- paste0(path, "/county", yy0, yy1)
        
        if (y_0 <= 2006) {
          file_pattern <- paste0("^co", yy0, yy1, state, f, "\\.xls$")
        } else {
          file_pattern <- paste0("^co", yy0, yy1, f, state, "\\.xls$")
        }
      }
      
      file <- list.files(base, pattern = file_pattern, full.names = TRUE, ignore.case = TRUE)
      
      if (length(file) != 1) stop(paste("Found", length(file), "files for", y_0, state, flow_name))
      if (length(file) != 1) stop(paste("Found", length(file), "files for", y_0, state, flow_name))
      
      dt <- read_excel(file, skip = 7, col_names = FALSE) |> data.table()
      dt <- dt[, 1:9]

      setnames(dt, names(dt),
               c("base_state", "base_county", "other_state", "other_county",
                 "state_abb", "desc", "returns", "exemptions", "agi"))

      dt[, c("returns", "exemptions", "agi") := lapply(.SD, as.numeric),
         .SDcols = c("returns", "exemptions", "agi")]
      
      dt <- dt[as.integer(base_county) != 0 & as.integer(other_county) != 0]

      base_fips <- paste0(sprintf("%02d", as.integer(dt$base_state)),
                          sprintf("%03d", as.integer(dt$base_county)))

      other_fips <- paste0(sprintf("%02d", as.integer(dt$other_state)),
                           sprintf("%03d", as.integer(dt$other_county)))

      if (f == "o") {
        dt[, `:=`(home_area_fips = base_fips, dest_area_fips = other_fips)]
      } else {
        dt[, `:=`(home_area_fips = other_fips, dest_area_fips = base_fips)]
      }

      # remove non-migrants and IRS aggregate/residual rows
      dt <- dt[home_area_fips != dest_area_fips]
      dt <- dt[
        as.integer(base_state) %between% c(1, 56) &
          as.integer(other_state) %between% c(1, 56) &
          as.integer(base_county) > 0 &
          as.integer(other_county) > 0
      ]

      dt <- merge(dt, home_county_conditions, by = "home_area_fips", all.x = T)
      dt <- merge(dt, dest_county_conditions, by = "dest_area_fips", all.x = T)

      dt <- dt[!is.na(home_IPW_US) & !is.na(dest_IPW_US)]

      dt[, dest_less_exposed := as.integer(dest_IPW_US < home_IPW_US)]
      dt[, dest_less_unemp := as.integer(dest_unemployed_share_labor_force <
                                     home_unemployed_share_labor_force)]

      dt_home <- dt[, .(
        rw_mean_dest_IPW_US = weighted.mean(dest_IPW_US, returns, na.rm = T),
        rw_mean_IPW_diff = weighted.mean(dest_IPW_US - home_IPW_US, returns, na.rm = T),
        rw_share_dest_less_exposed = weighted.mean(dest_less_exposed, returns, na.rm = T),
        rw_dest_less_unemp = weighted.mean(dest_less_unemp, returns, na.rm = T),

        ew_mean_dest_IPW_US = weighted.mean(dest_IPW_US, exemptions, na.rm = T),
        ew_mean_IPW_diff = weighted.mean(dest_IPW_US - home_IPW_US, exemptions, na.rm = T),
        ew_share_dest_less_exposed = weighted.mean(dest_less_exposed, exemptions, na.rm = T),
        ew_dest_less_unemp = weighted.mean(dest_less_unemp, exemptions, na.rm = T),
        total_movers = sum(exemptions, na.rm = T)
      ), by = home_area_fips]

      dt_home[, `:=`(year = y_0, flow = flow_name)]
      state_list[[i]] <- dt_home

      print(paste("Finished:", y_0, state, flow_name))
    }

    flow_list[[which(flows == f)]] <- rbindlist(state_list, use.names = T, fill = T)
  }

  irs_year <- rbindlist(flow_list, use.names = T, fill = T)

  fwrite(irs_year, paste0(path, "/lp_irs_destination_conditions_", y_0, ".csv"))
}



flows <- c("i", "o")

for (y_0 in 2011:2021) {
  
  y_1 <- y_0 + 1
  yy0 <- sprintf("%02d", y_0 %% 100)
  yy1 <- sprintf("%02d", y_1 %% 100)
  
  county_conditions <- fread(paste0(path, "/../output/lp_shock_exposure.csv"))
  county_conditions <- county_conditions[year == y_0]
  
  to_change <- names(county_conditions)
  home_county_conditions <- copy(county_conditions)
  dest_county_conditions <- copy(county_conditions)
  
  setnames(home_county_conditions, paste0("home_", to_change))
  setnames(dest_county_conditions, paste0("dest_", to_change))
  
  home_county_conditions[, home_area_fips := sprintf("%05d", as.integer(home_area_fips))]
  dest_county_conditions[, dest_area_fips := sprintf("%05d", as.integer(dest_area_fips))]
  
  flow_list <- vector("list", length(flows))
  
  for (f in flows) {
    
    flow_name <- ifelse(f == "i", "inflow", "outflow")
    file <- paste0(path, "/county", flow_name, yy0, yy1, ".csv")
    
    dt <- fread(file)
    
    setnames(dt, names(dt)[1:9],
             c("base_state", "base_county", "other_state", "other_county",
               "state_abb", "desc", "returns", "exemptions", "agi"))
    
    dt[, c("returns", "exemptions", "agi") := lapply(.SD, as.numeric),
       .SDcols = c("returns", "exemptions", "agi")]
    
    dt <- dt[as.integer(base_county) != 0 & as.integer(other_county) != 0]
    
    base_fips <- paste0(sprintf("%02d", as.integer(dt$base_state)),
                        sprintf("%03d", as.integer(dt$base_county)))
    
    other_fips <- paste0(sprintf("%02d", as.integer(dt$other_state)),
                         sprintf("%03d", as.integer(dt$other_county)))
    
    if (f == "o") {
      dt[, `:=`(home_area_fips = base_fips, dest_area_fips = other_fips)]
    } else {
      dt[, `:=`(home_area_fips = other_fips, dest_area_fips = base_fips)]
    }
    
    dt <- dt[home_area_fips != dest_area_fips]
    
    dt <- dt[
      as.integer(base_state) %between% c(1, 56) &
        as.integer(other_state) %between% c(1, 56) &
        as.integer(base_county) > 0 &
        as.integer(other_county) > 0
    ]
    
    dt <- merge(dt, home_county_conditions, by = "home_area_fips", all.x = T)
    dt <- merge(dt, dest_county_conditions, by = "dest_area_fips", all.x = T)
    
    dt <- dt[!is.na(home_IPW_US) & !is.na(dest_IPW_US)]
    
    dt[, dest_less_exposed := as.integer(dest_IPW_US < home_IPW_US)]
    dt[, dest_less_unemp := as.integer(dest_unemployed_share_labor_force <
                                         home_unemployed_share_labor_force)]
    
    dt_home <- dt[, .(
      rw_mean_dest_IPW_US = weighted.mean(dest_IPW_US, returns, na.rm = T),
      rw_mean_IPW_diff = weighted.mean(dest_IPW_US - home_IPW_US, returns, na.rm = T),
      rw_share_dest_less_exposed = weighted.mean(dest_less_exposed, returns, na.rm = T),
      rw_dest_less_unemp = weighted.mean(dest_less_unemp, returns, na.rm = T),
      
      ew_mean_dest_IPW_US = weighted.mean(dest_IPW_US, exemptions, na.rm = T),
      ew_mean_IPW_diff = weighted.mean(dest_IPW_US - home_IPW_US, exemptions, na.rm = T),
      ew_share_dest_less_exposed = weighted.mean(dest_less_exposed, exemptions, na.rm = T),
      ew_dest_less_unemp = weighted.mean(dest_less_unemp, exemptions, na.rm = T),
      total_movers = sum(exemptions, na.rm = T)
    ), by = home_area_fips]
    
    dt_home[, `:=`(year = y_0, flow = flow_name)]
    flow_list[[which(flows == f)]] <- dt_home
    
    print(paste("Finished:", y_0, flow_name))
  }
  
  irs_year <- rbindlist(flow_list, use.names = T, fill = T)
  fwrite(irs_year, paste0(path, "/lp_irs_destination_conditions_", y_0, ".csv"))
}

years <- 2000:2021
dt_list <- vector("list", length(years))

for (i in seq_along(years)) {
  y <- years[i]
  dt_list[[i]] <- fread(paste0(path, "/lp_irs_destination_conditions_", y, ".csv"))
}

irs <- rbindlist(dt_list, use.names = T, fill = T)

irs_wide <- dcast(
  irs,
  home_area_fips + year ~ flow,
  value.var = c(
    "rw_mean_dest_IPW_US",
    "rw_mean_IPW_diff",
    "rw_share_dest_less_exposed",
    "rw_dest_less_unemp",
    "ew_mean_dest_IPW_US",
    "ew_mean_IPW_diff",
    "ew_share_dest_less_exposed",
    "ew_dest_less_unemp",
    "total_movers"
  )
)

fwrite(irs_wide, paste0(path, "/lp_irs_destination_conditions_full.csv"))
