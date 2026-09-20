##########################################################################
# Created 9.12.2026
# Data downloaded from https://www.irs.gov/statistics/soi-tax-stats-migration-data 
##########################################################################

rm(list = ls())
path <- "D:/writing_sample/data/irs"


states <- tolower(state.abb)
start <- Sys.time()
plan(multisession, workers=4)

# base agi  --------------------------------------------------------------------
flows <- c("i")
future_lapply(c(1995, 1998:2010), function(y_0) {
  
  y_1 <- y_0 + 1
  file_y_0 <- y_0 - 2000
  file_y_1 <- y_1 - 2000

  flow_list <- vector("list", length(flows))
  
  for (f in flows) {
    state_list <- vector("list", length(states))
    for (i in seq_along(states)) {
      state <- states[i]
      flow_name <- "inflow"
      flow_dir <- "CountyMigrationInflow"
      
      if (y_0 <= 2003) {
        base <- paste0(path, "/", y_0, "to", y_1, "countymigration/", y_0, "to", y_1,
                       "CountyMigration/", y_0, "to", y_1, flow_dir)
        
        if (y_0 == 2000 | y_0 == 1995) {
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
               c("migration_into_state", "migration_into_county", 
                 "migration_from_state", "migration_from_county",
                 "state_abb", "desc", "returns", "exemptions", "agi"))
      
      dt[, c("returns", "exemptions", "agi") := lapply(.SD, as.numeric),
         .SDcols = c("returns", "exemptions", "agi")]
      
      dt[, `:=`(
        into_area_fips = paste0(sprintf("%02d", as.integer(migration_into_state)),
                                sprintf("%03d", as.integer(migration_into_county))),
        from_area_fips = paste0(sprintf("%02d", as.integer(migration_from_state)),
                                sprintf("%03d", as.integer(migration_from_county)))
      )]
      dt <- dt[(as.integer(migration_from_state) == 97 & as.integer(migration_from_county) == 0) |
                 from_area_fips == into_area_fips]
      
      dt[as.integer(migration_from_state) == 97 & as.integer(migration_from_county) == 0, new := 1L]
      dt[from_area_fips == into_area_fips, new := 0L]
      
      
      dt[, `:=`(year=y_0, flow=flow_name)]
      state_list[[i]] <- dt
      
      print(paste("Finished:", y_0, state, flow_name))
    }
    
    flow_list[[which(flows == f)]] <- rbindlist(state_list, use.names = T, fill = T)
  }
  
  irs_year <- rbindlist(flow_list, use.names = T, fill = T)
  
  fwrite(irs_year, paste0(path, "/lp_irs_destination_agi_",f, y_0, ".csv"))
}
)

# base agi after 2010 ----------------------------------------------------------
future_lapply(2011:2021, function(y_0) {
  
  y_1 <- y_0 + 1
  yy0 <- sprintf("%02d", y_0 %% 100)
  yy1 <- sprintf("%02d", y_1 %% 100)
  
  file <- paste0(path, "/countyinflow", yy0, yy1, ".csv")
  dt <- fread(file)
  
  setnames(dt, names(dt)[1:9],
           c("migration_into_state", "migration_into_county",
             "migration_from_state", "migration_from_county",
             "state_abb", "desc", "returns", "exemptions", "agi"))
  
  dt[, c("returns", "exemptions", "agi") := lapply(.SD, as.numeric),
     .SDcols=c("returns", "exemptions", "agi")]
  
  dt[, `:=`(
    into_area_fips=paste0(sprintf("%02d", as.integer(migration_into_state)),
                          sprintf("%03d", as.integer(migration_into_county))),
    from_area_fips=paste0(sprintf("%02d", as.integer(migration_from_state)),
                          sprintf("%03d", as.integer(migration_from_county)))
  )]
  
  # non-migrants = local county AGI
  dt <- dt[from_area_fips == into_area_fips & !is.na(into_area_fips)]
  
  dt[, `:=`(
    area_fips=into_area_fips,
    year=y_0,
    agi_per_return=1000*agi/returns,
    agi_per_exemption=1000*agi/exemptions
  )]
  
  dt <- dt[, .(area_fips, year, returns, exemptions, agi,
               agi_per_return, agi_per_exemption)]
  
  fwrite(dt, paste0(path, "/lp_irs_agi_county_", y_0, ".csv"))
  print(paste("Finished:", y_0, "county AGI"))
})

# aggregate agi of county ------------------------------------------------------
files_old <- list.files(path, pattern="lp_irs_destination_agi_i", full.names=T)

agi_old <- rbindlist(lapply(files_old, fread), use.names=T, fill=T)
agi_old <- agi_old[into_area_fips == from_area_fips & !is.na(into_area_fips)]

agi_old <- agi_old |>
  fselect(area_fips=into_area_fips, year, returns, exemptions, agi) |>
  fmutate(
    agi_per_return=1000*agi/returns,
    agi_per_exemption=1000*agi/exemptions
  )

files_new <- list.files(path, pattern="^lp_irs_agi_county_", full.names=T)
agi_new <- rbindlist(lapply(files_new, fread), use.names=T, fill=T)

agi <- rbindlist(list(agi_old, agi_new), use.names=T, fill=T)
setorder(agi, area_fips, year)

fwrite(agi, paste0(path, "/lp_irs_agi_full.csv"))

# outflows ---------------------------------------------------------------------
flows <- c("o")
future_lapply(c(1995,1998:2010), function(y_0) {
  
  y_1 <- y_0 + 1
  file_y_0 <- y_0 - 2000
  file_y_1 <- y_1 - 2000
  
  county_conditions <- fread(paste0(path, "/lp_irs_agi_full.csv"))
  county_conditions <- county_conditions[year == y_0]
  
  if (y_0 <= 1997) county_conditions[area_fips == "12086", area_fips := "12025"]
  
  into_county_conditions <- copy(county_conditions)
  from_county_conditions <- copy(county_conditions)
  
  setnames(into_county_conditions, paste0("into_", names(into_county_conditions)))
  setnames(from_county_conditions, paste0("from_", names(from_county_conditions)))
  
  into_county_conditions[, into_area_fips := sprintf("%05d", as.integer(into_area_fips))]
  from_county_conditions[, from_area_fips := sprintf("%05d", as.integer(from_area_fips))]
  
  flow_list <- vector("list", length(flows))
  
  for (f in flows) {
    state_list <- vector("list", length(states))
    
    for (i in seq_along(states)) {
      
      state <- states[i]
      flow_name <- "outflow"
      flow_dir <- "CountyMigrationOutflow"
      
      if (y_0 <= 2003 ) {
        
        base <- paste0(
          path, "/", y_0, "to", y_1, "countymigration/",
          y_0, "to", y_1, "CountyMigration/",
          y_0, "to", y_1, flow_dir
        )
        
        if (y_0 == 2000 | y_0 == 1995 ) {
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
      
      file <- list.files(
        base,
        pattern=file_pattern,
        full.names=TRUE,
        ignore.case=TRUE
      )
      
      if (length(file) != 1) stop(paste("Found", length(file), "files for", y_0, state, flow_name))
      
      dt <- read_excel(file, skip=7, col_names=FALSE) |> data.table()
      dt <- dt[, 1:9]
      
      setnames(
        dt,
        names(dt),
        c(
          "migration_from_state", "migration_from_county",
          "migration_into_state", "migration_into_county",
          "state_abb", "desc", "returns", "exemptions", "agi"
        )
      )
      
      dt[, c("returns", "exemptions", "agi") := lapply(.SD, as.numeric),
         .SDcols=c("returns", "exemptions", "agi")]
      
      dt[, `:=`(
        into_area_fips=paste0(
          sprintf("%02d", as.integer(migration_into_state)),
          sprintf("%03d", as.integer(migration_into_county))
        ),
        from_area_fips=paste0(
          sprintf("%02d", as.integer(migration_from_state)),
          sprintf("%03d", as.integer(migration_from_county))
        )
      )]
      
      # total domestic outmigration: known destinations + IRS residual flows
      total_movers <- dt[
        (as.integer(migration_into_state) %between% c(1,56) &
           as.integer(migration_into_county) > 0) |
          (as.integer(migration_into_state) %in% c(58,59) &
             as.integer(migration_into_county) == 0),
        .(
          total_movers_returns=sum(returns, na.rm=T),
          total_movers_exemptions=sum(exemptions, na.rm=T)
        ),
        by=from_area_fips
      ]
      
      # keep only flows with identifiable destination counties
      dt <- dt[
        as.integer(migration_into_state) %between% c(1,56) &
          as.integer(migration_from_state) %between% c(1,56) &
          as.integer(migration_into_county) > 0 &
          as.integer(migration_from_county) > 0
      ]
      
      # merge local non-migrant AGI for destination and origin counties
      dt <- merge(dt, into_county_conditions, by="into_area_fips", all.x=T)
      dt <- merge(dt, from_county_conditions, by="from_area_fips", all.x=T)
      
      # destination relative to origin
      dt[, into_higher_agi := as.integer(into_agi_per_return > from_agi_per_return)]
      dt[, agi_diff := into_agi_per_return - from_agi_per_return]
      
      # destination measures among movers with observed destination AGI
      dt_from <- dt[, .(
        ew_share_into_higher_agi=weighted.mean(into_higher_agi, exemptions, na.rm=T),
        rw_share_into_higher_agi=weighted.mean(into_higher_agi, returns, na.rm=T),
        ew_mean_agi_diff=weighted.mean(agi_diff, exemptions, na.rm=T),
        rw_mean_agi_diff=weighted.mean(agi_diff, returns, na.rm=T),
        observed_movers_exemptions=sum(exemptions[!is.na(into_higher_agi)], na.rm=T),
        observed_movers_returns=sum(returns[!is.na(into_higher_agi)], na.rm=T)
      ), by=from_area_fips]
      
      # add total domestic outmigration, including unidentified destinations
      dt_from <- merge(dt_from, total_movers, by="from_area_fips", all.x=T)
      
      # fraction of total outmigration for which destination AGI is observed
      dt_from[, `:=`(
        share_destination_observed_exemptions=observed_movers_exemptions/total_movers_exemptions,
        share_destination_observed_returns=observed_movers_returns/total_movers_returns
      )]
      
      dt_from[, `:=`(year=y_0, flow=flow_name)]
      state_list[[i]] <- dt_from
      
      print(paste("Finished:", y_0, state, flow_name))
    }
    
    flow_list[[which(flows == f)]] <- rbindlist(state_list, use.names=T, fill=T)
  }
  
  irs_year <- rbindlist(flow_list, use.names=T, fill=T)
  
  fwrite(irs_year, paste0(path, "/lp_irs_agi_measure_", f, y_0, ".csv"))
})
# outflows after 2010 ----------------------------------------------------------
future_lapply(2011:2021, function(y_0) {
  
  y_1 <- y_0 + 1
  yy0 <- sprintf("%02d", y_0 %% 100)
  yy1 <- sprintf("%02d", y_1 %% 100)
  
  county_conditions <- fread(paste0(path, "/lp_irs_agi_full.csv"))
  county_conditions <- county_conditions[year == y_0]
  
  into_county_conditions <- copy(county_conditions)
  from_county_conditions <- copy(county_conditions)
  
  setnames(into_county_conditions, paste0("into_", names(into_county_conditions)))
  setnames(from_county_conditions, paste0("from_", names(from_county_conditions)))
  
  into_county_conditions[, into_area_fips := sprintf("%05d", as.integer(into_area_fips))]
  from_county_conditions[, from_area_fips := sprintf("%05d", as.integer(from_area_fips))]
  
  flow_name <- "outflow"
  file <- paste0(path, "/countyoutflow", yy0, yy1, ".csv")
  
  dt <- fread(file)
  
  setnames(dt, names(dt)[1:9],
           c("migration_from_state", "migration_from_county",
             "migration_into_state", "migration_into_county",
             "state_abb", "desc", "returns", "exemptions", "agi"))
  
  dt[, c("returns", "exemptions", "agi") := lapply(.SD, as.numeric),
     .SDcols=c("returns", "exemptions", "agi")]
  
  dt[, `:=`(
    into_area_fips=paste0(sprintf("%02d", as.integer(migration_into_state)),
                          sprintf("%03d", as.integer(migration_into_county))),
    from_area_fips=paste0(sprintf("%02d", as.integer(migration_from_state)),
                          sprintf("%03d", as.integer(migration_from_county)))
  )]
  
  # total domestic outmigration: known destinations + IRS residual flows
  total_movers <- dt[
    (as.integer(migration_into_state) %between% c(1,56) &
       as.integer(migration_into_county) > 0) |
      (as.integer(migration_into_state) %in% c(58,59) &
         as.integer(migration_into_county) == 0),
    .(
      total_movers_returns=sum(returns, na.rm=T),
      total_movers_exemptions=sum(exemptions, na.rm=T)
    ),
    by=from_area_fips
  ]
  
  # keep only flows with identifiable destination counties
  dt <- dt[
    as.integer(migration_into_state) %between% c(1,56) &
      as.integer(migration_from_state) %between% c(1,56) &
      as.integer(migration_into_county) > 0 &
      as.integer(migration_from_county) > 0
  ]
  
  # merge local non-migrant AGI for destination and origin counties
  dt <- merge(dt, into_county_conditions, by="into_area_fips", all.x=T)
  dt <- merge(dt, from_county_conditions, by="from_area_fips", all.x=T)
  
  # destination relative to origin
  dt[, into_higher_agi := as.integer(into_agi_per_return > from_agi_per_return)]
  dt[, agi_diff := into_agi_per_return - from_agi_per_return]
  
  # destination measures among movers with observed destination AGI
  dt_from <- dt[, .(
    ew_share_into_higher_agi=weighted.mean(into_higher_agi, exemptions, na.rm=T),
    rw_share_into_higher_agi=weighted.mean(into_higher_agi, returns, na.rm=T),
    ew_mean_agi_diff=weighted.mean(agi_diff, exemptions, na.rm=T),
    rw_mean_agi_diff=weighted.mean(agi_diff, returns, na.rm=T),
    observed_movers_exemptions=sum(exemptions[!is.na(into_higher_agi)], na.rm=T),
    observed_movers_returns=sum(returns[!is.na(into_higher_agi)], na.rm=T)
  ), by=from_area_fips]
  
  # add total domestic outmigration, including unidentified destinations
  dt_from <- merge(dt_from, total_movers, by="from_area_fips", all.x=T)
  
  # fraction of total outmigration with observable destination AGI
  dt_from[, `:=`(
    share_destination_observed_exemptions=observed_movers_exemptions/total_movers_exemptions,
    share_destination_observed_returns=observed_movers_returns/total_movers_returns
  )]
  
  dt_from[, `:=`(year=y_0, flow=flow_name)]
  
  fwrite(dt_from, paste0(path, "/lp_irs_agi_measure_o", y_0, ".csv"))
  print(paste("Finished:", y_0, flow_name))
})

# aggregate agi of county 
files <- list.files(path, pattern="lp_irs_agi_measure_o", full.names=T)

agi <- rbindlist(lapply(files, function(f) {
  dt <- fread(f)
  dt[, file := basename(f)]
  dt
}), use.names=T, fill=T)
summary(agi[,ew_share_into_higher_agi])
summary(agi[, ew_mean_agi_diff])

summary(agi$observed_movers_returns / agi$total_movers_returns)
summary(agi$observed_movers_exemptions / agi$total_movers_exemptions)
weighted.mean(agi$rw_share_into_higher_agi, agi$total_movers_returns, na.rm=T)
fwrite(agi, paste0(path, "/lp_irs_agi_measures.csv"))
exit 
# inflows ----------------------------------------------------------------------
flows <- c("i")
future_lapply(1998:2010, function(y_0) {
  
  y_1 <- y_0 + 1
  file_y_0 <- y_0 - 2000
  file_y_1 <- y_1 - 2000
  
  county_conditions <- fread(paste0(path, "/../output/shock_exposure.csv"))
  # county_conditions <- county_conditions[year == y_0]
  
  if (y_0 <= 1997) county_conditions[area_fips == "12086", area_fips := "12025"]
  
  to_change <- names(county_conditions)
  
  into_county_conditions <- copy(county_conditions)
  from_county_conditions <- copy(county_conditions)
  
  setnames(into_county_conditions, paste0("into_", names(into_county_conditions)))
  setnames(from_county_conditions, paste0("from_", names(from_county_conditions)))
  
  into_county_conditions[, into_area_fips := sprintf("%05d", as.integer(into_area_fips))]
  from_county_conditions[, from_area_fips := sprintf("%05d", as.integer(from_area_fips))]
  
  flow_list <- vector("list", length(flows))
  
  for (f in flows) {
    state_list <- vector("list", length(states))
    for (i in seq_along(states)) {
      state <- states[i]
      flow_name <- "inflow"
      flow_dir <- "CountyMigrationInflow"
      
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
               c("migration_into_state", "migration_into_county", 
                 "migration_from_state", "migration_from_county",
                 "state_abb", "desc", "returns", "exemptions", "agi"))
      
      dt[, c("returns", "exemptions", "agi") := lapply(.SD, as.numeric),
         .SDcols = c("returns", "exemptions", "agi")]
      
      dt <- dt[as.integer(migration_into_state) != 0 & as.integer(migration_into_state) != 0]
      
      dt[, `:=`(
        into_area_fips = paste0(sprintf("%02d", as.integer(migration_into_state)),
                                sprintf("%03d", as.integer(migration_into_county))),
        from_area_fips = paste0(sprintf("%02d", as.integer(migration_from_state)),
                                sprintf("%03d", as.integer(migration_from_county)))
      )]
      
      # remove non-migrants and IRS aggregate/residual rows
      dt <- dt[into_area_fips != from_area_fips]
      dt <- dt[
        as.integer(migration_into_state) %between% c(1, 56) &
          as.integer(migration_from_state) %between% c(1, 56) &
          as.integer(migration_into_county) > 0 &
          as.integer(migration_from_county) > 0
      ]
      
      dt <- merge(dt, into_county_conditions, by="into_area_fips", all.x=T)
      dt <- merge(dt, from_county_conditions, by="from_area_fips", all.x=T)
      
      dt <- dt[!is.na(into_IPW_US) & !is.na(from_IPW_US)]
      
      dt[, from_more_exposed := as.integer(from_IPW_US > into_IPW_US)]
      dt[, from_more_unemp := as.integer(from_unemployed_share_labor_force >
                                           into_unemployed_share_labor_force)]
      
      dt_into <- dt[, .(
        rw_mean_from_IPW_US = weighted.mean(from_IPW_US, returns, na.rm=T),
        rw_mean_IPW_diff = weighted.mean(from_IPW_US - into_IPW_US, returns, na.rm=T),
        rw_share_from_more_exposed = weighted.mean(from_more_exposed, returns, na.rm=T),
        rw_share_from_more_unemp = weighted.mean(from_more_unemp, returns, na.rm=T),
        
        ew_mean_from_IPW_US = weighted.mean(from_IPW_US, exemptions, na.rm=T),
        ew_mean_IPW_diff = weighted.mean(from_IPW_US - into_IPW_US, exemptions, na.rm=T),
        ew_share_from_more_exposed = weighted.mean(from_more_exposed, exemptions, na.rm=T),
        ew_share_from_more_unemp = weighted.mean(from_more_unemp, exemptions, na.rm=T),
        total_movers_in = sum(exemptions, na.rm=T)
      ), by=into_area_fips]
      
      dt_into[, `:=`(year = y_0, flow = flow_name)]
      state_list[[i]] <- dt_into
      
      print(paste("Finished:", y_0, state, flow_name))
    }
    
    flow_list[[which(flows == f)]] <- rbindlist(state_list, use.names = T, fill = T)
  }
  
  irs_year <- rbindlist(flow_list, use.names = T, fill = T)
  
  fwrite(irs_year, paste0(path, "/lp_irs_destination_conditions_",f, y_0, ".csv"))
}
)

# years after 2010 -------------------------------------------------------------
# outflows after 2010 ----------------------------------------------------------
future_lapply(2011:2021, function(y_0) {
  
  y_1 <- y_0 + 1
  yy0 <- sprintf("%02d", y_0 %% 100)
  yy1 <- sprintf("%02d", y_1 %% 100)
  
  county_conditions <- fread(paste0(path, "/../output/lp_irs_destination_agi_i.csv"))
  county_conditions <- county_conditions[year == y_0]
  
  into_county_conditions <- copy(county_conditions)
  from_county_conditions <- copy(county_conditions)
  
  setnames(into_county_conditions, paste0("into_", names(into_county_conditions)))
  setnames(from_county_conditions, paste0("from_", names(from_county_conditions)))
  
  into_county_conditions[, into_area_fips := sprintf("%05d", as.integer(into_area_fips))]
  from_county_conditions[, from_area_fips := sprintf("%05d", as.integer(from_area_fips))]
  
  flow_name <- "outflow"
  file <- paste0(path, "/countyoutflow", yy0, yy1, ".csv")
  
  dt <- fread(file)
  
  setnames(dt, names(dt)[1:9],
           c("migration_from_state", "migration_from_county",
             "migration_into_state", "migration_into_county",
             "state_abb", "desc", "returns", "exemptions", "agi"))
  
  dt[, c("returns", "exemptions", "agi") := lapply(.SD, as.numeric),
     .SDcols = c("returns", "exemptions", "agi")]
  
  dt[, `:=`(
    into_area_fips = paste0(sprintf("%02d", as.integer(migration_into_state)),
                            sprintf("%03d", as.integer(migration_into_county))),
    from_area_fips = paste0(sprintf("%02d", as.integer(migration_from_state)),
                            sprintf("%03d", as.integer(migration_from_county)))
  )]
  
  dt <- dt[into_area_fips != from_area_fips]
  dt <- dt[
    as.integer(migration_into_state) %between% c(1, 56) &
      as.integer(migration_from_state) %between% c(1, 56) &
      as.integer(migration_into_county) > 0 &
      as.integer(migration_from_county) > 0
  ]
  
  dt <- merge(dt, into_county_conditions, by="into_area_fips", all.x=T)
  dt <- merge(dt, from_county_conditions, by="from_area_fips", all.x=T)
  
  dt <- dt[!is.na(into_IPW_US) & !is.na(from_IPW_US)]
  
  dt[, into_less_exposed := as.integer(into_IPW_US < from_IPW_US)]
  dt[, into_less_unemp := as.integer(into_unemployed_share_labor_force <
                                       from_unemployed_share_labor_force)]
  
  dt_from <- dt[, .(
    rw_mean_into_IPW_US = weighted.mean(into_IPW_US, returns, na.rm=T),
    rw_mean_IPW_diff = weighted.mean(into_IPW_US - from_IPW_US, returns, na.rm=T),
    rw_share_into_less_exposed = weighted.mean(into_less_exposed, returns, na.rm=T),
    rw_share_into_less_unemp = weighted.mean(into_less_unemp, returns, na.rm=T),
    
    ew_mean_into_IPW_US = weighted.mean(into_IPW_US, exemptions, na.rm=T),
    ew_mean_IPW_diff = weighted.mean(into_IPW_US - from_IPW_US, exemptions, na.rm=T),
    ew_share_into_less_exposed = weighted.mean(into_less_exposed, exemptions, na.rm=T),
    ew_share_into_less_unemp = weighted.mean(into_less_unemp, exemptions, na.rm=T),
    total_movers_out = sum(exemptions, na.rm=T)
  ), by=from_area_fips]
  
  dt_from[, `:=`(year=y_0, flow=flow_name)]
  
  fwrite(dt_from, paste0(path, "/lp_irs_destination_conditions_o", y_0, ".csv"))
  print(paste("Finished:", y_0, flow_name))
}
)

# inflows after 2010 -----------------------------------------------------------
future_lapply(2011:2021, function(y_0) {
  
  y_1 <- y_0 + 1
  yy0 <- sprintf("%02d", y_0 %% 100)
  yy1 <- sprintf("%02d", y_1 %% 100)
  
  county_conditions <- fread(paste0(path, "/../output/shock_exposure.csv"))
  # county_conditions <- county_conditions[year == y_0]
  
  into_county_conditions <- copy(county_conditions)
  from_county_conditions <- copy(county_conditions)
  
  setnames(into_county_conditions, paste0("into_", names(into_county_conditions)))
  setnames(from_county_conditions, paste0("from_", names(from_county_conditions)))
  
  into_county_conditions[, into_area_fips := sprintf("%05d", as.integer(into_area_fips))]
  from_county_conditions[, from_area_fips := sprintf("%05d", as.integer(from_area_fips))]
  
  flow_name <- "inflow"
  file <- paste0(path, "/countyinflow", yy0, yy1, ".csv")
  
  dt <- fread(file)
  
  setnames(dt, names(dt)[1:9],
           c("migration_into_state", "migration_into_county",
             "migration_from_state", "migration_from_county",
             "state_abb", "desc", "returns", "exemptions", "agi"))
  
  dt[, c("returns", "exemptions", "agi") := lapply(.SD, as.numeric),
     .SDcols = c("returns", "exemptions", "agi")]
  
  dt[, `:=`(
    into_area_fips = paste0(sprintf("%02d", as.integer(migration_into_state)),
                            sprintf("%03d", as.integer(migration_into_county))),
    from_area_fips = paste0(sprintf("%02d", as.integer(migration_from_state)),
                            sprintf("%03d", as.integer(migration_from_county)))
  )]
  
  dt <- dt[into_area_fips != from_area_fips]
  dt <- dt[
    as.integer(migration_into_state) %between% c(1, 56) &
      as.integer(migration_from_state) %between% c(1, 56) &
      as.integer(migration_into_county) > 0 &
      as.integer(migration_from_county) > 0
  ]
  
  dt <- merge(dt, into_county_conditions, by="into_area_fips", all.x=T)
  dt <- merge(dt, from_county_conditions, by="from_area_fips", all.x=T)
  
  dt <- dt[!is.na(into_IPW_US) & !is.na(from_IPW_US)]
  
  dt[, from_more_exposed := as.integer(from_IPW_US > into_IPW_US)]
  dt[, from_more_unemp := as.integer(from_unemployed_share_labor_force >
                                       into_unemployed_share_labor_force)]
  
  dt_into <- dt[, .(
    rw_mean_from_IPW_US = weighted.mean(from_IPW_US, returns, na.rm=T),
    rw_mean_IPW_diff = weighted.mean(from_IPW_US - into_IPW_US, returns, na.rm=T),
    rw_share_from_more_exposed = weighted.mean(from_more_exposed, returns, na.rm=T),
    rw_share_from_more_unemp = weighted.mean(from_more_unemp, returns, na.rm=T),
    
    ew_mean_from_IPW_US = weighted.mean(from_IPW_US, exemptions, na.rm=T),
    ew_mean_IPW_diff = weighted.mean(from_IPW_US - into_IPW_US, exemptions, na.rm=T),
    ew_share_from_more_exposed = weighted.mean(from_more_exposed, exemptions, na.rm=T),
    ew_share_from_more_unemp = weighted.mean(from_more_unemp, exemptions, na.rm=T),
    total_movers_in = sum(exemptions, na.rm=T)
  ), by=into_area_fips]
  
  dt_into[, `:=`(year=y_0, flow=flow_name)]
  
  fwrite(dt_into, paste0(path, "/lp_irs_destination_conditions_i", y_0, ".csv"))
  print(paste("Finished:", y_0, flow_name))
}
)

# append and rename final file -------------------------------------------------
years <- 1998:2021
out_list <- vector("list", length(years))
in_list <- vector("list", length(years))

for (i in seq_along(years)) {
  y <- years[i]
  
  out_list[[i]] <- fread(paste0(path, "/lp_irs_destination_conditions_o", y, ".csv"))
  setnames(out_list[[i]],
           c("from_area_fips", "rw_mean_IPW_diff", "ew_mean_IPW_diff"),
           c("area_fips", "rw_mean_IPW_diff_out", "ew_mean_IPW_diff_out"))
  
  in_list[[i]] <- fread(paste0(path, "/lp_irs_destination_conditions_i", y, ".csv"))
  setnames(in_list[[i]],
           c("into_area_fips", "rw_mean_IPW_diff", "ew_mean_IPW_diff"),
           c("area_fips", "rw_mean_IPW_diff_in", "ew_mean_IPW_diff_in"))
}

irs_out <- rbindlist(out_list, use.names=T, fill=T)
irs_in <- rbindlist(in_list, use.names=T, fill=T)

irs_out[, flow := NULL]
irs_in[, flow := NULL]

irs <- merge(irs_out, irs_in, by=c("area_fips", "year"), all=T)

fwrite(irs, paste0(path, "/lp_irs_destination_conditions_full.csv"))
end <- Sys.time()
end - start 