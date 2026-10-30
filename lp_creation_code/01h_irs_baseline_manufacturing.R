# IRS outmigration to counties with lower baseline manufacturing shares.
# Construction only. Merge the final county-year file into the regression panel.
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")
irs_path <- file.path(path, "irs")
baseline_year <- 1990L
years <- 1994:2021  # Includes the baseline lag for a 1995 LP start.
states <- tolower(state.abb)

# One fixed manufacturing share per county; no county-year expansion in the merge.
baseline <- fread(file.path(path, "output/lp_transformed_reg_2026-10-26.csv"),
  select = c("area_fips", "year", "sh_empl_mfg"))
baseline <- baseline[year == baseline_year,
  .(area_fips = as.integer(area_fips), sh_empl_mfg)]
stopifnot(nrow(baseline) > 0, !anyDuplicated(baseline$area_fips))
from_baseline <- copy(baseline)
into_baseline <- copy(baseline)
setnames(from_baseline, c("from_area_fips", "from_mfg"))
setnames(into_baseline, c("into_area_fips", "into_mfg"))

out_list <- vector("list", length(years))
for (j in seq_along(years)) {
  y_0 <- years[j]
  y_1 <- y_0 + 1L
  yy0 <- sprintf("%02d", y_0 %% 100)
  yy1 <- sprintf("%02d", y_1 %% 100)

  if (y_0 <= 2010) {
    # Same historical IRS layout and column order as 01g.
    folder <- if (y_0 <= 2003) file.path(irs_path,
      paste0(y_0,"to",y_1,"countymigration"),
      paste0(y_0,"to",y_1,"CountyMigration"),
      paste0(y_0,"to",y_1,"CountyMigrationOutflow")) else
        file.path(irs_path, paste0("county", yy0, yy1))
    if (!dir.exists(folder)) {
      warning("No IRS folder for ", y_0, "; year left missing")
      next
    }
    state_list <- lapply(states, function(st) {
      pattern <- if (y_0 %in% c(1995, 2000)) paste0("^co.*",st,"or\\.xls$") else
        if (y_0 <= 2003) paste0("^co.*",st,"o\\.xls$") else
        if (y_0 <= 2006) paste0("^co",yy0,yy1,st,"o\\.xls$") else
          paste0("^co",yy0,yy1,"o",st,"\\.xls$")
      f <- list.files(folder, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
      if (length(f) != 1) stop("Check IRS file for ", st, " ", y_0)
      as.data.table(read_excel(f, skip = 7, col_names = FALSE,
        .name_repair = "minimal"))[, 1:9]
    })
    dt <- rbindlist(state_list, use.names = FALSE)
  } else {
    f <- file.path(irs_path, paste0("countyoutflow", yy0, yy1, ".csv"))
    if (!file.exists(f)) {
      warning("No IRS file for ", y_0, "; year left missing")
      next
    }
    dt <- fread(f)[, 1:9]
  }

  # Local 2013-2014 file contains exact repeated rows; never sum those copies.
  dt <- unique(dt)
  setnames(dt, c("from_state", "from_county", "into_state", "into_county",
    "state_abb", "description", "returns", "exemptions", "agi"))
  cols <- c("from_state", "from_county", "into_state", "into_county", "returns", "exemptions")
  dt[, (cols) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))), .SDcols = cols]
  dt <- dt[from_state %between% c(1,56) & into_state %between% c(1,56) &
    from_county > 0 & into_county > 0]
  dt[, `:=`(from_area_fips = as.integer(from_state * 1000 + from_county),
    into_area_fips = as.integer(into_state * 1000 + into_county))]
  dt[from_area_fips == 12025, from_area_fips := 12086L]
  dt[into_area_fips == 12025, into_area_fips := 12086L]
  dt <- dt[from_area_fips != into_area_fips]
  # Matching county FIPS excludes IRS aggregate/residual rows.
  dt <- merge(dt, from_baseline, by = "from_area_fips")
  dt <- merge(dt, into_baseline, by = "into_area_fips")
  stopifnot(!anyDuplicated(dt[, .(from_area_fips, into_area_fips)]))
  dt <- dt[is.finite(from_mfg) & is.finite(into_mfg)]
  dt[, lower_mfg := as.integer(into_mfg < from_mfg)]
  out_list[[j]] <- dt[, .(
    ew_share_into_lower_baseline_mfg = if (any(is.finite(exemptions) & exemptions > 0))
      weighted.mean(lower_mfg[is.finite(exemptions) & exemptions > 0],
        exemptions[is.finite(exemptions) & exemptions > 0]) else NA_real_,
    rw_share_into_lower_baseline_mfg = if (any(is.finite(returns) & returns > 0))
      weighted.mean(lower_mfg[is.finite(returns) & returns > 0],
        returns[is.finite(returns) & returns > 0]) else NA_real_
  ), by = .(area_fips = from_area_fips)][, year := y_0]
  message("Finished IRS outflows: ", y_0)
}

irs_baseline_mfg <- rbindlist(out_list)
setorder(irs_baseline_mfg, area_fips, year)
stopifnot(!anyDuplicated(irs_baseline_mfg[, .(area_fips, year)]))
fwrite(irs_baseline_mfg, file.path(irs_path,
  paste0("irs_destination_manufacturing_baseline_", baseline_year, ".csv")))
