################################################################################
# Created 10.24.2026
#
# Author: Sophie Handley
################################################################################

rm(list = ls())

# Paths
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")

lodes_path <- paste0(path, "/lodes/clean_lp_full")
output_dir <- paste0(lodes_path, "/new_clean_lp_full")
shock <- fread(paste0(lodes_path, "/../../output/lp_weighted_qcew.csv")) |> 
  as.data.table()
shock <- shock[, .(year, IPW_US, IPW_OTH, area_fips, baseline_emp)] 

url <- "https://www2.census.gov/geo/docs/reference/county_adjacency/county_adjacency2010.txt"

neighbors <- fread(url, sep="\t", fill=TRUE, header=FALSE,
                   col.names=c("county_name", "county", "neighbor_name", "neighbor"))

neighbors[, county := nafill(county, type="locf")]
neighbors <- neighbors[, .(county, neighbor)]
neighbors[, neighbor_merge := 1 ]



# Full main-plus-auxiliary commuting and network measures.
# Geographic neighbor shocks are unchanged and remain produced by 02c.

# Industry composition from full main + auxiliary resident-to-workplace flows.
# SI01 = goods; SI02 = trade/transportation/utilities; SI03 = all other services.
# These are employment sectors, not separate manufacturing-only counts.
create_lodes_sector_measures <- function(flows) {
  required <- c("county", "w_county", "year", "S000", "SI01", "SI02", "SI03")
  stopifnot(all(required %in% names(flows)))
  sector_codes <- c(goods = "SI01", trade_transport_utilities = "SI02", other_services = "SI03")
  sector <- flows[, {
    inside <- county == w_county
    values <- list(
      sector_total_jobs = sum(as.double(S000), na.rm = TRUE),
      sector_inside_jobs = sum(as.double(S000[inside]), na.rm = TRUE),
      sector_outside_jobs = sum(as.double(S000[!inside]), na.rm = TRUE)
    )
    for (name in names(sector_codes)) {
      x <- as.double(get(sector_codes[[name]]))
      # Missing industry data means unknown, including the Census-only years.
      available <- all(is.finite(x) & x >= 0)
      values[[paste0("com_", name, "_jobs")]] <- if (available) sum(x) else NA_real_
      values[[paste0("com_", name, "_inside_jobs")]] <- if (available) sum(x[inside]) else NA_real_
      values[[paste0("com_", name, "_outside_jobs")]] <- if (available) sum(x[!inside]) else NA_real_
    }
    values
  }, by = .(county, year)]

  # Non-goods combines SI02 and SI03; retain both components separately as well.
  for (ending in c("jobs", "inside_jobs", "outside_jobs")) {
    sector[, (paste0("com_nongoods_", ending)) :=
      get(paste0("com_trade_transport_utilities_", ending)) +
      get(paste0("com_other_services_", ending))]
  }
  divide <- function(numerator, denominator) {
    fifelse(is.finite(numerator) & is.finite(denominator) & denominator > 0,
            numerator / denominator, NA_real_)
  }
  for (name in c(names(sector_codes), "nongoods")) {
    total <- sector[[paste0("com_", name, "_jobs")]]
    inside <- sector[[paste0("com_", name, "_inside_jobs")]]
    outside <- sector[[paste0("com_", name, "_outside_jobs")]]
    sector[, (paste0("com_", name, "_share_total_jobs")) := divide(total, sector_total_jobs)]
    sector[, (paste0("com_", name, "_inside_share_total_jobs")) := divide(inside, sector_total_jobs)]
    sector[, (paste0("com_", name, "_outside_share_total_jobs")) := divide(outside, sector_total_jobs)]
    sector[, (paste0("com_", name, "_share_inside_jobs")) := divide(inside, sector_inside_jobs)]
    sector[, (paste0("com_", name, "_share_outside_jobs")) := divide(outside, sector_outside_jobs)]
    sector[, (paste0("com_", name, "_inside_share_sector_jobs")) := divide(inside, total)]
    sector[, (paste0("com_", name, "_outside_share_sector_jobs")) := divide(outside, total)]
    stopifnot(all(is.na(total) | abs(total - inside - outside) < 1e-8))
  }
  # Sector counts must reconcile with the corresponding all-sector flow counts.
  for (ending in c("jobs", "inside_jobs", "outside_jobs")) {
    total <- sector[[paste0("sector_", if (ending == "jobs") "total_jobs" else ending)]]
    components <- sector[[paste0("com_goods_", ending)]] +
      sector[[paste0("com_trade_transport_utilities_", ending)]] +
      sector[[paste0("com_other_services_", ending)]]
    stopifnot(all(is.na(components) | abs(components - total) < 1e-8))
  }
  sector[, c("sector_total_jobs", "sector_inside_jobs", "sector_outside_jobs") := NULL]
  stopifnot(!anyDuplicated(sector[, .(county, year)]))
  sector[]
}


# Monthly earnings: low <=1250; middle 1251-3333; high >3333 dollars.
# SE categories are separate from industry categories; no industry-by-earnings cross-tab.
create_lodes_earnings_measures <- function(flows) {
  required <- c("county", "w_county", "year", "S000", "SE01", "SE02", "SE03")
  stopifnot(all(required %in% names(flows)))
  sector_codes <- c(low_earnings = "SE01", middle_earnings = "SE02", high_earnings = "SE03")
  sector <- flows[, {
    inside <- county == w_county
    values <- list(
      sector_total_jobs = sum(as.double(S000), na.rm = TRUE),
      sector_inside_jobs = sum(as.double(S000[inside]), na.rm = TRUE),
      sector_outside_jobs = sum(as.double(S000[!inside]), na.rm = TRUE)
    )
    for (name in names(sector_codes)) {
      x <- as.double(get(sector_codes[[name]]))
      # Missing earnings data means unknown, including the Census-only years.
      available <- all(is.finite(x) & x >= 0)
      values[[paste0("com_", name, "_jobs")]] <- if (available) sum(x) else NA_real_
      values[[paste0("com_", name, "_inside_jobs")]] <- if (available) sum(x[inside]) else NA_real_
      values[[paste0("com_", name, "_outside_jobs")]] <- if (available) sum(x[!inside]) else NA_real_
    }
    values
  }, by = .(county, year)]

  divide <- function(numerator, denominator) {
    fifelse(is.finite(numerator) & is.finite(denominator) & denominator > 0,
            numerator / denominator, NA_real_)
  }
  for (name in names(sector_codes)) {
    total <- sector[[paste0("com_", name, "_jobs")]]
    inside <- sector[[paste0("com_", name, "_inside_jobs")]]
    outside <- sector[[paste0("com_", name, "_outside_jobs")]]
    sector[, (paste0("com_", name, "_share_total_jobs")) := divide(total, sector_total_jobs)]
    sector[, (paste0("com_", name, "_inside_share_total_jobs")) := divide(inside, sector_total_jobs)]
    sector[, (paste0("com_", name, "_outside_share_total_jobs")) := divide(outside, sector_total_jobs)]
    sector[, (paste0("com_", name, "_share_inside_jobs")) := divide(inside, sector_inside_jobs)]
    sector[, (paste0("com_", name, "_share_outside_jobs")) := divide(outside, sector_outside_jobs)]
    sector[, (paste0("com_", name, "_inside_share_earnings_jobs")) := divide(inside, total)]
    sector[, (paste0("com_", name, "_outside_share_earnings_jobs")) := divide(outside, total)]
    stopifnot(all(is.na(total) | abs(total - inside - outside) < 1e-8))
  }
  # Earnings-group counts must reconcile with the corresponding all-sector flow counts.
  for (ending in c("jobs", "inside_jobs", "outside_jobs")) {
    total <- sector[[paste0("sector_", if (ending == "jobs") "total_jobs" else ending)]]
    components <- sector[[paste0("com_low_earnings_", ending)]] +
      sector[[paste0("com_middle_earnings_", ending)]] +
      sector[[paste0("com_high_earnings_", ending)]]
    stopifnot(all(is.na(components) | abs(components - total) < 1e-8))
  }
  sector[, c("sector_total_jobs", "sector_inside_jobs", "sector_outside_jobs") := NULL]
  stopifnot(!anyDuplicated(sector[, .(county, year)]))
  sector[]
}


# Annual LODES commuting measures ---------------------------------------------
outside <- fread(paste0(lodes_path,"/../raw/full_lodes_data_with_aux_1990-2025.csv"))

stopifnot(!anyDuplicated(outside[, .(county, w_county, year)]))
stopifnot(!anyDuplicated(shock[, .(area_fips, year)]))
stopifnot(!anyDuplicated(neighbors[, .(county, neighbor)]))

# Home-county shock
outside <- merge(
  outside,shock,
  by.x=c("county","year"),
  by.y=c("area_fips","year"),
  all.x=TRUE
)

setnames(outside,c("IPW_US","IPW_OTH"),c("county_IPW_US","county_IPW_OTH"))

# Workplace-county shock
outside <- merge(
  outside,shock,
  by.x=c("w_county","year"),
  by.y=c("area_fips","year"),
  all.x=TRUE
)
setnames(outside,c("IPW_US","IPW_OTH"),c("work_IPW_US","work_IPW_OTH"))

outside[, work_less_exposure :=
          as.integer(work_IPW_US < county_IPW_US)]

# All commuting patterns by distance  ------------------------------------------
commuting <- merge(
  outside,neighbors,
  by.x=c("county","w_county"),
  by.y=c("county","neighbor"),
  all.x=TRUE
)

commuting[is.na(neighbor_merge),neighbor_merge := 0L]

commuting <- commuting[, .(
  com_resident_emp=sum(S000,na.rm=TRUE),
  com_inside_jobs=sum(S000[county==w_county],na.rm=TRUE),
  com_outside_jobs=sum(S000[county!=w_county],na.rm=TRUE),
  com_adjacent_jobs=sum(S000[county!=w_county & neighbor_merge==1],na.rm=TRUE),
  com_nonadjacent_jobs=sum(S000[county!=w_county & neighbor_merge==0],na.rm=TRUE)
), by=.(county,year)]

commuting[, `:=`(
  com_outside_share_all=com_outside_jobs/com_resident_emp,
  com_adjacent_share_external=com_adjacent_jobs/com_outside_jobs,
  com_nonadjacent_share_external=com_nonadjacent_jobs/com_outside_jobs,
  com_adjacent_share_all=com_adjacent_jobs/com_resident_emp,
  com_nonadjacent_share_all=com_nonadjacent_jobs/com_resident_emp
)]

# Additional industry-sector counts and raw composition shares (no winsorization).
sector_commuting <- create_lodes_sector_measures(outside)
commuting <- merge(commuting, sector_commuting, by = c("county", "year"), all.x = TRUE)

# Earnings-group counts and shares from the same full resident-to-workplace flows.
earnings_commuting <- create_lodes_earnings_measures(outside)
commuting <- merge(commuting, earnings_commuting, by = c("county", "year"), all.x = TRUE)

# External commuting network ------------------------------------------------
external_network_shock <- merge(
  outside,neighbors,
  by.x=c("county","w_county"),
  by.y=c("county","neighbor"),
  all.x=TRUE
)

external_network_shock[is.na(neighbor_merge),neighbor_merge := 0L]
external_network_shock <- external_network_shock[county != w_county]

external_network_shock <- external_network_shock[, .(
  # All external commuting destinations
  ex_mean_work_IPW_US=weighted.mean(work_IPW_US,S000,na.rm=TRUE),
  ex_mean_work_IPW_OTH=weighted.mean(work_IPW_OTH,S000,na.rm=TRUE),
  ex_share_less_exposed=weighted.mean(work_less_exposure,S000,na.rm=TRUE),
  ex_share_neighbor_commuters=weighted.mean(neighbor_merge,S000,na.rm=TRUE),
  
  # Adjacent commuting destinations only
  ex_mean_neighbor_IPW_US=weighted.mean(
    work_IPW_US[neighbor_merge == 1],
    S000[neighbor_merge == 1],
    na.rm=TRUE
  ),
  ex_mean_neighbor_IPW_OTH=weighted.mean(
    work_IPW_OTH[neighbor_merge == 1],
    S000[neighbor_merge == 1],
    na.rm=TRUE
  )
), by=.(county,year)]

# Full employment network ------------------------------------------------------
# Includes workers employed in their home county AND outside counties

full_network_shock <- merge(
  outside,neighbors,
  by.x=c("county","w_county"),
  by.y=c("county","neighbor"),
  all.x=TRUE
)

full_network_shock[is.na(neighbor_merge),neighbor_merge := 0L]

full_network_shock <- full_network_shock[, .(
  # All workplace destinations
  fn_mean_work_IPW_US=weighted.mean(work_IPW_US,S000,na.rm=TRUE),
  fn_mean_work_IPW_OTH=weighted.mean(work_IPW_OTH,S000,na.rm=TRUE),
  fn_share_less_exposed=weighted.mean(work_less_exposure,S000,na.rm=TRUE),
  
  # Goods workers
  fn_goods_mean_work_IPW_US=weighted.mean(work_IPW_US,SI01,na.rm=TRUE),
  fn_goods_mean_work_IPW_OTH=weighted.mean(work_IPW_OTH,SI01,na.rm=TRUE),
  fn_goods_share_less_exposed=weighted.mean(work_less_exposure,SI01,na.rm=TRUE),
  
  # Service workers
  fn_servc_mean_work_IPW_US=weighted.mean(work_IPW_US,SI03,na.rm=TRUE),
  fn_servc_mean_work_IPW_OTH=weighted.mean(work_IPW_OTH,SI03,na.rm=TRUE),
  fn_servc_share_less_exposed=weighted.mean(work_less_exposure,SI03,na.rm=TRUE),
  fn_mean_neighbor_IPW_US=weighted.mean(
    work_IPW_US[neighbor_merge==1 | county==w_county],
    S000[neighbor_merge==1 | county==w_county],
    na.rm=TRUE
  ),
  fn_mean_neighbor_IPW_OTH=weighted.mean(
    work_IPW_OTH[neighbor_merge==1 | county==w_county],
    S000[neighbor_merge==1 | county==w_county],
    na.rm=TRUE
  )
  
), by=.(county,year)]


# Final county x year panel
out <- merge(
  full_network_shock,
  external_network_shock,
  by=c("county","year"),
  all=TRUE
)
out <- merge(out,commuting,by=c("county","year"),all=TRUE)
setorder(out,county,year)

stopifnot(!anyDuplicated(out[, .(county, year)]))
# aux_ denotes full main + auxiliary coverage, matching the names in 03.
measure_cols <- setdiff(names(out), c("county", "year"))
setnames(out, measure_cols, paste0("aux_", measure_cols))
fwrite(out, paste0(output_dir, "/new_commuting_measures_lp_full_with_aux_1990-2025.csv"))
