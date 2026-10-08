# Setup ------------------------------------------------------------------------

rm(list=ls())
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")

lodes_path <- paste0(path, "/lodes/clean_lp_full")
output_dir <- paste0(path, "/output")


# Read shocks and crosswalk ----------------------------------------------------

shock <- fread(paste0(output_dir, "/lp_weighted_qcew_CZ.csv"))[
  , .(commuting_zone_id_2000, year, IPW_US, IPW_OTH, baseline_emp)
]
stopifnot(!anyDuplicated(shock[, .(commuting_zone_id_2000, year)]))

crosswalk <- read_excel(paste0(path, "/cz00eqvv1.xls")) |>
  data.table() |> clean_names()

cz_crosswalk <- unique(crosswalk[
  !is.na(commuting_zone_id_2000),
  .(county=as.integer(fips), cz=as.integer(commuting_zone_id_2000))
])
stopifnot(!anyDuplicated(cz_crosswalk$county))


# County adjacency -------------------------------------------------------------

url <- "https://www2.census.gov/geo/docs/reference/county_adjacency/county_adjacency2010.txt"

neighbors <- fread(url, sep="\t", fill=TRUE, header=FALSE,
                   col.names=c("county_name", "county", "neighbor_name", "neighbor"))

neighbors[, county := nafill(as.integer(county), type="locf")]
neighbors[, neighbor := as.integer(neighbor)]
neighbors <- unique(neighbors[!is.na(neighbor), .(county, neighbor)])
neighbors[, neighbor_merge := 1L]


# Geographic neighboring-CZ matrix --------------------------------------------

neighbors_cz <- merge(neighbors[, .(county, neighbor)], cz_crosswalk,
                      by="county", all.x=TRUE)

neighbors_cz <- merge(neighbors_cz, cz_crosswalk,
                      by.x="neighbor", by.y="county", all.x=TRUE)

setnames(neighbors_cz, c("cz.x", "cz.y"), c("home_cz", "neighbor_cz"))

neighbors_cz <- unique(neighbors_cz[
  !is.na(home_cz) & !is.na(neighbor_cz) & home_cz != neighbor_cz,
  .(home_cz, neighbor_cz)
])


# Geographic neighboring-CZ shocks --------------------------------------------

neighbors_ipw <- merge(neighbors_cz, shock,
                       by.x="neighbor_cz", by.y="commuting_zone_id_2000",
                       all.x=TRUE, allow.cartesian=TRUE)

neighbor_shock <- neighbors_ipw[!is.na(year), .(
  mean_neighbor_IPW_US = mean(IPW_US, na.rm=TRUE),
  mean_neighbor_IPW_OTH = mean(IPW_OTH, na.rm=TRUE),
  empw_neighbor_IPW_US = weighted.mean(
    IPW_US[!is.na(baseline_emp)], baseline_emp[!is.na(baseline_emp)], na.rm=TRUE
  ),
  empw_neighbor_IPW_OTH = weighted.mean(
    IPW_OTH[!is.na(baseline_emp)], baseline_emp[!is.na(baseline_emp)], na.rm=TRUE
  ),
  n_neighbors = .N
), by=.(commuting_zone_id_2000=home_cz, year)]

# Convert undefined means to NA
neighbor_cols <- grep("neighbor_IPW", names(neighbor_shock), value=TRUE)
for (v in neighbor_cols)
  set(neighbor_shock, which(is.nan(neighbor_shock[[v]])), v, NA_real_)

fwrite(neighbor_shock, paste0(output_dir, "/neighbor_shock_CZ.csv"))


# Read commuting data ----------------------------------------------------------

outside <- fread(paste0(lodes_path, "/../raw/full_lodes_data_1990-2025.csv"))

# Residence CZ
outside <- merge(outside, cz_crosswalk, by="county", all.x=TRUE)
setnames(outside, "cz", "commuting_zone_id_2000")

# Workplace CZ
work_cz <- copy(cz_crosswalk)
setnames(work_cz, c("county", "cz"), c("w_county", "work_cz"))
outside <- merge(outside, work_cz, by="w_county", all.x=TRUE)

print(outside[, .(
  missing_home_cz=sum(is.na(commuting_zone_id_2000)),
  missing_work_cz=sum(is.na(work_cz))
)])


# Attach home- and workplace-CZ shocks -----------------------------------------

home_shock <- shock[, .(
  commuting_zone_id_2000, year,
  home_IPW_US=IPW_US, home_IPW_OTH=IPW_OTH
)]

work_shock <- shock[, .(
  work_cz=commuting_zone_id_2000, year,
  work_IPW_US=IPW_US, work_IPW_OTH=IPW_OTH
)]

outside <- merge(outside, home_shock,
                 by=c("commuting_zone_id_2000", "year"), all.x=TRUE)

outside <- merge(outside, work_shock,
                 by=c("work_cz", "year"), all.x=TRUE)

outside <- merge(outside, neighbors,
                 by.x=c("county", "w_county"),
                 by.y=c("county", "neighbor"), all.x=TRUE)

outside[is.na(neighbor_merge), neighbor_merge := 0L]

outside[, work_less_exposure := as.integer(work_IPW_US < home_IPW_US)]


# CZ commuting totals ----------------------------------------------------------

# Outside-county commuters include workers commuting within their home CZ.

commuting_cz <- outside[!is.na(commuting_zone_id_2000), .(
  com_resident_emp = sum(S000, na.rm=TRUE),
  com_inside_jobs = sum(S000[county == w_county], na.rm=TRUE),
  com_outside_jobs = sum(S000[county != w_county], na.rm=TRUE),
  com_adjacent_jobs = sum(S000[county != w_county & neighbor_merge == 1], na.rm=TRUE),
  com_nonadjacent_jobs = sum(S000[county != w_county & neighbor_merge == 0], na.rm=TRUE)
), by=.(commuting_zone_id_2000, year)]

commuting_cz[, `:=`(
  com_outside_share_all = com_outside_jobs / com_resident_emp,
  com_adjacent_share_external = com_adjacent_jobs / com_outside_jobs,
  com_nonadjacent_share_external = com_nonadjacent_jobs / com_outside_jobs,
  com_adjacent_share_all = com_adjacent_jobs / com_resident_emp,
  com_nonadjacent_share_all = com_nonadjacent_jobs / com_resident_emp
)]


# External commuting network ---------------------------------------------------

external_network_shock <- outside[
  county != w_county & !is.na(commuting_zone_id_2000),
  .(
    ex_mean_work_IPW_US = weighted.mean(work_IPW_US, S000, na.rm=TRUE),
    ex_mean_work_IPW_OTH = weighted.mean(work_IPW_OTH, S000, na.rm=TRUE),
    ex_share_less_exposed = weighted.mean(work_less_exposure, S000, na.rm=TRUE),
    ex_share_neighbor_commuters = weighted.mean(neighbor_merge, S000, na.rm=TRUE),
    
    ex_mean_neighbor_IPW_US = weighted.mean(
      work_IPW_US[neighbor_merge == 1], S000[neighbor_merge == 1], na.rm=TRUE
    ),
    ex_mean_neighbor_IPW_OTH = weighted.mean(
      work_IPW_OTH[neighbor_merge == 1], S000[neighbor_merge == 1], na.rm=TRUE
    )
  ),
  by=.(commuting_zone_id_2000, year)
]


# Full employment network ------------------------------------------------------

full_network_shock <- outside[!is.na(commuting_zone_id_2000), .(
  fn_mean_work_IPW_US = weighted.mean(work_IPW_US, S000, na.rm=TRUE),
  fn_mean_work_IPW_OTH = weighted.mean(work_IPW_OTH, S000, na.rm=TRUE),
  fn_share_less_exposed = weighted.mean(work_less_exposure, S000, na.rm=TRUE),
  
  fn_goods_mean_work_IPW_US = weighted.mean(
    work_IPW_US[!is.na(SI01)], SI01[!is.na(SI01)], na.rm=TRUE
  ),
  fn_goods_mean_work_IPW_OTH = weighted.mean(
    work_IPW_OTH[!is.na(SI01)], SI01[!is.na(SI01)], na.rm=TRUE
  ),
  fn_goods_share_less_exposed = weighted.mean(
    work_less_exposure[!is.na(SI01)], SI01[!is.na(SI01)], na.rm=TRUE
  ),
  
  fn_servc_mean_work_IPW_US = weighted.mean(
    work_IPW_US[!is.na(SI03)], SI03[!is.na(SI03)], na.rm=TRUE
  ),
  fn_servc_mean_work_IPW_OTH = weighted.mean(
    work_IPW_OTH[!is.na(SI03)], SI03[!is.na(SI03)], na.rm=TRUE
  ),
  fn_servc_share_less_exposed = weighted.mean(
    work_less_exposure[!is.na(SI03)], SI03[!is.na(SI03)], na.rm=TRUE
  ),
  
  fn_mean_neighbor_IPW_US = weighted.mean(
    work_IPW_US[neighbor_merge == 1 | county == w_county],
    S000[neighbor_merge == 1 | county == w_county], na.rm=TRUE
  ),
  fn_mean_neighbor_IPW_OTH = weighted.mean(
    work_IPW_OTH[neighbor_merge == 1 | county == w_county],
    S000[neighbor_merge == 1 | county == w_county], na.rm=TRUE
  )
), by=.(commuting_zone_id_2000, year)]


# Final CZ-year panel ----------------------------------------------------------

out <- merge(commuting_cz, external_network_shock,
             by=c("commuting_zone_id_2000", "year"), all=TRUE)

out <- merge(out, full_network_shock,
             by=c("commuting_zone_id_2000", "year"), all=TRUE)

out <- merge(out, neighbor_shock,
             by=c("commuting_zone_id_2000", "year"), all.x=TRUE)

# Convert undefined weighted means to NA
network_cols <- grep("^(ex_|fn_)", names(out), value=TRUE)
for (v in network_cols)
  set(out, which(is.nan(out[[v]])), v, NA_real_)

setorder(out, commuting_zone_id_2000, year)
stopifnot(!anyDuplicated(out[, .(commuting_zone_id_2000, year)]))

fwrite(out, paste0(output_dir, "/new_commuting_measures_CZ_1990-2025.csv"))