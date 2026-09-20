################################################################################
# Created 8.10.2026
#
# Author: Sophie Handley
################################################################################

rm(list = ls())

# Paths
path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data/lodes"
path <- "D:/writing_sample/data/lodes/clean_lp_full"
output_dir <- paste0(path, "/new_clean_lp_full")
shock <- fread(paste0(path, "/../../output/lp_weighted_qcew.csv")) |> 
  fselect(year, IPW_US, IPW_OTH, area_fips) 

url <- "https://www2.census.gov/geo/docs/reference/county_adjacency/county_adjacency2010.txt"

neighbors <- fread(url, sep="\t", fill=TRUE, header=FALSE,
                   col.names=c("county_name", "county", "neighbor_name", "neighbor"))

neighbors[, county := nafill(county, type="locf")]
neighbors <- neighbors[, .(county, neighbor)]
neighbors[, neighbor_merge := 1 ]

# Annual LODES commuting measures ---------------------------------------------
outside <- fread(paste0(path,"/../raw/full_lodes_data_1990-2025.csv"))

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
fwrite(out,paste0(output_dir,"/new_commuting_measures_lp_full_1990-2025.csv"))
