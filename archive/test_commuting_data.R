

rm(list = ls())

path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"

qcew <- fread(paste0(path, "/output/weighted_qcew.csv"))

years <- c(2002, 2007, 2013) 

for (i in 1:length(years)){
  year <- years[i]
  
  # has location w_geocode and h_geocode which are census blocks
  lodes <- fread(paste0(path, "/lodes/clean/lodes_", year, ".csv"))
  
  # has location county and census block 
  crosswalk <- fread(paste0(path, "/nhgis_blk2000_co2015/nhgis_blk2000_co2015.csv"))
  # need to go from lodes census block -> crosswalk census block -> county -> qcew county 
  crosswalk[,state:=floor(blk2000ge/1e13)]
  
  # merge on block group for home and workplace 
  lodes <- merge(lodes, crosswalk, by.x = "h_geocode", by.y = "blk2000ge")
  setnames(lodes, "co2015ge", "h_county")
  lodes <- merge(lodes, crosswalk, by.x = "w_geocode", by.y = "blk2000ge")
  setnames(lodes, "co2015ge", "w_county")
  
  # prepare lodes data -----------------------------------------------------------
  # collapse to a sum of all jobs outside of the county 
  lodes[,outside := ifelse(h_county != w_county, S000, 0)]
  
  outside <- lodes |> fgroup_by(h_county, year) |>
    fsummarise(all_jobs = fsum(S000),
               outside_jobs = fsum(outside)) 
  
  outside[,outside_d_jobs := outside_jobs / all_jobs]
  
  new <- merge(outside, qcew, by.x = c("h_county","year"), by.y = c("area_fips", "year"))
  
  fwrite(new, paste0(path, "/lodes/clean/lodes_", year, "_collapsed.csv"))
  
  counties <- counties(cb = TRUE, year = 2020)
  
  ggplot(counties) +
    geom_sf(fill = "white", color = "grey70", linewidth = 0.1) +
    theme_void()
  
  
  counties$area_fips <- as.integer(counties$GEOID)
  
  map_dt <- merge(
    counties,
    new,
    by.x = "GEOID",
    by.y = "h_county", 
    all.x = TRUE
  )
  
  ggplot(map_dt) +
    geom_sf(aes(fill = outside_d_jobs), color = "grey70", linewidth = 0.1) +
    coord_sf(
      xlim = c(-125, -66),
      ylim = c(24, 50)
    ) +
    theme_void()
  
  
  map_dt <- merge(
    counties,
    new,
    by.x = "GEOID",
    by.y = "h_county"
  )
  
  
  ggplot(map_dt) +
    geom_sf(aes(fill = outside_d_jobs), color = "grey70", linewidth = 0.1) +
    coord_sf(
      xlim = c(-80, -90),
      ylim = c(34, 50)
    ) +
    theme_void()
  
  ggsave(filename = paste0(path, "/../figures/lodes_outside_d_jobs_", year, ".pdf"))
  
  rm(lodes, outside, map_dt, new, counties)
}





















