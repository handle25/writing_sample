
rm(list = ls())


library(collapse) 
library(readxl)
library(data.table)
library(fixest)
library(sf)
library(haven)
library(tigris)
library(ggplot2)
library(janitor)
# qcewdata 
path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
setwd(path)

# read in data -----------------------------------------------------------------
# has location area_fips which is county level 
qcew <- fread(paste0(path, "/qcew/clean/michigan.csv"))
qcew[,hierarchy:= nchar(industry_code)]
# has location w_geocode and h_geocode which are census blocks
lodes <- fread(paste0(path, "/lodes/clean/mi_full_2002_2020.csv"))

# has location county and census block 
crosswalk <- fread(paste0(path, "/nhgis_blk2000_co2015/nhgis_blk2000_co2015.csv"))

# need to go from lodes census block -> crosswalk census block -> county -> qcew county 
crosswalk[,state:=floor(blk2000ge/1e13)]
crosswalk <- crosswalk[state == 26,]

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

# prepare qcew industry data ---------------------------------------------------
# get naics6 county data, we have employment. Can then merge this to the instrument. 
naics6 <- qcew[agglvl_code == 78,]
naics6 <- naics6 |> fgroup_by(area_fips, year, agglvl_code) |> 
  fmutate(gdp = fsum(total_annual_wages), 
          county_jobs = fsum(annual_avg_emplvl), 
          county_estabs = fsum(annual_avg_estabs_count))

manufac <- naics2[industry_code == "31-33" & own_code == 5,]
service <- naics2[industry_code == "31-33" & own_code == 5,]

manufac[, `:=` (
  m_d_gdp = total_annual_wages / gdp , 
  m_d_jobs = annual_avg_emplvl / county_jobs, 
  m_d_est = annual_avg_estabs_count / county_estabs
)]


clean_qcew <- manufac |> fselect(m_d_est, m_d_jobs, m_d_gdp, year, gdp, county_jobs, area_fips)

manufac_with_flows <- merge(clean_qcew, outside, 
                            by.x = c("year","area_fips"), 
                            by.y = c("year","h_county"))

setorder(manufac_with_flows, area_fips, year)


manufac_with_flows[, d_outside_d_jobs :=
                     outside_d_jobs - shift(outside_d_jobs),
                   by = area_fips]

manufac_with_flows[, d_m_d_gdp :=
                     m_d_gdp - shift(m_d_gdp),
                   by = area_fips]
# analysis ---------------------------------------------------------------------
year <- c(unique(manufac_with_flows[,year]))


vars <- c("d_m_d_gdp") #"d_outside_d_jobs")

# for (v in range(length(vars))){
#   fill_range <- range(manufac_with_flows[,get(vars[v])], na.rm = TRUE)
#   fill_range <- range(-.15,.15)
#   for (y in year){
#     year_dt <- manufac_with_flows[year==y,]
#     
#     # Download US counties
#     counties <- counties(cb = TRUE, year = 2020)
#     
#     # Keep Michigan
#     mi <- counties[counties$STATEFP == "26", ]
#     
#     # Convert GEOID to integer to match your data
#     mi$area_fips <- as.integer(mi$GEOID)
#     
#     # Merge your county-level data
#     map_dt <- merge(mi, year_dt, by = "area_fips", all.y = TRUE)
#     
#     ggplot(map_dt) +
#       geom_sf(aes(fill = get(vars[v]))) +
#       geom_sf(data = mi, fill = NA, color = "black") +
#       coord_sf(
#         xlim = st_bbox(mi)[c("xmin", "xmax")],
#         ylim = st_bbox(mi)[c("ymin", "ymax")]
#       ) +
#       scale_fill_viridis_c(limits = fill_range) +
#       theme_void()
#     ggsave(paste0("C:/Users/Sophie/Desktop/phd_apps/writing_sample/figures/",vars[v],y,".pdf"))
#   }
# }


feols(outside_d_jobs ~ m_d_gdp + all_jobs | area_fips + year, data = manufac_with_flows)

manufac_with_flows[,log_all_jobs := log(all_jobs)]
vars <- c("m_d_gdp","all_jobs","log_all_jobs") 

for (v in seq_along(vars)){
  
  baseline <- manufac_with_flows[year == 2002, 
                                 .(area_fips, value = get(vars[v]))]
  
  setnames(baseline, "value", paste0(vars[v], "_2002_pre"))
  
  manufac_with_flows <- merge(manufac_with_flows, baseline, 
                              by = "area_fips",
                              all.x = TRUE)
}
manufac_with_flows[,m_d_gdp_2002 := m_d_gdp - m_d_gdp_2002_pre]
manufac_with_flows[,log_all_jobs_2002 := log_all_jobs - log_all_jobs_2002_pre]

feols(outside_d_jobs ~ m_d_gdp_2002 + log_all_jobs_2002 | area_fips + year, data = manufac_with_flows)


# trade data -> naics for naics level shock ------------------------------------ 
shock <- data.table(read_stata(paste0(path, "/112670-V1/Public-Release-Data/dta/sic87dd_trade_data.dta")))
crosswalk_naics <- data.table(fread(paste0(path, "/my_crosswalk.csv"))) |> clean_names()

# merge in naics 
shock_naics <- merge(shock, crosswalk_naics, by.x = "sic87dd", by.y = "sic87")
shock_naics[, naics2 := substr(naics12, 1, 2)]


# create the shock 
# get the other country imports by year and product 
shock_naics <- shock_naics |> fgroup_by(importer, exporter, year, naics2, naics12) |> 
  fsummarize(imports = fsum(imports))
shock_naics <- shock_naics |> fgroup_by(importer, year, exporter, naics12) |> 
  fmutate(total_importers_naics = fsum(imports))
# divide by all immports in all sectors? No 
shock_naics <- shock_naics |> fgroup_by(importer, year, naics12) |> 
  fmutate(total_imports = fsum(imports))

shock_naics[,share := total_importers_naics / total_imports ]
setorder(shock_naics, importer, exporter, naics12, year)

shock_naics[, share_shift := share - shift(share),
            by = .(importer, exporter, naics12)]

shock_naics |> fgroup_by(importer, naics12, year) |> 
  fsummarize(check = fsum(share_shift))

full <- read_stata(paste0(path, "/112670-V1/Public-Release-Data/dta/workfile_china_preperiod.dta"))

