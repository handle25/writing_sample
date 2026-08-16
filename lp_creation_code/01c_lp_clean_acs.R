################################################################################
# Created 8.16.2026 
# Author: Sophie Handley 
# Purpose: Aggregate ACS data 
################################################################################

rm(list = ls())

# qcewdata 
path <- "D:/writing_sample/data"
setwd(path)

################################################################################
# Population
################################################################################

acs <- rbind(
  fread(paste0(path, "/acs/co-est00int-tot.csv")), 
  fread(paste0(path, "/acs/co-est2020.csv")),
  fill = TRUE
)

acs <- melt(
  acs,
  id.vars = c("STATE", "COUNTY"),
  measure.vars = patterns("^POPESTIMATE"),
  variable.name = "year",
  value.name = "population"
) |>
  fmutate(year = as.integer(substr(year, 12, 15)))

# Drop rows created from years not covered by that source file
acs <- acs[!is.na(population)]

# Drop state totals
acs <- acs[COUNTY != 0]

# Create county FIPS
acs[, area_fips := as.integer(paste0(
  sprintf("%02d", STATE),
  sprintf("%03d", COUNTY)
))]
acs[,area_fips := as.character(as.integer(area_fips))]

# population -------------------------------------------------------------------

pop_list <- list()
for (y in c(1995:1999)){
  url <- paste0(
    "https://www2.census.gov/programs-surveys/popest/",
    "tables/1990-2000/intercensal/st-co/stch-icen", y, ".txt"
  )
  
  pop <- fread(url)
  
  names <- c("year","area_fips","age","sex","eth","population")
  
  names(pop) <- names
  pop <- pop |> 
    fgroup_by(area_fips) |> 
    fsummarize(population = fsum(population)) |> 
    fmutate(year = y) 
  
  pop_list[[length(pop_list) + 1]] <- pop
}

pop <- rbindlist(pop_list) 
acs <- rbind(acs, pop, fill = TRUE)
acs[, area_fips := as.integer(area_fips)]


fwrite(acs, paste0(path, "/output/lp_population.csv"))