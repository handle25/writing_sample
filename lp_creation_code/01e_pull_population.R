################################################################################
# Get all county population 

################################################################################
# population weights -----------------------------------------------------------
#county populations 2020-2025 

acs25 <- read_excel(paste0(path, "/acs/PopulationEstimates.xlsx"), skip = 4) |> 
  clean_names() |> 
  data.table()

acs25 <- acs25 |> fselect(fip_stxt, pop_estimate_2020, 
                          pop_estimate_2021, pop_estimate_2022, 
                          pop_estimate_2023)

acs25 <- melt(
  acs25,
  id.vars = c("fip_stxt"),
  measure.vars = patterns("^pop_estimate_"),
  variable.name = "year",
  value.name = "population"
) |>
  fmutate(year = as.integer(substr(year, 14, 17)))
setnames(acs25, "fip_stxt", "area_fips") 

acs25 <- acs25[year != 2020]

#county popuations 2000-2020 
acs00 <- fread(
  paste0(path, "/acs/co-est00int-tot.csv")
)

acs20 <- fread(
  paste0(path, "/acs/co-est2020.csv")
)

# drop the april edition 
acs20[, POPESTIMATE042020 := NULL]

acs00 <- melt(
  acs00,
  id.vars = c("STATE", "COUNTY"),
  measure.vars = patterns("^POPESTIMATE"),
  variable.name = "year",
  value.name = "population"
) |>
  fmutate(year = as.integer(substr(year, 12, 15)))

acs20 <- melt(
  acs20,
  id.vars = c("STATE", "COUNTY"),
  measure.vars = patterns("^POPESTIMATE"),
  variable.name = "year",
  value.name = "population"
) |>
  fmutate(year = as.integer(substr(year, 12, 15)))


# Keep 2000-2009 from older vintage
acs00 <- acs00[year < 2010]

# Keep 2010 onward from newer vintage
acs <- rbind(
  acs00,
  acs20,
  fill = TRUE
)

# Drop rows created from years not covered by that source file
acs <- acs[!is.na(population)]

# Drop state totals
acs <- acs[COUNTY != 0]

acs[, area_fips := as.character(paste0(
  sprintf("%02d", STATE),
  sprintf("%03d", COUNTY)
))]

acs <- rbind(acs, acs25, fill = TRUE)

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
acs[,area_fips := as.character(as.integer(area_fips))]

acs <- acs[
  ,
  .(area_fips, year, population)
]

setorder(acs, area_fips, year)
acs[, .N, by = .(area_fips, year)][N > 1]

fwrite(acs, paste0(path, "/acs/population_1995_2023.csv"))
