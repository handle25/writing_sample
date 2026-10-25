################################################################################
# Combine saved main and auxiliary county-to-county flows. No downloads.
################################################################################
library(data.table)

raw_dir <- "D:/writing_sample/data/lodes/raw"

# The existing main file already includes Census 1990 and 2000.
main <- fread(file.path(raw_dir, "full_lodes_data_1990-2025.csv"))

aux_files <- list.files(
  raw_dir,
  pattern = "^aux_county_od_lodes_[a-z]{2}\\.csv$",
  full.names = TRUE
)
stopifnot(length(aux_files) > 0)
aux <- rbindlist(lapply(aux_files, fread), use.names = TRUE, fill = TRUE)
stopifnot(all(aux$year >= 2002))

flows <- rbindlist(list(main, aux), use.names = TRUE, fill = TRUE)

# Carry Census 2000 flows into 2001 once, as in the original aggregation.
if (!2001 %in% flows$year) {
  flows_2001 <- copy(flows[year == 2000])
  flows_2001[, year := 2001L]
  flows <- rbindlist(list(flows, flows_2001), use.names = TRUE, fill = TRUE)
}

setorder(flows, county, year, w_county)
stopifnot(!anyDuplicated(flows[, .(county, w_county, year)]))

fwrite(
  flows,
  file.path(raw_dir, "full_lodes_data_with_aux_1990-2025.csv")
)


# County-year totals for 03_lp_merge_data.R
lodes <- flows[, .(
  total_jobs = sum(S000, na.rm = TRUE),
  outside_jobs = sum(S000[county != w_county], na.rm = TRUE)
), by = .(county, year)]

lodes[, outside_d_jobs := outside_jobs / total_jobs]

fwrite(
  lodes,
  file.path(raw_dir, "lp_collapsed_full_lodes_data_with_aux_1990-2025.csv")
)
