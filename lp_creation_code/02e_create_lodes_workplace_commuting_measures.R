################################################################################
# Workplace-county commuting measures from full main + auxiliary OD flows.
# county = residence county in the input; w_county = workplace county.
################################################################################
rm(list = ls())
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")

flows <- fread(
  file.path(path, "lodes/raw/full_lodes_data_with_aux_1990-2025.csv"),
  select = c("county", "w_county", "year", "S000")
)
stopifnot(!anyDuplicated(flows[, .(county, w_county, year)]))
stopifnot(!anyNA(flows$county), !anyNA(flows$w_county),
          !anyNA(flows$year), !anyNA(flows$S000), all(flows$S000 >= 0))

# Group by WORKPLACE county: count jobs located there, regardless of residence.
# The existing source also includes Census 1990/2000 and the copied 2001 flows.
workplace <- flows[, .(
  full_employment_all = sum(as.numeric(S000)),
  full_inside_commuters = sum(as.numeric(S000[county == w_county])),
  full_outside_commuters = sum(as.numeric(S000[county != w_county]))
), by = .(w_county, year)]

workplace[, full_outside_commuters_share_full_employment_all := fifelse(
  full_employment_all > 0,
  full_outside_commuters / full_employment_all,
  NA_real_
)]

setorder(workplace, w_county, year)
stopifnot(!anyDuplicated(workplace[, .(w_county, year)]))
stopifnot(all(workplace$full_employment_all ==
              workplace$full_inside_commuters + workplace$full_outside_commuters))
stopifnot(sum(workplace$full_employment_all) == sum(as.numeric(flows$S000)))

# Separate output: existing residence-based commuting measures are unchanged.
fwrite(
  workplace,
  file.path(path, "output/new_workplace_commuting_measures_full_1990-2025.csv")
)
