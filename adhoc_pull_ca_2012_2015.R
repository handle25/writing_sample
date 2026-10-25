################################################################################
# Retry California MAIN LODES for 2012 and 2015 only. No originals overwritten.
################################################################################
library(data.table)
library(lehdr)

raw_dir <- "D:/writing_sample/data/lodes/raw"
existing_file <- file.path(raw_dir, "county_od_lodes_ca.csv")
existing_ca <- fread(existing_file)  # Read existing California data ONCE.
stopifnot(!anyDuplicated(existing_ca[, .(county, w_county, year)]))

years <- c(2012L, 2015L)
retry_dir <- file.path(raw_dir, paste0("ca_retry_", format(Sys.time(), "%Y%m%d_%H%M%S")))
stopifnot(!dir.exists(retry_dir))
dir.create(retry_dir, recursive = TRUE)

pulled <- vector("list", length(years))
names(pulled) <- as.character(years)

for (y in years) {
  message("Pulling California main: ", y)
  pulled[[as.character(y)]] <- tryCatch({
    # New folder forces a fresh download; use_cache retains the downloaded gzip.
    dt <- as.data.table(grab_lodes(
      state = "ca", year = y, version = "LODES8", job_type = "JT00",
      lodes_type = "od", state_part = "main",
      download_dir = retry_dir, use_cache = TRUE
    ))
    stopifnot(nrow(dt) > 0)
    dt[, `:=`(
      county = as.integer(substr(as.character(h_geocode), 1, 5)),
      w_county = as.integer(substr(as.character(w_geocode), 1, 5)),
      year = y
    )]
    stopifnot(!anyNA(dt$county), !anyNA(dt$w_county))
    stopifnot(all(dt$county %/% 1000 == 6), all(dt$w_county %/% 1000 == 6))

    dt <- dt[, .(
      S000 = sum(S000, na.rm = TRUE),
      SI01 = sum(SI01, na.rm = TRUE),
      SI02 = sum(SI02, na.rm = TRUE),
      SI03 = sum(SI03, na.rm = TRUE),
      SA01 = sum(SA01, na.rm = TRUE),
      SA02 = sum(SA02, na.rm = TRUE),
      SA03 = sum(SA03, na.rm = TRUE),
      SE01 = sum(SE01, na.rm = TRUE),
      SE02 = sum(SE02, na.rm = TRUE),
      SE03 = sum(SE03, na.rm = TRUE)
    ), by = .(county, w_county, year)]
    dt[, state_str := "ca"]
    stopifnot(sum(dt$S000, na.rm = TRUE) > 0)

    # Save each successful retry independently, even if the other year fails.
    fwrite(dt, file.path(retry_dir, paste0("ca_main_", y, "_collapsed.csv")))
    dt
  }, error = function(e) {
    message("FAILED ", y, ": ", conditionMessage(e))
    NULL
  })
}

successful <- rbindlist(pulled, use.names = TRUE, fill = TRUE)
if (!all(years %in% successful$year)) {
  stop("A requested year failed. Successful pulls are saved in: ", retry_dir,
       ". No combined file was written and the original is unchanged.")
}

# In the NEW combined file, replace these years if they already existed.
# This avoids double-counting on a repeat run; other years are kept unchanged.
repaired_ca <- rbindlist(
  list(existing_ca[!year %in% years], successful),
  use.names = TRUE, fill = TRUE
)
setorder(repaired_ca, county, year, w_county)
stopifnot(!anyDuplicated(repaired_ca[, .(county, w_county, year)]))
stopifnot(all(unique(existing_ca$year) %in% repaired_ca$year))

output_file <- file.path(retry_dir, "county_od_lodes_ca.csv")
fwrite(repaired_ca, output_file)
print(repaired_ca[, .(rows = .N, jobs = sum(S000, na.rm = TRUE)), by = year][order(year)])
message("Saved repaired California file: ", output_file)
message("Original left unchanged: ", existing_file)

ca_check <- fread(
  "D:/writing_sample/data/lodes/raw/ca_retry_20261024_180032/county_od_lodes_ca.csv"
)

ca_check[, .(
  rows = .N,
  jobs = sum(S000, na.rm = TRUE)
), by = year][order(year)]

setdiff(2002:2023, unique(ca_check$year))

fwrite(
  ca_check,
  "D:/writing_sample/data/lodes/raw/county_od_lodes_ca.csv"
)