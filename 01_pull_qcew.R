################################################################################
# Pull QCEW industry composition data 
# Contains information at the county level by employer location. 
# will get country employment levels by industry 
################################################################################


library(data.table)

path <- "D:/writing_sample/data/qcew"

years <- 2015:2025

for (y in years) {
  
  print(paste("Downloading", y))
  
  url <- paste0(
    "https://data.bls.gov/cew/data/files/",
    y,
    "/csv/",
    y,
    "_annual_singlefile.zip"
  )
  
  zip_file <- paste0(path, "/raw_", y, ".zip")
  unzip_dir <- paste0(path, "/temp_", y)
  
  # download annual national QCEW file
  download.file(
    url,
    destfile = zip_file,
    mode = "wb"
  )
  
  # unzip
  dir.create(unzip_dir, showWarnings = FALSE)
  unzip(zip_file, exdir = unzip_dir)
  
  # find CSV inside zip
  csv_file <- list.files(
    unzip_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )[1]
  
  # read full national annual file
  dt <- fread(csv_file)
  
  # save under new name so old files stay untouched
  fwrite(
    dt,
    paste0(path, "/clean/new_full_", y, ".csv")
  )
  
  # clean temporary files
  unlink(zip_file)
  unlink(unzip_dir, recursive = TRUE)
  
  rm(dt)
  gc()
  
  print(paste("Finished", y))
}