# Reconstruct main-pipeline commuting outcomes from full main + auxiliary flows.
# Original source data and outputs are never overwritten. No downloads required.
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")
build_date <- "2026-10-29"
flows <- fread(file.path(path, "lodes/raw/full_lodes_data_with_aux_1990-2025.csv"))
stopifnot(!anyDuplicated(flows[, .(county, w_county, year)]))
flows <- flows[year %in% c(1990,2000,2002,2007,2013,2019)]
mapping <- c(goods = "SI01", trade = "SI02", servc = "SI03",
  age29 = "SA01", age30_54 = "SA02", age55 = "SA03",
  earn1250 = "SE01", earn1251_3333 = "SE02", earn3333 = "SE03")
lodes <- flows[, {
  inside <- county == w_county
  usable <- all(is.finite(S000) & S000 >= 0)
  ans <- list(total_jobs = if (usable) sum(as.double(S000)) else NA_real_,
    outside_jobs = if (usable) sum(as.double(S000[!inside])) else NA_real_)
  for (n in names(mapping)) {
    x <- as.double(get(mapping[[n]]))
    ok <- all(is.finite(x) & x >= 0)
    ans[[paste0("total_",n,"_jobs")]] <- if (ok) sum(x) else NA_real_
    ans[[paste0("outside_",n,"_jobs")]] <- if (ok) sum(x[!inside]) else NA_real_
  }
  ans
}, by = .(county,year)]
lodes[, outside_d_jobs := fifelse(total_jobs > 0, outside_jobs / total_jobs, NA_real_)]
lodes[, state_str := tolower(state.abb[match(as.integer(county) %/% 1000L,
  c(1,2,4,5,6,8,9,10,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
    30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56))])]
# Same known coverage exclusions as the county LP pipeline.
bad <- with(lodes,
  (state_str %chin% c("ar","nh") & year == 2002) |
  (state_str %chin% c("az","ms") & year %in% 2002:2003) |
  (state_str == "ma" & year %in% 2002:2010) |
  (state_str == "ct" & year %in% 2009:2010))
measure_cols <- setdiff(names(lodes), c("county","year","state_str"))
lodes[which(bad), (measure_cols) := NA_real_]
setorder(lodes,county,year)
stopifnot(!anyDuplicated(lodes[, .(county,year)]))
fwrite(lodes, file.path(path,"output",paste0("ld_lodes_with_aux_",build_date,".csv")))
rm(flows)

# Retain the existing main+aux network measures (built by LP 02d), not main-only networks.
network <- fread(file.path(path,
  "lodes/clean_lp_full/new_clean_lp_full/new_commuting_measures_lp_full_with_aux_1990-2025.csv"))
network_cols <- grep("^aux_(ex|fn)_",names(network),value=TRUE)
network <- network[, c("county","year",network_cols), with=FALSE]
stopifnot(!anyDuplicated(network[, .(county,year)]))
# Explicit non-overlapping windows. Boundary years belong to the later window.
network[, period := fifelse(year %in% 2000:2001,2000L,
  fifelse(year %in% 2002:2006,2007L,
  fifelse(year %in% 2007:2012,2013L,
  fifelse(year %in% 2013:2019,2019L,NA_integer_))))]
network <- network[!is.na(period), lapply(.SD, function(x)
  if (all(is.na(x))) NA_real_ else mean(x,na.rm=TRUE)),
  by=.(county,period), .SDcols=network_cols]
setnames(network,c("county","period"),c("area_fips","year"))
setnames(network,network_cols,sub("^aux_","",network_cols))
setnames(network,names(network),sub("mean_work_IPW","mean_dest_IPW",names(network)))
fwrite(network,file.path(path,"output",paste0("ld_lodes_network_with_aux_",build_date,".csv")))
message("Dated auxiliary commuting counts and network measures saved: ",build_date)
