################################################################################
# Created 8.8.2026 
# Author: Sophie Handley 
# Purpose: Weight dollar shifts in naics x year imports by labor shares with QCEW

################################################################################

rm(list = ls())

# qcewdata 
path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
setwd(path)


qcew <- fread(paste0(path, "/output/weighted_qcew.csv")) 
qcew[, .(
  N = .N,
  mean_US  = mean(IPW_US, na.rm = TRUE),
  median_US = median(IPW_US, na.rm = TRUE),
  min_US   = min(IPW_US, na.rm = TRUE),
  max_US   = max(IPW_US, na.rm = TRUE),
  mean_OTH = mean(IPW_OTH, na.rm = TRUE)
), by = year]

qcew[,area_fips_str := sprintf("%06d", area_fips)]
qcew[,state := floor(area_fips/1000)]
lodes <- fread(paste0(path, "/output/lodes_collapsed_all_no_crosswalk.csv"))
lodes[year %in% c(2002, 2003, 2004), year := 2000]
# check regs 2013 --------------------------------------------------------------

reg <- merge(qcew, lodes, 
             by.x = c("area_fips", "year"), 
             by.y = c("county", "year"))
setorder(reg, area_fips, year)
outside_jobs <- names(reg)[grep("^outside", names(reg))]

setorder(reg, area_fips, year)
setorder(reg, area_fips, year)

# ---------------------------------------------------------------------------
# Variables to difference in levels
# ---------------------------------------------------------------------------

level_vars <- c(
  grep("^outside.*_jobs$", names(reg), value = TRUE),
  grep("^total.*_jobs$", names(reg), value = TRUE),
  grep("^inside.*_jobs$", names(reg), value = TRUE)
)

level_vars <- unique(level_vars)

for (i in level_vars) {
  
  # denominator = total jobs ---------------------------------------------------
  
  var_d_all <- paste0(i, "_share_total_jobs")
  d_var_d_all <- paste0("d_", i, "_share_total_jobs")
  
  reg[, (var_d_all) := get(i) / total_jobs]
  
  reg[, (d_var_d_all) :=
        get(var_d_all) - shift(get(var_d_all), type = "lag", n = 1),
      by = .(area_fips)]
  
  w_var_d_all <- paste0("w_", d_var_d_all)
  
  q99 <- quantile(reg[[d_var_d_all]], .99, na.rm = TRUE)
  q01 <- quantile(reg[[d_var_d_all]], .01, na.rm = TRUE)
  
  reg[, (w_var_d_all) := get(d_var_d_all)]
  reg[get(w_var_d_all) >= q99, (w_var_d_all) := q99]
  reg[get(w_var_d_all) <= q01, (w_var_d_all) := q01]
  
  
  # denominator = outside jobs -------------------------------------------------
  
  var_d_out <- paste0(i, "_share_outside_jobs")
  d_var_d_out <- paste0("d_", i, "_share_outside_jobs")
  
  reg[, (var_d_out) := get(i) / outside_jobs]
  
  reg[, (d_var_d_out) :=
        get(var_d_out) - shift(get(var_d_out), type = "lag", n = 1),
      by = .(area_fips)]
  
  w_var_d_out <- paste0("w_", d_var_d_out)
  
  q99 <- quantile(reg[[d_var_d_out]], .99, na.rm = TRUE)
  q01 <- quantile(reg[[d_var_d_out]], .01, na.rm = TRUE)
  
  reg[, (w_var_d_out) := get(d_var_d_out)]
  reg[get(w_var_d_out) >= q99, (w_var_d_out) := q99]
  reg[get(w_var_d_out) <= q01, (w_var_d_out) := q01]
  
  
  # denominator = service jobs -------------------------------------------------
  
  var_d_svc <- paste0(i, "_share_service_jobs")
  d_var_d_svc <- paste0("d_", i, "_share_service_jobs")
  
  reg[, (var_d_svc) := get(i) / total_servc_jobs]
  
  reg[, (d_var_d_svc) :=
        get(var_d_svc) - shift(get(var_d_svc), type = "lag", n = 1),
      by = .(area_fips)]
  
  w_var_d_svc <- paste0("w_", d_var_d_svc)
  
  q99 <- quantile(reg[[d_var_d_svc]], .99, na.rm = TRUE)
  q01 <- quantile(reg[[d_var_d_svc]], .01, na.rm = TRUE)
  
  reg[, (w_var_d_svc) := get(d_var_d_svc)]
  reg[get(w_var_d_svc) >= q99, (w_var_d_svc) := q99]
  reg[get(w_var_d_svc) <= q01, (w_var_d_svc) := q01]
  
  # denominator = goods jobs ---------------------------------------------------
  
  var_d_good <- paste0(i, "_share_goods_jobs")
  d_var_d_good <- paste0("d_", i, "_share_goods_jobs")
  
  reg[, (var_d_good) := get(i) / total_goods_jobs]
  
  reg[, (d_var_d_good) :=
        get(var_d_good) - shift(get(var_d_good), type = "lag", n = 1),
      by = .(area_fips)]
  
  w_var_d_good <- paste0("w_", d_var_d_good)
  
  q99 <- quantile(reg[[d_var_d_good]], .99, na.rm = TRUE)
  q01 <- quantile(reg[[d_var_d_good]], .01, na.rm = TRUE)
  
  reg[, (w_var_d_good) := get(d_var_d_good)]
  reg[get(w_var_d_good) >= q99, (w_var_d_good) := q99]
  reg[get(w_var_d_good) <= q01, (w_var_d_good) := q01]
}

reg <- reg[year %in% c(2007, 2013), ]# Period indicator
reg[, t2 := as.integer(year == 2013)]

# State FIPS for clustering
reg[, statefip := floor(as.integer(area_fips) / 1000)]

# dependent variables ----------------------------------------------------------

dep_vars <- c(
  # Services
  "w_d_total_servc_jobs_share_total_jobs",
  "w_d_outside_servc_jobs_share_outside_jobs",
  "w_d_outside_servc_jobs_share_service_jobs",
  
  # Goods
  "w_d_total_goods_jobs_share_total_jobs",
  "w_d_outside_goods_jobs_share_outside_jobs",
  "w_d_outside_goods_jobs_share_goods_jobs"
)

# variable labels --------------------------------------------------------------

names_dict <- c(
  "fit_IPW_US" = "Import Exposure",
  "t2" = "Period 2",
  "l_shind_manuf" = "Initial Manufacturing Share",
  
  # Services
  "w_d_total_servc_jobs_share_total_jobs" =
    "$\\Delta \\frac{Jobs_{service}}{Jobs_{total}}$",
  
  "w_d_outside_servc_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Jobs_{outside, service}}{Jobs_{outside}}$",
  
  "w_d_outside_servc_jobs_share_service_jobs" =
    "$\\Delta \\frac{Jobs_{outside, service}}{Jobs_{service}}$",
  
  # Goods
  "w_d_total_goods_jobs_share_total_jobs" =
    "$\\Delta \\frac{Jobs_{goods}}{Jobs_{total}}$",
  
  "w_d_outside_goods_jobs_share_outside_jobs" =
    "$\\Delta \\frac{Jobs_{outside, goods}}{Jobs_{outside}}$",
  
  "w_d_outside_goods_jobs_share_goods_jobs" =
    "$\\Delta \\frac{Jobs_{outside, goods}}{Jobs_{goods}}$"
)


# baseline models --------------------------------------------------------------

mods <- lapply(dep_vars, function(y) {
  
  fml <- as.formula(
    paste0(
      y,
      " ~ t2 + l_shind_manuf | ",
      "IPW_US ~ IPW_OTH"
    )
  )
  
  feols(
    fml,
    data = reg,
    weights = ~baseline_emp,
    cluster = ~statefip
  )
})

names(mods) <- dep_vars


etable(
  mods,
  dict = names_dict,
  drop = "Constant",
  headers = list(
    "^:_:Sector" = list(
      "Services" = 3,
      "Goods" = 3
    )
  ),
  tex = TRUE,
  file = paste0(path, "/../figures/models_sector.tex"),
  replace = TRUE
)


# interaction models -----------------------------------------------------------

mods <- lapply(dep_vars, function(y) {
  
  fml <- as.formula(
    paste0(
      y,
      " ~ t2 + l_shind_manuf + l_shind_manuf*t2 | ",
      "IPW_US ~ IPW_OTH"
    )
  )
  
  feols(
    fml,
    data = reg,
    weights = ~baseline_emp,
    cluster = ~statefip
  )
})

names(mods) <- dep_vars


etable(
  mods,
  dict = names_dict,
  drop = "Constant",
  headers = list(
    "^:_:Sector" = list(
      "Services" = 3,
      "Goods" = 3
    )
  ),
  tex = TRUE,
  file = paste0(path, "/../figures/models_sector_interaction.tex"),
  replace = TRUE
)

# counties <- counties(cb = TRUE, year = 2020)
# 
# counties$area_fips <- as.integer(counties$GEOID)
# 
# map_dt <- merge(
#   counties,
#   reg,
#   by = "area_fips", 
#   all.x = TRUE
# )
# 
# ggplot(map_dt) +
#   geom_sf(aes(fill = outside_d_jobs), color = "grey70", linewidth = 0.1) +
#   coord_sf(
#     xlim = c(-125, -66),
#     ylim = c(24, 50)
#   ) +
#   theme_void()

# ggplot(map_dt) +
#   geom_sf(aes(fill = d_outside_d_jobs), color = "grey70", linewidth = 0.1) +
#   coord_sf(
#     xlim = c(-80, -90),
#     ylim = c(34, 50)
#   ) +
#   theme_void()



