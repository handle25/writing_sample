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
# check regs 2007 --------------------------------------------------------------
reg <- merge(qcew, lodes, 
             by.x = c("area_fips", "year"), 
             by.y = c("county", "year"))
setorder(reg, area_fips, year)
outside_jobs <- names(reg)[grep("^outside", names(reg))]

outside_jobs <- grep("^outside.*_jobs$", names(reg), value = TRUE)

for (v in outside_jobs) {
  
  # create share
  share_var <- paste0(v, "_share_total_jobs")
  
  reg[, (share_var) := get(v) / total_jobs]
  
  d_var <- paste0("d_", v)
  
  reg[, (d_var) := get(v) - shift(get(v), type = "lag", n = 1), by =.(area_fips)]
  
  # create change in share
  d_share_var <- paste0("d_", share_var)
  
  reg[, (d_share_var) :=
        get(share_var) - shift(get(share_var), type = "lag"),
      by = area_fips]
}

total_jobs <- grep("^total.*_jobs$", names(reg), value = TRUE)

for (v in total_jobs) {
  
  # create share
  share_var <- paste0(v, "_share_total_jobs")
  
  reg[, (share_var) := get(v) / total_jobs]
  
  # create change in share
  d_share_var <- paste0("d_", share_var)
  
  reg[, (d_share_var) :=
        get(share_var) - shift(get(share_var), type = "lag"),
      by = area_fips]
  
  d_var <- paste0("d_", v)
  
  reg[, (d_var) :=
        get(v) - shift(get(v), type = "lag"),
      by = area_fips]
}

reg <- reg[year %in% c(2000, 2007), ]# Period indicator
reg[, t2 := as.integer(year == 2007)]

# State FIPS for clustering
reg[, statefip := floor(as.integer(area_fips) / 1000)]

mod <- feols(
  d_sh_empl_mfg ~ t2 | IPW_US ~ IPW_OTH,
  data = reg,
  cluster = ~statefip
)
mod <- feols(
  d_sh_empl_mfg ~ as.factor(year) + l_shind_manuf |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)
print("MANUFACTURING SHARE")
summary(mod, stage = 2)

mod <- feols(
  d_sh_empl_mfg ~ t2 |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

summary(mod, stage = 2)
print("OUTSIDE EMPLOYMENT")
mod <- feols(
  d_outside_jobs_share_total_jobs ~ t2  |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)
summary(mod, stage = 2)
print("OUTSIDE EMPLOYMENT")
mod <- feols(
  d_outside_jobs_share_total_jobs ~ t2  |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)
summary(mod, stage = 1)
summary(mod, stage = 2)


print("OUTSIDE EMPLOYMENT")
mod <- feols(
  total_servc_jobs_share_total_jobs
  ~ t2  |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)
summary(mod, stage = 1)
summary(mod, stage = 2)

print("OUTSIDE EMPLOYMENT")
mod <- feols(
  d_total_goods_jobs
  ~ t2  |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)
summary(mod, stage = 1)
summary(mod, stage = 2)

reg[, inside_servc_jobs :=
      total_servc_jobs - outside_servc_jobs]

setorder(reg, area_fips, year)

reg[, d_inside_servc_jobs :=
      inside_servc_jobs - shift(inside_servc_jobs),
    by = area_fips]
mod <- feols(
  d_outside_servc_jobs
  ~ t2  |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)
summary(mod, stage = 1)
summary(mod, stage = 2)

exit 
# check regs 2013 --------------------------------------------------------------
reg <- merge(qcew, lodes, 
             by.x = c("area_fips", "year"), 
             by.y = c("county", "year"))

setorder(reg, area_fips, year)
reg[, d_outside_d_jobs :=
      outside_d_jobs - shift(outside_d_jobs, type = "lag", n = 1),
    by = .(area_fips)]

reg <- reg[year %in% c(2007, 2013), ]# Period indicator
reg[, t2 := as.integer(year == 2013)]

# State FIPS for clustering
reg[, statefip := floor(as.integer(area_fips) / 1000)]


mod <- feols(
  d_sh_empl_mfg ~ as.factor(year) + l_shind_manuf |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

summary(mod, stage = 1)
summary(mod, stage = 2)

mod <- feols(
  d_sh_empl_mfg ~ t2 |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)
summary(mod, stage = 2)

mod <- feols(
  d_outside_d_jobs ~ t2 |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

summary(mod, stage = 2)
mod <- feols(
  d_sh_empl_mfg ~ as.factor(year) + l_shind_manuf |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

summary(mod, stage = 2)

mod <- feols(
  d_sh_empl_mfg ~ t2 |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

summary(mod, stage = 2)

mod <- feols(
  d_outside_d_jobs ~ t2 |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

# summary(mod, stage = 1)
summary(mod, stage = 2)

counties <- counties(cb = TRUE, year = 2020)
counties$area_fips <- as.integer(counties$GEOID)

map_dt <- merge(
  counties,
  reg,
  by = "area_fips", 
  all.x = TRUE
)

ggplot(map_dt) +
  geom_sf(aes(fill = outside_d_jobs), color = "grey70", linewidth = 0.1) +
  coord_sf(
    xlim = c(-125, -66),
    ylim = c(24, 50)
  ) +
  theme_void()
# 
# ggplot(map_dt) +
#   geom_sf(aes(fill = d_outside_d_jobs), color = "grey70", linewidth = 0.1) +
#   coord_sf(
#     xlim = c(-80, -90),
#     ylim = c(34, 50)
#   ) +
#   theme_void()


q975 <- quantile(reg$d_outside_d_jobs, .975, na.rm = TRUE)
q025 <- quantile(reg$d_outside_d_jobs, .025, na.rm = TRUE)
reg[d_outside_d_jobs >= q975, d_outside_d_jobs := q975]
reg[d_outside_d_jobs <= q025, d_outside_d_jobs := q025]

q975 <- quantile(reg$IPW_US, .975, na.rm = TRUE)
q975 <- quantile(reg$IPW_US, .025, na.rm = TRUE)
reg[IPW_US >= q975, IPW_US := q975]
reg[IPW_US <= q025, IPW_US := q025]

q975 <- quantile(reg$IPW_OTH, .975, na.rm = TRUE)
q025 <- quantile(reg$IPW_OTH, .025, na.rm = TRUE)
reg[IPW_OTH >= q975, IPW_OTH := q975]
reg[IPW_OTH <= q025, IPW_OTH := q025]

mod <- feols(
  d_outside_d_jobs ~ t2 |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

# summary(mod, stage = 1)
summary(mod, stage = 2)


counties <- counties(cb = TRUE, year = 2020)

counties$area_fips <- as.integer(counties$GEOID)

map_dt <- merge(
  counties,
  reg,
  by = "area_fips", 
  all.x = TRUE
)

ggplot(map_dt) +
  geom_sf(aes(fill = outside_d_jobs), color = "grey70", linewidth = 0.1) +
  coord_sf(
    xlim = c(-125, -66),
    ylim = c(24, 50)
  ) +
  theme_void()

# ggplot(map_dt) +
#   geom_sf(aes(fill = d_outside_d_jobs), color = "grey70", linewidth = 0.1) +
#   coord_sf(
#     xlim = c(-80, -90),
#     ylim = c(34, 50)
#   ) +
#   theme_void()



