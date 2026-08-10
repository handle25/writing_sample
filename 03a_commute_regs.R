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
qcew[,area_fips_str := sprintf("%06d", area_fips)]
qcew[,state := floor(area_fips/1000)]
lodes <- fread(paste0(path, "/output/lodes_collapsed_all.csv"))
lodes[year %in% c(2002, 2003, 2004), year := 2000]
# check regs 2007 --------------------------------------------------------------
reg <- merge(qcew, lodes, 
             by.x = c("area_fips", "year"), 
             by.y = c("h_county", "year"))

setorder(reg, area_fips, year)
reg[, d_outside_d_jobs :=
      outside_d_jobs - shift(outside_d_jobs, type = "lag", n = 1),
    by = .(area_fips)]

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

summary(mod, stage = 1)
summary(mod, stage = 2)

mod <- feols(
  d_sh_empl_mfg ~ t2 |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

summary(mod, stage = 1)
summary(mod, stage = 2)



mod <- feols(
  d_outside_d_jobs ~ t2 |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip
)

summary(mod, stage = 1)
summary(mod, stage = 2)
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

# check regs 2013 --------------------------------------------------------------
reg <- merge(qcew, lodes, 
             by.x = c("area_fips", "year"), 
             by.y = c("h_county", "year"))

setorder(reg, area_fips, year)
reg[, d_outside_d_jobs :=
      outside_d_jobs - shift(outside_d_jobs, type = "lag", n = 1),
    by = .(area_fips)]

reg <- reg[year %in% c(2007, 2013), ]# Period indicator
reg[, t2 := as.integer(year == 2013)]

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

exit 

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

ggplot(counties) +
  geom_sf(fill = "white", color = "grey70", linewidth = 0.1) +
  theme_void()


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



