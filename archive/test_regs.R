################################################################################
# Created 8.8.2026
#
# Author: Sophie Handley
#
# Purpose: Collapse LODES to county x year, combine states, then merge to QCEW
################################################################################

rm(list = ls())

path <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
setwd(path)

qcew <- fread(paste0(path, "/output/weighted_qcew.csv"))
qcew[, area_fips_str := sprintf("%06d", area_fips)]
qcew[, state := floor(area_fips / 1000)]

states <- c("mi", "oh", "in", "ak", "ar", "al", "va")

lodes_list <- list()

for (s in states) {
  
  lodes <- fread(
    paste0(
      path,
      "/lodes/full_test/lodes_subset_",
      s,
      "_test.csv"
    )
  )
  
  lodes[year %in% c(2002, 2003, 2004), year := 2000]
  
  # county identifiers
  lodes[, county := floor(h_geocode / 10000000000)]
  lodes[, w_county := floor(w_geocode / 10000000000)]
  
  # outside-county indicator
  lodes[, outside := fifelse(county == w_county, 0, 1)]
  
  # outside-county jobs
  lodes[, outside_jobs := S000 * outside]
  lodes[, outside_goods_jobs := SI01 * outside]
  lodes[, outside_trade_jobs := SI02 * outside]
  lodes[, outside_servc_jobs := SI03 * outside]
  
  # collapse state file to county x year
  lodes <- lodes |>
    fgroup_by(county, year) |>
    fsummarize(
      total_jobs = fsum(S000),
      total_goods_jobs = fsum(SI01),
      total_trade_jobs = fsum(SI02),
      total_servc_jobs = fsum(SI03),
      outside_jobs = fsum(outside_jobs),
      outside_goods_jobs = fsum(outside_goods_jobs),
      outside_trade_jobs = fsum(outside_trade_jobs),
      outside_servc_jobs = fsum(outside_servc_jobs)
    ) |>
    data.table()
  
  # save collapsed state piece to list
  lodes_list[[s]] <- lodes
  
  rm(lodes)
  gc()
}

# combine collapsed state datasets
lodes <- rbindlist(lodes_list, fill = TRUE)

# sanity check: should be unique county x year
lodes[, .N, by = .(county, year)][N > 1]

# merge once to full QCEW
reg <- merge(
  qcew,
  lodes,
  by.x = c("area_fips", "year"),
  by.y = c("county", "year")
)

# check regs 2007 --------------------------------------------------------------
reg <- merge(qcew, lodes, 
             by.x = c("area_fips", "year"), 
             by.y = c("county", "year"))

# shares working outside county ------------------------------------------------

reg[, outside_d_jobs :=
      outside_jobs / total_jobs]

reg[, outside_d_goods_jobs :=
      outside_goods_jobs / total_goods_jobs]

reg[, outside_d_trade_jobs :=
      outside_trade_jobs / total_trade_jobs]

reg[, outside_d_servc_jobs :=
      outside_servc_jobs / total_servc_jobs]


# order before taking changes -------------------------------------------------

setorder(reg, area_fips, year)


# changes in total jobs -------------------------------------------------------

reg[, d_total_jobs :=
      total_jobs - shift(total_jobs, type = "lag", n = 1),
    by = .(area_fips)]

reg[, d_total_goods_jobs :=
      total_goods_jobs - shift(total_goods_jobs, type = "lag", n = 1),
    by = .(area_fips)]

reg[, d_total_trade_jobs :=
      total_trade_jobs - shift(total_trade_jobs, type = "lag", n = 1),
    by = .(area_fips)]

reg[, d_total_servc_jobs :=
      total_servc_jobs - shift(total_servc_jobs, type = "lag", n = 1),
    by = .(area_fips)]


# changes in number working outside county -----------------------------------

reg[, d_outside_jobs :=
      outside_jobs - shift(outside_jobs, type = "lag", n = 1),
    by = .(area_fips)]

reg[, d_outside_goods_jobs :=
      outside_goods_jobs - shift(outside_goods_jobs, type = "lag", n = 1),
    by = .(area_fips)]

reg[, d_outside_trade_jobs :=
      outside_trade_jobs - shift(outside_trade_jobs, type = "lag", n = 1),
    by = .(area_fips)]

reg[, d_outside_servc_jobs :=
      outside_servc_jobs - shift(outside_servc_jobs, type = "lag", n = 1),
    by = .(area_fips)]


# changes in share working outside county ------------------------------------

reg[, d_outside_d_jobs :=
      outside_d_jobs - shift(outside_d_jobs, type = "lag", n = 1),
    by = .(area_fips)]

reg[, d_outside_d_goods_jobs :=
      outside_d_goods_jobs - shift(outside_d_goods_jobs, type = "lag", n = 1),
    by = .(area_fips)]

reg[, d_outside_d_trade_jobs :=
      outside_d_trade_jobs - shift(outside_d_trade_jobs, type = "lag", n = 1),
    by = .(area_fips)]

reg[, d_outside_d_servc_jobs :=
      outside_d_servc_jobs - shift(outside_d_servc_jobs, type = "lag", n = 1),
    by = .(area_fips)]



reg <- reg[year %in% c(2007, 2013), ]# Period indicator
reg[, t2 := as.integer(year == 2013)]

# State FIPS for clustering
reg[, statefip := floor(as.integer(area_fips) / 1000)]

mod <- feols(
  d_sh_empl_mfg ~ t2 |
    IPW_US ~ IPW_OTH,
  data = reg
)

summary(mod, stage = 2)

mod <- feols(
  d_outside_d_jobs ~ t2 |
    IPW_US ~ IPW_OTH,
  data = reg, 
  vcov = ~statefip
)

summary(mod, stage = 2)
mod <- feols(
  d_outside_d_jobs ~ t2 |
    IPW_US ~ IPW_OTH,
  weights = ~baseline_emp,
  data = reg, 
  vcov = ~statefip
)

summary(mod, stage = 2)


counties <- counties(cb = TRUE, year = 2020)
counties$area_fips <- as.integer(counties$GEOID)

map_dt <- merge(
  counties,
  reg,
  by = "area_fips", 
  all.x = TRUE
)

# ggplot(map_dt) +
#   geom_sf(aes(fill = d_outside_d_jobs), color = "grey70", linewidth = 0.1) +
#   coord_sf(
#     xlim = c(-125, -66),
#     ylim = c(24, 50)
#   ) +
#   theme_void()

ggplot(map_dt) +
  geom_sf(aes(fill = d_outside_d_jobs), color = "grey70", linewidth = 0.1) +
  coord_sf(
    xlim = c(-80, -90),
    ylim = c(40, 50)
  ) +
  theme_void()




# create quartiles of county size
reg[, jobs_q := cut(
  total_jobs,
  breaks = quantile(total_jobs,
                    probs = seq(0, 1, .33),
                    na.rm = TRUE),
  include.lowest = TRUE
  #, labels = c("Q1_smallest", "Q2", "Q3", "Q4_largest")
)]

table(reg$jobs_q)

mods <- lapply(levels(reg$jobs_q), function(q) {
  
  feols(
    d_outside_d_jobs ~ t2 |
      IPW_US ~ IPW_OTH,
    data = reg[jobs_q == q], 
    vcov = ~statefip
  )
})

names(mods) <- levels(reg$jobs_q)

lapply(mods, summary, stage = 2)


exit 

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
  weights = ~baseline_emp
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



