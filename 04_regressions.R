

reg <- fread(paste0(path, "/output/transformed_reg.csv"))
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

# Create employment quartiles -------------------------------------------------

reg[, quantile := cut(
  baseline_emp,
  breaks = quantile(baseline_emp, probs = 0:4/4, na.rm = TRUE),
  include.lowest = TRUE,
  labels = FALSE
)]
mod_outside <- feols(
  w_d_outside_servc_jobs_share_outside_jobs ~ t2 + l_shind_manuf |
    IPW_US ~ IPW_OTH,
  data = reg,
  weights = ~baseline_emp,
  cluster = ~statefip,
  split = ~quantile
)

mod_outside

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

