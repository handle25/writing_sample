################################################################################
# Created 8.15.2026 
# Author: Sophie Handley 
# Purpose: Run LP regressions
################################################################################


rm(list = ls())
figure_1 <- T
figure_2 <- T
figure_3 <- T
figure_4 <- T
date <- Sys.Date()

# qcewdata 
# read in data -----------------------------------------------------------------
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")
run_date <- date
reg <- fread(paste0(path, "/output/lp_transformed_reg_", run_date, ".csv"))
# reg <- reg[year != 2009]
reg <- reg[year<2020]
reg <- reg[year>1994]

setorder(reg, area_fips, year)
reg[, baseline_resident_emp := resident_emp[year == 2000][1], by=area_fips]

reg[, l_sh_empl_mfg := shift(sh_empl_mfg), by = area_fips]
reg[, total_migration := returns_3_inflow + returns_3_outflow]
reg[, net_migration := returns_3_inflow - returns_3_outflow]
reg[, net_migration_share_population := net_migration / total_migration]
reg[, l2_ex_mean_work_IPW_US := shift(ex_mean_work_IPW_US, n = 2), by = area_fips]
reg[, l1_w_ex_mean_work_IPW_US := shift(w_ex_mean_work_IPW_US, n = 1), by = area_fips]


shocks <- grep("IPW", names(reg), value=TRUE)
# for (v in shocks) reg[, (paste0("l1_", v)) := shift(get(v), 1), by=area_fips]

shocks <- grep("empw|mean", names(reg), value=TRUE)
for (v in shocks) winsor(reg, v, p = .01)
reg[, unemployed := as.double(unemployed)]

reg[, state_fips := substr(sprintf("%05d", as.integer(as.character(area_fips))), 1, 2)]

coastal_fips <- c("01", "02", "06", "09", "10", "12", "13", "15",
                  "22", "23", "24", "25", "28", "33", "34", "36",
                  "37", "41", "44", "45", "48", "51", "53")

reg[, coastal_group := fifelse(
  is.na(state_fips), NA_character_,
  fifelse(state_fips %in% coastal_fips,
          "A. Coastal states", "B. Other states")
)]

open_var <- "outside_jobs_share_resident_emp"

base_open <- reg[
  year == 2000 & is.finite(get(open_var)),
  .(openness_2000 = mean(get(open_var))),
  by = area_fips
]

median_open <- median(base_open$openness_2000)

base_open[, openness_group := fifelse(
  openness_2000 > median_open,
  "High openness", "Low openness"
)]

reg[, c("openness_2000", "openness_group") := NULL]

reg[base_open, on = "area_fips", `:=`(
  openness_2000 = i.openness_2000,
  openness_group = i.openness_group
)]


setorder(reg, area_fips, year)

# Fill missing 2001 commuting values using each county's 2000 value.
reg[reg[year == 2000, .(area_fips, value_2000 = com_outside_jobs)],
    on = .(area_fips),
    com_outside_jobs := fifelse(
      year == 2001 & is.na(com_outside_jobs),
      i.value_2000,
      com_outside_jobs
    )]

reg[reg[year == 2000, .(area_fips, value_2000 = aux_outside_jobs)],
    on = .(area_fips),
    aux_outside_jobs := fifelse(
      year == 2001 & is.na(aux_outside_jobs),
      i.value_2000,
      aux_com_outside_jobs
    )]

commuting_two_shock <- run_lp_multiple_shocks(
  reg,
  outcome = "com_outside_jobs",
  denominator = "labor_force",
  shock_1 = "w_IPW_US",
  shock_2 = "w_mean_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_mean_neighbor_IPW_OTH",
  controls = "+l1_own_shock_1+population",
  start_year = 2000,
  end_year = 2015, 
  horizons = c(0:6)
)

commuting_two_shock <- run_lp_multiple_shocks(
  reg,
  outcome = "aux_outside_jobs",
  denominator = "aux_resident_emp",
  shock_1 = "w_IPW_US",
  shock_2 = "w_mean_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_mean_neighbor_IPW_OTH",
  controls = "+l1_own_shock_1+population",
  start_year = 2000,
  end_year = 2015,
  horizons = c(0:6)
)

commuting_two_shock <- run_lp_multiple_shocks(
  reg,
  outcome = "aux_com_outside_jobs",
  denominator = "labor_force",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock_1",
  start_year = 2000,
  end_year = 2015,
  horizons = 0:7,
  table_horizons = 0:7,
  height = 4,
  width = 7
)


commuting_two_shock <- run_lp_multiple_shocks(
  reg,
  outcome = "rw_share_into_less_exposed",
  denominator = "resident_emp",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "+l1_own_shock_1+population",
  start_year = 2000,
  end_year = 2015,
  horizons = 0:7,
  table_horizons = 0:7,
  height = 4,
  width = 7
)

reg[,test :=aux_com_outside_jobs/labor_force]
winsor(reg, "test")
run_lp_ratio(reg, "w_test", start_year=2003, end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")

lf_multi <- run_lp_ratio_multiple_shocks(
  reg,
  outcome="w_test",
  shock_1="w_IPW_US",
  shock_2="w_mean_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_mean_neighbor_IPW_OTH",
  start_year=2003,
  end_year=2015
)


# run regressions --------------------------------------------------------------
# run_lp_lagged_denom(reg, outcome="outside_goods_jobs_share_outside_jobs", 
#                     denominator="resident_emp",
#                     controls="+l1_own_shock",
#                     start_year = 1998, 
#                     end_year = 2008)

run_lp_ratio(reg, "ew_share_into_less_exposed", start_year=1998, end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")


reg[, commute_under30 := rowSums(.SD), .SDcols = paste0("commute_", c("lt5", "5_9", "10_14", "15_19", "20_24", "25_29"), "E")]
reg[, commute_30_59 := rowSums(.SD), .SDcols = paste0("commute_", c("30_34", "35_39", "40_44", "45_59"), "E")]
reg[, commute_60plus_new := rowSums(.SD), .SDcols = paste0("commute_", c("60_89", "90plus"), "E")]
reg[, commute_30plus := commute_30_59 + commute_60plus_new]
reg[, tot_commuters := commute_under30 + commute_30_59 + commute_60plus_new]
reg[, agg_per_pers := commute_under30 / tot_commuters]
winsor(reg, "agg_per_pers")
run_lp_ratio(reg, "w_agg_per_pers", start_year=2005, end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")


lf_multi <- run_lp_ratio_multiple_shocks(
  reg,
  outcome="w_agg_per_pers",
  shock_1="w_IPW_US",
  shock_2="w_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_empw_neighbor_IPW_OTH",
  start_year=2007,
  end_year=2015
)

lf_multi <- run_lp_ratio_multiple_shocks(
  reg,
  outcome="rw_share_into_higher_agi",
  shock_1="w_IPW_US",
  shock_2="w_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_empw_neighbor_IPW_OTH",
  start_year=1995,
  end_year=2015
)
reg18 <- reg[year<2018]
lf_hetero <- run_lp_ratio_multiple_shocks_heterogeneity(
  reg18,
  outcome="ew_share_into_higher_agi",
  group_var="openness_group",
  shock_1="w_IPW_US",
  shock_2="w_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_empw_neighbor_IPW_OTH",
  start_year=1995,
  end_year=2015,
  horizons=0:7,
  controls="",
  figure=TRUE,
  unit="area_fips"
)


run_lp_lagged_denom(reg, outcome="exemptions_3_inflow", 
                    denominator="exemptions",
                    controls="+l1_own_shock+w_ex_mean_work_IPW_US",
                    start_year = 1997, 
                    end_year = 2015)

run_lp_ratio(reg, "sh_empl_mfg", start_year=1998, end_year=2008,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")

winsor(reg, "com_outside_share_all")
run_lp_ratio(reg, "com_outside_share_all", start_year=1998, end_year=2008,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")


run_lp_ratio(reg, "w_unemployed_share_labor_force", start_year=1998, end_year=2008,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")

winsor(reg, "sh_empl_mfg", p = .01)

lf_multi <- run_lp_ratio_multiple_shocks(
  reg,
  outcome="w_sh_empl_mfg",
  shock_1="w_IPW_US",
  shock_2="w_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_empw_neighbor_IPW_OTH",
  start_year=1995,
  end_year=2015
)


## unemployment ----------------------------------------------------------------
run_lp_lagged_denom(reg, outcome="unemployed", 
                    denominator="labor_force",
                    controls="+l1_own_shock",
                    start_year = 1995, 
                    end_year = 2015)

run_lp_ratio(reg, "w_unemployed_share_labor_force", start_year=1998, end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")




lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="unemployed",
  denominator="labor_force", # or resident_emp
  shock_1="w_IPW_US",
  shock_2="w_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_empw_neighbor_IPW_OTH",
  start_year=1995,
  end_year=2015
)

lf_multi <- run_lp_ratio_multiple_shocks(
  reg,
  outcome="unemployed_share_labor_force",
  shock_1="w_IPW_US",
  shock_2="w_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_empw_neighbor_IPW_OTH",
  start_year=1995,
  end_year=2015
)

## Labor force -----------------------------------------------------------------
run_lp_ratio(reg, "labor_force_share_population", start_year=1998, end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")

lf_multi <- run_lp_ratio_multiple_shocks(
  reg,
  outcome="labor_force_share_population",
  shock_1="w_IPW_US",
  shock_2="w_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_empw_neighbor_IPW_OTH",
  start_year=1995,
  end_year=2015
)

# run_lp_lagged_denom(reg, outcome="labor_force", 
#                     denominator="population",
#                     controls="+l1_own_shock+w_ex_mean_work_IPW_US",
#                     start_year = 1998, 
#                     end_year = 2015)
# 
# lf_multi <- run_lp_multiple_shocks(
#   reg,
#   outcome="labor_force",
#   denominator="population", # or resident_emp
#   shock_1="w_IPW_US",
#   shock_2="w_empw_neighbor_IPW_US",
#   instrument_1="w_IPW_OTH",
#   instrument_2="w_empw_neighbor_IPW_OTH",
#   controls="+l1_own_shock",
#   start_year=2000,
#   end_year=2015
# )

## Commuting -------------------------------------------------------------------
reg[, outside_jobs_share_l1_labor_force := outside_jobs / shift(labor_force) , by = area_fips]
winsor(reg, "outside_jobs_share_l1_labor_force")
run_lp_ratio(reg, "w_outside_jobs_share_l1_labor_force", 
             start_year=2000, 
             end_year=2010,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")

winsor(reg, "mean_neighbor_IPW_US")
winsor(reg, "mean_neighbor_IPW_OTH")
reg[, l1_labor_force := shift(labor_force), by = area_fips]

################################################################################
lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="com_outside_jobs",
  denominator="resident_emp", # or resident_emp
  shock_1="w_IPW_US",
  shock_2="w_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_empw_neighbor_IPW_OTH",
  controls="+l1_own_shock_1+population",
  start_year=2000,
  end_year=2015, 
  height = 4, 
  width = 7 , 
  horizons = 0:6,
  table_horizons = c(0:7)
)

reg[, com_outside_jobs_share_resident_emp := com_outside_jobs / resident_emp]
winsor(reg, "com_outside_jobs_share_resident_emp")
lf_multi <- run_lp_ratio_multiple_shocks(
  reg,
  outcome="w_com_outside_jobs_share_resident_emp",
  shock_1="w_IPW_US",
  shock_2="w_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_empw_neighbor_IPW_OTH",
  start_year=2000, 
  end_year=2008, 
  horizons = 0:6
)


reg[, ld_pop := log(population) - log(shift(population)), by = area_fips]

################################################################################
run_lp_lagged_denom(reg, outcome="com_outside_jobs", 
                    denominator="labor_force",
                    controls="+l1_own_shock+population", # removed w_ex_mean_work_IPW_US 
                    start_year = 2000, 
                    end_year = 2015)
################################################################################

reg[, com_outside_share_population := com_outside_jobs / resident_emp]
winsor(reg, "com_outside_share_population", p = .02)
winsor(reg, "empw_neighbor_IPW_US", p = .02)
winsor(reg, "empw_neighbor_IPW_OTH", p = .02)
reg[, w_l1_IPW_US := shift(w_IPW_US) , by= area_fips]
reg[, w_l1_IPW_OTH := shift(w_IPW_OTH) , by= area_fips]
run_lp_ratio_multiple_shocks(
  reg,
  outcome = "w_com_outside_share_population", 
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 2000,
  end_year = 2015
)


## Migration -------------------------------------------------------------------
n <- 0
reg[, net_full_migration := exemptions_3_inflow  - exemptions_3_outflow ]
reg[, net_domestic_migration := exemptions_3_inflow - exemptions_3_outflow]
# Same-state migration -------------------------------------------------------
reg[, total_domestic_migration := exemptions_3_inflow + exemptions_3_outflow]
# Net migration

# Cumulative net migration
setorder(reg, area_fips, year)

reg[year >= 1995, cum_net_full_mig := cumsum(net_full_migration), by = area_fips]
reg[year >= 1995, cum_net_domestic_mig := cumsum(net_domestic_migration), by = area_fips]
reg[year >= 1995, cum_domestic_mig := cumsum(total_domestic_migration), by = area_fips]
reg[year >= 1995, cum_domestic_inflow := cumsum(exemptions_3_inflow), by = area_fips]
reg[year >= 1995, cum_domestic_outflow := cumsum(exemptions_3_outflow), by = area_fips]
reg[year >= 1995, cum_same_inflow := cumsum(exemptions_same_state_inflow), by = area_fips]
reg[year >= 1995, cum_same_outflow := cumsum(exemptions_same_state_outflow), by = area_fips]
reg[year >= 1998, cum_exemptions := cumsum(exemptions), by = area_fips]

reg[, cum_total_domestic_migration := cum_domestic_inflow + cum_domestic_outflow]
reg[, cum_net_full_mig_share_population := cum_net_full_mig / population]
reg[, cum_net_domestic_mig_share_population := cum_net_domestic_mig / population]
reg[, cum_domestic_inflow_share_population := cum_domestic_inflow / population]
reg[, cum_domestic_outflow_share_population := cum_domestic_outflow / population]
reg[, cum_same_inflow_share_population := cum_same_inflow / population]
reg[, cum_same_outflow_share_population := cum_same_outflow / population]
reg[, cum_net_full_mig_share_migration := cum_net_full_mig / cum_total_domestic_migration]
reg[, cum_net_domestic_mig_share_migration := cum_net_domestic_mig / cum_total_domestic_migration]
reg[, cum_domestic_inflow_share_migration := cum_domestic_inflow / cum_total_domestic_migration]
reg[, cum_domestic_outflow_share_migration := cum_domestic_outflow / cum_total_domestic_migration]
reg[, cum_same_inflow_share_migration := cum_same_inflow / cum_total_domestic_migration]
reg[, cum_same_outflow_share_migration := cum_same_outflow / cum_total_domestic_migration]
reg[, cum_net_full_mig_share_nonmigration := cum_net_full_mig / exemptions]
reg[, cum_net_domestic_mig_share_nonmigration := cum_net_domestic_mig / exemptions]
reg[, cum_domestic_inflow_share_nonmigration := cum_domestic_inflow / exemptions]
reg[, cum_domestic_outflow_share_nonmigration := cum_domestic_outflow / exemptions]
reg[, cum_same_inflow_share_nonmigration := cum_same_inflow / exemptions]
reg[, cum_same_outflow_share_nonmigration := cum_same_outflow / exemptions]

winsor(reg, "cum_net_full_mig_share_population", p = .01)
winsor(reg, "cum_net_domestic_mig_share_population", p = .01)
winsor(reg, "cum_domestic_inflow_share_population", p = .01)
winsor(reg, "cum_domestic_outflow_share_population", p = .01)
winsor(reg, "cum_same_inflow_share_population", p = .01)
winsor(reg, "cum_same_outflow_share_population", p = .01)
winsor(reg, "cum_net_full_mig_share_migration", p = .01)
winsor(reg, "cum_net_domestic_mig_share_migration", p = .01)
winsor(reg, "cum_domestic_inflow_share_migration", p = .01)
winsor(reg, "cum_domestic_outflow_share_migration", p = .01)
winsor(reg, "cum_same_inflow_share_migration", p = .01)
winsor(reg, "cum_same_outflow_share_migration", p = .01)
winsor(reg, "cum_net_full_mig_share_nonmigration", p = .01)
winsor(reg, "cum_net_domestic_mig_share_nonmigration", p = .01)
winsor(reg, "cum_domestic_inflow_share_nonmigration", p = .01)
winsor(reg, "cum_domestic_outflow_share_nonmigration", p = .01)
winsor(reg, "cum_same_inflow_share_nonmigration", p = .01)
winsor(reg, "cum_same_outflow_share_nonmigration", p = .01)
winsor(reg, "cum_net_domestic_mig")
winsor(reg, "cum_net_full_mig", p = .01)

# Full
lp_full_mig <- run_lp_multiple_shocks(
  reg,
  outcome = "w_cum_net_full_mig",
  denominator = "population",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  start_year = 1995,
  end_year = 2015
)


################################################################################
lp_full_mig <- run_lp_ratio_multiple_shocks(
  reg,
  outcome = "w_cum_domestic_outflow_share_migration",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015
)
reg[, total_domestic_migration_share_nonmigration:=total_domestic_migration / population]
lp_full_mig <- run_lp_ratio_multiple_shocks(
  reg,
  outcome = "total_domestic_migration_share_nonmigration",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015
)
################################################################################

run_lp_ratio(reg, "w_cum_domestic_outflow_share_migration", start_year=1998, end_year=2015,
             shock = "w_IPW_US", 
             instrument ="w_IPW_OTH")


lp_full_mig <- run_lp_ratio_multiple_shocks(
  reg,
  outcome = "w_cum_domestic_outflow_share_nonmigration",
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015
)


lp_full_mig <- run_lp_multiple_shocks(
  reg,
  outcome = "cum_domestic_outflow",
  denominator = "exemptions", 
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015
)

lp_full_mig <- run_lp_multiple_shocks(
  reg,
  outcome = "cum_total_domestic_migration",
  denominator = "exemptions", 
  shock_1 = "w_IPW_US",
  shock_2 = "w_empw_neighbor_IPW_US",
  instrument_1 = "w_IPW_OTH",
  instrument_2 = "w_empw_neighbor_IPW_OTH",
  controls = "",
  start_year = 1995,
  end_year = 2015
)



run_lp_lagged_denom(reg, outcome="cum_domestic_outflow", 
                    denominator="exemptions",
                    controls="+l1_own_shock", # removed w_ex_mean_work_IPW_US 
                    start_year = 2000, 
                    end_year = 2015)


