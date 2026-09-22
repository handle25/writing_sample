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
path <- "D:/writing_sample/data"
figs <- "D:/writing_sample/figures"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
setwd(path)
source(paste0(local, "/../code/writing_sample/utilities.R"))
# read in data -----------------------------------------------------------------
reg <- fread(paste0(path, "/output/lp_transformed_reg.csv"))
setorder(reg, area_fips, year)
# reg <- reg[year != 2009]
reg <- reg[year<2020]

# reg <- reg[year<=2019 & year >=1995]
# balanced <- unique(reg[year %in% 1995:2019,.N, by = area_fips][N==22]$area_fips)
# 
# #extract balanced panel 
# reg <- reg[area_fips %in% balanced,]
setorder(reg, area_fips, year)
reg[, baseline_resident_emp := resident_emp[year == 2000][1], by=area_fips]

reg[, l_sh_empl_mfg := shift(sh_empl_mfg), by = area_fips]
reg[, total_migration := returns_3_inflow + returns_3_outflow]
reg[, net_migration := returns_3_inflow - returns_3_outflow]
reg[, net_migration_share_population := net_migration / total_migration]
reg[, l2_ex_mean_work_IPW_US := shift(ex_mean_work_IPW_US, n = 2), by = area_fips]
reg[, l1_w_ex_mean_work_IPW_US := shift(w_ex_mean_work_IPW_US, n = 1), by = area_fips]


shocks <- grep("IPW", names(reg), value=TRUE)
for (v in shocks) reg[, (paste0("l1_", v)) := shift(get(v), 1), by=area_fips]
for (v in shocks) winsor(reg, v, p = .02)

var <- "w_ex_mean_work_IPW_US"
reg[, unemployed := as.double(unemployed)]

reg[, {
  x <- get(var)
  w <- baseline_emp
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  
  .(
    N=length(x),
    mean=weighted.mean(x,w),
    sd=sqrt(weighted.mean((x-weighted.mean(x,w))^2,w)),
    min=min(x),
    p25=Hmisc::wtd.quantile(x,w,probs=.25),
    median=Hmisc::wtd.quantile(x,w,probs=.5),
    p75=Hmisc::wtd.quantile(x,w,probs=.75),
    max=max(x)
  )
}, by=year]

# run regressions --------------------------------------------------------------

# Labor market
# run_lp_lagged_denom(reg, outcome="labor_force", denominator="population")
# run_lp_lagged_denom(reg, outcome="labor_force", denominator="population", 
#        shock = "w_IPW_US", 
#        instrument = "w_fn_mean_work_IPW_OTH",
#        controls = "+l1_own_shock+l2_own_shock")
# 
# run_lp_lagged_denom(reg, outcome="labor_force", denominator="population", 
#        controls = "+l1_own_shock+l2_own_shock")
# 
# run_lp_lagged_denom(reg, outcome="labor_force", denominator="population", 
#        controls = "+l1_own_shock+l2_own_shock+w_fn_mean_work_IPW_US")
# 
# # unemployment 
# run_lp_lagged_denom(reg, outcome="unemployed", denominator="labor_force", 
#        shock = "w_IPW_US", 
#        instrument = "w_ex_mean_work_IPW_OTH",
#        controls = "+l1_own_shock+l2_own_shock+l1_w_ex_mean_work_IPW_OTH",
#        end_year = 2010)
# 
# run_lp_lagged_denom(reg, outcome="unemployed", denominator="labor_force", 
#        shock = "w_IPW_US", 
#        instrument = "w_IPW_OTH",
#        controls = "+l1_own_shock+l2_own_shock",
#        end_year = 2010)
# 
# run_lp_lagged_denom(reg, outcome="unemployed", denominator="labor_force", 
#        controls = "+ex_mean_work_IPW_US", end_year = 2012)
# 
# run_lp_lagged_denom(reg, outcome="unemployed", denominator="labor_force",
#        shock="w_ex_mean_work_IPW_US",
#        instrument="w_ex_mean_work_IPW_OTH",
#        end_year=2008)
# 
# 
# # Commuting
# run_lp_lagged_denom(reg, outcome="outside_jobs", denominator="population",
#        controls = "+ex_mean_work_IPW_US", end_year = 2013)
# run_lp_lagged_denom(reg, outcome="outside_jobs", denominator="population",
#        controls = "+ex_mean_work_IPW_US")
# run_lp_lagged_denom(reg, outcome="outside_jobs", denominator="labor_force",
#        controls = "+ex_mean_work_IPW_US", end_year = 2013)
# run_lp_lagged_denom(reg, outcome="outside_jobs", denominator="workplace_emp",
#        controls = "+ex_mean_work_IPW_US", end_year = 2007)
# run_lp_lagged_denom(reg, outcome="outside_jobs", denominator="workplace_emp",
#          controls = "+ex_mean_work_IPW_US", start_year = 2010, end_year = 2013)
# 

run_lp_lagged_denom(reg, outcome="outside_jobs", 
                    denominator="resident_emp",
                    controls="+l1_own_shock+w_ex_mean_work_IPW_US",
                    start_year = 1998, 
                    end_year = 2008)

run_lp_lagged_denom(reg, outcome="exemptions_3_outflow", 
                    denominator="exemptions",
                    controls="+l1_own_shock+w_ex_mean_work_IPW_US",
                    start_year = 1997, 
                    end_year = 2020)

# # Migration
# run_lp_lagged_denom(reg, outcome="exemptions_net_migration", denominator="population",
#               controls = "+ex_mean_work_IPW_US")
# run_lp_lagged_denom(reg, outcome="exemptions_3_outflow", denominator="exemptions_total_migration")
# run_lp_lagged_denom(reg, outcome="returns_3_outflow", denominator="returns_total_migration")
# run_lp_lagged_denom(reg, outcome="returns_net_migration", denominator="population")
# 

# multi shock regs -------------------------------------------------------------

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="unemployed",
  denominator="labor_force",
  shock_1="w_IPW_US",
  shock_2="w_ex_mean_work_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_ex_mean_work_IPW_OTH",
  start_year=2000,
  end_year=2015
)

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="total_migration",
  denominator="population",
  shock_1="w_IPW_US",
  shock_2="w_ex_mean_work_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_ex_mean_work_IPW_OTH",
  controls="+l1_own_shock+l2_own_shock",
  start_year=1998,
  end_year=2015
)

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="outside_jobs",
  denominator="labor_force",
  shock_1="w_IPW_US",
  shock_2="w_ex_mean_work_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_ex_mean_work_IPW_OTH",
  controls="+l1_own_shock",
  start_year=2000,
  end_year=2015
)

setorder(reg, area_fips, year)
reg[, l_empw_neighbor_IPW_US := shift(empw_neighbor_IPW_US), by = area_fips]
reg[, l_empw_neighbor_IPW_OTH := shift(empw_neighbor_IPW_OTH), by = area_fips]
winsor(reg, "l_empw_neighbor_IPW_OTH", p = .01)
winsor(reg, "l_empw_neighbor_IPW_US", p = .01 )

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="outside_jobs",
  denominator="labor_force", # or resident_emp
  shock_1="w_IPW_US",
  shock_2="w_l_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l_empw_neighbor_IPW_OTH",
  controls="+l1_own_shock",
  start_year=2000,
  end_year=2015
)


lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="net_migration",
  denominator="labor_force", # or resident_emp
  shock_1="l1_w_IPW_US",
  shock_2="w_l_empw_neighbor_IPW_US",
  instrument_1="l1_w_IPW_OTH",
  instrument_2="w_l_empw_neighbor_IPW_OTH",
  controls="+l1_own_shock",
  start_year=2000,
  end_year=2015
)


lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="outside_jobs",
  denominator="labor_force", # or resident_emp
  shock_1="w_IPW_US",
  shock_2="w_l_empw_neighbor_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_l_empw_neighbor_IPW_OTH",
  controls="+l1_own_shock",
  start_year=2000,
  end_year=2020
)




lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="labor_force",
  denominator="population",
  shock_1="w_IPW_US",
  shock_2="w_ex_mean_work_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_ex_mean_work_IPW_OTH",
  controls="+l1_own_shock",
  start_year=2000,
  end_year=2015
)

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="outside_goods_jobs",
  denominator="resident_emp",
  shock_1="w_IPW_US",
  shock_2="w_ex_mean_work_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_ex_mean_work_IPW_OTH",
  controls="+l1_own_shock",
  start_year=2000,
  end_year=2007
)


lf_multi <- run_lp_multiple_shocks(
  reg[year!=2019,],
  outcome="outside_jobs",
  denominator="labor_force",
  shock_1="l1_w_IPW_US",
  shock_2="l1_w_ex_mean_work_IPW_US",
  instrument_1="l1_w_IPW_OTH",
  instrument_2="l1_w_ex_mean_work_IPW_OTH",
  start_year=2000,
  end_year=2010, 
  controls = "+rw_mean_into_IPW_US"
)

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="outside_jobs",
  denominator="labor_force",
  shock_1="l1_w_IPW_US",
  shock_2="l1_w_ex_mean_work_IPW_US",
  instrument_1="l1_w_IPW_OTH",
  instrument_2="l1_w_ex_mean_work_IPW_OTH",
  start_year=2000,
  end_year=2007, 
  controls = "+rw_share_into_less_unemp"
)

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="outside_jobs",
  denominator="labor_force",
  shock_1="l1_w_IPW_US",
  shock_2="l1_w_ex_mean_work_IPW_US",
  instrument_1="l1_w_IPW_OTH",
  instrument_2="l1_w_ex_mean_work_IPW_OTH",
  controls = "+l1_own_shock+l2_own_shock+exemptions_3_outflow_share_exemptions_net_migration", 
  start_year=2000,
  end_year=2007
)

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="outside_jobs",
  denominator="labor_force",
  shock_1="l1_w_IPW_US",
  shock_2="l1_w_ex_mean_work_IPW_US",
  instrument_1="l1_w_IPW_OTH",
  instrument_2="l1_w_ex_mean_work_IPW_OTH",
  controls = "+l1_own_shock", 
  start_year=2000,
  end_year=2007
)

lf_multi <- run_lp_multiple_shocks(
  reg,
  outcome="outside_jobs",
  denominator="labor_force",
  shock_1="w_IPW_US",
  shock_2="w_ex_mean_work_IPW_US",
  instrument_1="w_IPW_OTH",
  instrument_2="w_ex_mean_work_IPW_OTH",
  start_year=2000,
  end_year=2007
)
pca_dt <- reg[, c("unemployed","IPW_US", "ex_mean_work_IPW_US", "rw_share_into_less_unemp", "year")]
pca_dt<- pca_dt[!is.na(IPW_US), ]
pca_dt<- pca_dt[!is.na(unemployed), ]
pca_dt<- pca_dt[!is.na(ex_mean_work_IPW_US), ]
pca_dt<- pca_dt[!is.na(rw_share_into_less_unemp), ]
pca <- prcomp(pca_dt)