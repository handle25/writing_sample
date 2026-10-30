# Long-difference motivation tables: all variables come from transformed_reg_aux.
# Same LD IV specification as 04_final_regs_aux_2026-10-29.R:
# period indicator + baseline controls, employment weights, state-clustered SEs.
# This is an LD model, not the LP's two-shock dynamic specification.
source("C:/Users/Sophie/Desktop/phd_apps/writing_sample/code/writing_sample/utilities.R")
reg <- fread(file.path(path,"output/transformed_reg_aux_2026-10-29.csv"))
# All outcome variables are constructed in 03_merge_transform_aux.
setorder(reg,area_fips,year)

# Regression periods ------------------------------------------------------------
reg07 <- reg[year %in% c(2000,2007)]
reg13 <- reg[year %in% c(2007,2013)]
reg07[,t2:=as.integer(year==2007)]
reg13[,t2:=as.integer(year==2013)]
out_dir <- file.path(dirname(path),"figures","LD_motivation_LP_outcomes_aux_2026-10-29")
if(lp_save_outputs()) dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)

names_dict <- c("fit_w_IPW_US"="Import Exposure","t2"="Period 2",
  "w_ld_outside_jobs"="Outside jobs: change / baseline resident employment",
  "w_ld_outside_share"="$\\Delta$ outside / resident employment share",
  "w_ld_outside_jobs_2002"="Outside jobs: 2002-baseline robustness",
  "w_ld_outside_share_2002"="Outside employment share: 2002-baseline robustness",
  "w_ld_unemployment"="$\\Delta$ unemployment rate",
  "w_ld_lfp"="$\\Delta$ labor-force participation",
  "w_ld_manufacturing"="$\\Delta$ manufacturing employment share",
  "w_ld_hhi"="$\\Delta$ industry HHI (x100)",
  "w_ld_migration_total"="Cumulative total migration / baseline exemptions",
  "w_ld_migration_inflow"="Inflow change / baseline exemptions",
  "w_ld_migration_inward_share"="$\\Delta$ inward share of migration (returns)",
  "w_d_returns_3_outflow_share_returns_migration"="$\\Delta$ outward share of migration (returns)",
  "w_ld_low_earnings_outside"="$\\Delta$ low-earnings share of outside jobs",
  "w_ld_inside_nongoods"="$\\Delta$ inside share of nongoods jobs")

# Final tables: same IV method and layout as 04_final_regs_aux -------------------
final_table <- function(depvar, desc,
  periods=c("1990--2000 and 2000--2007","2000--2007 and 2007--2013")) {

  fml <- as.formula(paste0(depvar,
    " ~ t2 + l_sh_empl_mfg + sh_popfborn + sh_popedu_c + sh_empl_f | w_IPW_US ~ w_IPW_OTH"))

  mod07 <- feols(fml,data=reg07,weights=~baseline_emp,cluster=~statefip)
  mod13 <- feols(fml,data=reg13,weights=~baseline_emp,cluster=~statefip)
  mods <- list(mod07,mod13)

  print(etable(mods,dict=names_dict,digits=3))
  if(!lp_save_outputs()) return(invisible(mods))

  file <- file.path(out_dir,paste0(desc,".tex"))
  period_labels <- c(if(grepl("2002",periods[1])) "2002--2007" else "2000--2007", "2007--2013")
  etable(mods,
    dict=names_dict,
    drop=c("Constant","sh_popfborn","sh_popedu_c","l_sh_empl_mfg","sh_empl_f"),
    extralines=list(
      "Baseline manufacturing share"=rep("$\\checkmark$",2),
      "Baseline demographic controls"=rep("$\\checkmark$",2)),
    digits=3,
    style.tex=style.tex(var.title="",fixef.title="",stats.title="",notes.tpt.intro=""),
    headers=as.list(setNames(c(1,1),period_labels)),
    tex=TRUE,file=file,replace=TRUE,notes="")

  x <- readLines(file)
  x <- sub("Dependent Variable:","",x,fixed=TRUE)
  x <- sub("Model:","",x,fixed=TRUE)
  x <- x[!grepl("standard-errors in parentheses",x)]
  x <- x[!grepl("Signif\\. Codes",x)]
  i <- grep(paste(period_labels,collapse=".*"),x)
  x <- append(x,"\\cmidrule(lr){2-2}\\cmidrule(lr){3-3}",after=i)
  i <- grep("Import Exposure",x,fixed=TRUE)
  x <- append(x,"\\midrule",after=i[1]-1L)
  i <- grep("Baseline manufacturing share",x,fixed=TRUE)
  x[i] <- sub("Baseline manufacturing share","\\midrule Baseline manufacturing share",x[i],fixed=TRUE)
  writeLines(x,file)
  invisible(mods)
}

# Tables: one outcome per file ---------------------------------------------------
final_table("w_ld_outside_jobs","commuting_count_change")
final_table("w_ld_outside_share","commuting_share_change")

final_table("w_ld_outside_jobs_2002","commuting_count_change_2002",
  c("1990--2000 and 2002--2007","2002--2007 and 2007--2013"))
final_table("w_ld_outside_share_2002","commuting_share_change_2002",
  c("1990--2000 and 2002--2007","2002--2007 and 2007--2013"))

final_table("w_ld_unemployment","unemployment")
final_table("w_ld_lfp","labor_force_participation")
final_table("w_ld_manufacturing","manufacturing")
final_table("w_ld_hhi","industry_hhi")

# Migration differences: 1995-2000, 2000-2007, and 2007-2013.
final_table("w_ld_migration_total","migration_total",
  c("1995--2000 and 2000--2007","2000--2007 and 2007--2013"))
final_table("w_ld_migration_inflow","migration_inflow")
final_table("w_d_returns_3_outflow_share_returns_migration","migration_outward_share")
final_table("w_ld_migration_inward_share","migration_inward_share")

# Census baseline years lack these earnings/industry composition measures.
final_table("w_ld_low_earnings_outside","low_earnings_outside",
  c("2002--2007","2002--2007 and 2007--2013"))
final_table("w_ld_inside_nongoods","inside_nongoods",
  c("2002--2007","2002--2007 and 2007--2013"))
