path <- "D:/writing_sample/data"
local <- "C:/Users/Sophie/Desktop/phd_apps/writing_sample/data"
date <- Sys.Date()
figs <- "D:/writing_sample/figures"

# long differences -------------------------------------------------------------
reg <- fread(paste0(path, "/output/transformed_reg.csv"))
## shock  ----------------------------------------------------------------------
file <- paste0(figs, "/shock_distribution.tex")
p <- c(.01,.05,.25,.50,.75,.90,.99) 
d <- 2 
sum07 <- round(quantile(reg[year==2007, w_IPW_US], p = p, na.rm = T), d)
sum13 <- round(quantile(reg[year==2013, w_IPW_US], p = p,na.rm = T), d)

mean07 <- round(weighted.mean(reg[year==2007, w_IPW_US], reg[year==2007, baseline_emp], na.rm=T), 3)
mean13 <- round(weighted.mean(reg[year==2013, w_IPW_US], reg[year==2013, baseline_emp], na.rm=T), 3)

cat("\\begin{tabular}{l", rep("c", length(p)), "c} \n", file = file)
cat("\\toprule \n & \\multicolumn{", length(p), "}{c}{Percentile}  \\\\ \n \\cmidrule{2-8}  ",  
    file = file, append = T)

cat(" & 1 & 5 & 25 & 50 & 75 & 90 & 99 & Mean \\\\\n \\midrule ",  file = file, append = T)
cat("2000-2007 Import Exposure ", sum07, mean07, sep = "&", file = file, append = T)
cat("\\\\\n", file = file, append = T)
cat("2007-2013 Import Exposure ",sum13, mean13, sep = "&", file = file, append = T)
cat("\\\\\nTotal", rep("", length(p)), round(mean07 + mean13, 3), sep = "&", file = file, append = T)
cat("\\\\\n \\bottomrule \n \\end{tabular}", file = file, append = T)

## dep vars -------------------------------------------------------------------
file <- paste0(figs, "/dep_var_distribution.tex")
p <- c(.01,.25,.50,.75,.99)
d <- 2

to_summarize <- c("w_d_labor_force_share_population", "w_d_unemployed_share_labor_force",
                  "w_d_outside_jobs_share_population", "w_d_net_outmigration")

cat("\\begin{tabular}{l", rep("c", length(p)), "c} \n", file=file)
cat("\\toprule \n & \\multicolumn{", length(p), "}{c}{Percentile} & \\\\ \n \\cmidrule{2-6}\n", file=file, append=T)
cat(" & 1 & 25 & 50 & 75 & 99 & Mean \\\\\n \\midrule\n", file=file, append=T)

cat("\\multicolumn{7}{l}{\\textit{Panel A: 2000--2007}} \\\\\n", file=file, append=T)
for (v in to_summarize) {
  q <- round(quantile(reg[year %in% 2000:2007, get(v)], p=p, na.rm=T), d)
  m <- round(mean(reg[year %in% 2000:2007, get(v)], na.rm=T), 3)
  cat(names_dict[v], q, m, sep="&", file=file, append=T)
  cat("\\\\\n", file=file, append=T)
}

cat("\\addlinespace\n", file=file, append=T)
cat("\\multicolumn{7}{l}{\\textit{Panel B: 2007--2013}} \\\\\n", file=file, append=T)
for (v in to_summarize) {
  q <- round(quantile(reg[year %in% 2007:2013, get(v)], p=p, na.rm=T), d)
  m <- round(weighted.mean(reg[year %in% 2007:2013, get(v)], reg[year %in% 2007:2013, baseline_emp], na.rm=T), 3)
  cat(names_dict[v], q, m, sep="&", file=file, append=T)
  cat("\\\\\n", file=file, append=T)
}

cat("\\bottomrule \n\\end{tabular}", file=file, append=T)

stop()
# local projections 
reg <- fread(paste0(path, "/output/lp_transformed_reg.csv"))

file <- paste0(figs, "/lp_shock_distribution.tex")
p <- c(.01,.05,.25,.50,.75,.90,.99) 
means <- 0 
cat("\\begin{tabular}{l", rep("c", length(p)), "c} \n", file = file)
cat("\\toprule \n & \\multicolumn{", length(p), "}{c}{Percentile}  \\\\ \n \\cmidrule{2-8}  ",  
    file = file, append = T)
cat(" & 1 & 5 & 25 & 50 & 75 & 90 & 99 & Mean \\\\\n \\midrule ",  file = file, append = T)

for (y in 2000:2013){
  sum <- round(quantile(reg[year==y, w_IPW_US], p = p, na.rm = T), d)
  mean <- round(mean(reg[year==y,w_IPW_US], na.rm = T), 3)
  cat(paste0(y, " Import Exposure "), sum, mean, sep = "&", file = file, append = T)
  cat("\\\\\n", file = file, append = T)
  
  means <- means + mean(reg[year==y,w_IPW_US], na.rm = T) 
}

cat("Total", rep("", length(p)), round(means, 3), sep="&", file=file, append=T)
cat("\\\\\n", file=file, append=T)
cat(" \\bottomrule \n \\end{tabular}", file = file, append = T)


# labor force participation 
laus <- read_excel(paste0(path, "/laus/laucnty90.xlsx"), skip = 1)
for (year in c(1991:2024)) {
  y <- sprintf("%02.f", as.integer(substr(as.character(year), 3,4)))
  laus <- rbind(laus, 
                read_excel(paste0(path, "/laus/laucnty", y, ".xlsx"),
                           skip = 1)
  )
}
laus <- laus |> 
  clean_names() |>
  data.table() |> 
  fmutate(area_fips = as.integer(
    paste0(state_fips_code, county_fips_code)), 
    year = as.integer(year))

acs <- fread(paste0(path, "/acs/population_1995_2023.csv"))

laus <- merge(laus, acs, by = c("year", "area_fips"))
laus <- laus[year %in% 2000:2013,]

laus <- laus[,.(
  labor_force = sum(labor_force, na.rm = T), 
  population = sum(population, na.rm = T)), 
  by = year]
laus[, lfp := labor_force / population ]
ggplot(laus, aes(
  x = year, 
  y = lfp
))+ 
  geom_line()
