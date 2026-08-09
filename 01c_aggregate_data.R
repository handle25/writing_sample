# using U.S. HS10 import values to select the dominant SIC within each HS6
weights <- data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_91n/imp_detl_yearly_91n.dta")))
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_95n/imp_detl_yearly_95n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_100n/imp_detl_yearly_100n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_107n/imp_detl_yearly_107n.dta"))), fill = TRUE)
weights <- rbind(weights, data.table(read_stata(paste0(path, "/peter_schott/imp_detl_yearly_113n/imp_detl_yearly_113n.dta"))), fill = TRUE)
fwrite(weights, paste0(path, "/peter_schott/weights.csv"))
  