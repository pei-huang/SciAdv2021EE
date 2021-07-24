# This file reads NEMS simulation data

# standard NEMS output data ----------------
filenames_model <- list.files(path=paste0(base_path, "NEMSPost/nems_simulation_data"), pattern="ref_no_cpp_+.*csv")
data_model_all <- list()
for (k in 1:length(filenames_model)){
  ## read data
  data_scen <- fread(paste0(base_path, "NEMSPost/nems_simulation_data/", filenames_model[k]),
                     na.strings = c("NA","NaN", ""),
                     fill = TRUE)
  
  ## process data into long format
  data_scen <- data_scen[, -c(1, dim(data_scen)[2], dim(data_scen)[2]-1), with = FALSE]
  col <- names(data_scen)[grepl("20", names(data_scen))]
  data_scen[, (col) := lapply(.SD, as.numeric), .SDcols = col]
  data_scen <- melt(data_scen, id.vars=names(data_scen)[!grepl("20", names(data_scen))], variable.name="Year")
  data_scen[, Year := as.numeric(as.character(Year))]
  
  data_scen[, RegionNum := as.integer(RegionNum)]
  data_scen[, `:=`(Scen = scenario[k],
                   Scenario.1 = scenario_name_1[k],
                   Scenario.2 = scenario_name_2[k],
                   Scenario.3 = scenario_name_3[k])]
  data_model_all[[k]] <- data_scen
}
data_model_all <- rbindlist(data_model_all)

## update data to reference before 2016
data_model_ref <- data_model_all[Scen == "s00"]
data_model_ref <- data_model_ref[, `:=`(Scen = NULL, Scenario.1 = NULL, Scenario.2 = NULL, Scenario.3 = NULL)]
setnames(data_model_ref, old = c("value"), new = c("refval"))

data_model_all <- merge(data_model_all, data_model_ref,
                        by = c("Datekey", "TableNumber", "RowNum", "RegionNum",
                               "VarName", "GLabel", "Gunits", "RowFmt",
                               "DaType", "SubDat", "Sector", "SubSec",
                               "Source", "SubSrc", "Geogr", "Year"))
data_model_all[Year <= 2016, value := refval]
data_model_all[, refval := NULL]

## split data for the main scenarios and sensitivity scenarios
data_model_main <- data_model_all[Scen %in% scenario_main]
data_model_sens <- data_model_all[Scen %in% scenario_sens]


# CO2 emissions by fuel from NEMS runs ------------------------------------------------------------------
filenames_co2 <- list.files(path=paste0(base_path, "/NEMSPost/nems_simulation_data"), pattern="demand_co2_+.*csv")
data_co2_sec_all <- list()
for (k in 1:length(filenames_co2)){
  ## read residential data
  data_co2_scen_res <- fread(paste0(base_path, "/NEMSPost/nems_simulation_data/", filenames_co2[k]),
                             strip.white = TRUE,
                             stringsAsFactors = FALSE,
                             na.strings = c("NA","NaN", ""),
                             fill = TRUE,
                             header = TRUE,
                             skip = 6,
                             nrows = 11)
  
  ## process data into long format
  data_co2_scen_res <- data_co2_scen_res[-c(1:2), -c(2:3, dim(data_co2_scen_res)[2]), with = FALSE]
  names(data_co2_scen_res)[1] <- c("Fuel")
  data_co2_scen_res[, Fuel := trimws(Fuel, which = c("both"), whitespace = " ")]
  data_co2_scen_res <- data_co2_scen_res[Fuel %in% c("Oil Subtotal",
                                                     "Natural Gas",
                                                     "Electricity",
                                                     "Total Residential")]
  data_co2_scen_res[Fuel == "Total Residential", Fuel := "Total"]
  data_co2_scen_res[, Sector := "Residential"]
  
  ## read commercial data
  data_co2_scen_com <- fread(paste0(base_path, "/NEMSPost/nems_simulation_data/", filenames_co2[k]),
                             strip.white = TRUE,
                             stringsAsFactors = FALSE,
                             na.strings = c("NA","NaN", ""),
                             fill = TRUE,
                             header = TRUE,
                             skip = 34,
                             nrows = 13)
  
  ## process data into long format
  data_co2_scen_com <- data_co2_scen_com[-c(1:2), -c(2:3, dim(data_co2_scen_com)[2]), with = FALSE]
  names(data_co2_scen_com)[1] <- c("Fuel")
  data_co2_scen_com[, Fuel := trimws(Fuel, which = c("both"), whitespace = " ")]
  data_co2_scen_com <- data_co2_scen_com[Fuel %in% c("Oil Subtotal",
                                                     "Coal",
                                                     "Natural Gas",
                                                     "Electricity",
                                                     "Total Commercial")]
  data_co2_scen_com[Fuel == "Total Commercial", Fuel := "Total"]
  data_co2_scen_com[, Sector := "Commercial"]
  
  ## read industrial data
  data_co2_scen_ind <- fread(paste0(base_path, "/NEMSPost/nems_simulation_data/", filenames_co2[k]),
                             strip.white = TRUE,
                             stringsAsFactors = FALSE,
                             na.strings = c("NA","NaN", ""),
                             fill = TRUE,
                             header = TRUE,
                             skip = 66,
                             nrows = 24)
  
  ## process data into long format
  data_co2_scen_ind <- data_co2_scen_ind[-c(1:2), -c(2:3, dim(data_co2_scen_ind)[2]), with = FALSE]
  names(data_co2_scen_ind)[1] <- c("Fuel")
  data_co2_scen_ind[, Fuel := trimws(Fuel, which = c("both"), whitespace = " ")]
  data_co2_scen_ind <- data_co2_scen_ind[Fuel %in% c("Oil Subtotal",
                                                     "Coal Subtotal",
                                                     "Natural Gas Subtotal",
                                                     "Electricity",
                                                     "Total Industrial")]
  data_co2_scen_ind[Fuel == "Total Industrial", Fuel := "Total"]
  data_co2_scen_ind[, Sector := "Industrial"]
  
  ## read transportation data
  data_co2_scen_trn <- fread(paste0(base_path, "/NEMSPost/nems_simulation_data/", filenames_co2[k]),
                             strip.white = TRUE,
                             stringsAsFactors = FALSE,
                             na.strings = c("NA","NaN", ""),
                             fill = TRUE,
                             header = TRUE,
                             skip = 120,
                             nrows = 17)
  
  ## process data into long format
  data_co2_scen_trn <- data_co2_scen_trn[-c(1:2), -c(2:3, dim(data_co2_scen_trn)[2]), with = FALSE]
  names(data_co2_scen_trn)[1] <- c("Fuel")
  data_co2_scen_trn[, Fuel := trimws(Fuel, which = c("both"), whitespace = " ")]
  data_co2_scen_trn <- data_co2_scen_trn[Fuel %in% c("Oil Subtotal",
                                                     "Natural Gas",
                                                     "Electricity",
                                                     "Total Transportation")]
  data_co2_scen_trn[Fuel == "Total Transportation", Fuel := "Total"]
  data_co2_scen_trn[, Sector := "Transportation"]
  
  ## merge sector data
  data_co2_scen <- rbind(data_co2_scen_res, data_co2_scen_com, data_co2_scen_ind, data_co2_scen_trn)
  
  data_co2_scen <- melt(data_co2_scen, id.vars = c("Sector", "Fuel"), variable.name = "Year", variable.factor = FALSE)
  data_co2_scen[, Year := as.integer(Year)]
  data_co2_scen <- data_co2_scen[Year >= 2013]
  
  data_co2_scen[, `:=`(Scen = scenario[k],
                       Scenario.1 = scenario_name_1[k],
                       Scenario.2 = scenario_name_2[k],
                       Scenario.3 = scenario_name_3[k])]
  
  data_co2_sec_all[[k]] <- data_co2_scen
}
data_co2_sec_all <- rbindlist(data_co2_sec_all)

## update data to reference before 2016
data_co2_sec_ref <- data_co2_sec_all[Scen == "s00"]
data_co2_sec_ref <- data_co2_sec_ref[, `:=`(Scen = NULL, Scenario.1 = NULL, Scenario.2 = NULL, Scenario.3 = NULL)]
setnames(data_co2_sec_ref, old = c("value"), new = c("refval"))

data_co2_sec_all <- merge(data_co2_sec_all, data_co2_sec_ref,
                          by = c("Sector", "Fuel", "Year"))
data_co2_sec_all[Year <= 2016, value := refval]
data_co2_sec_all[, refval := NULL]
