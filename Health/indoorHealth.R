# This file estimates the health effects of indoor air quality
## based on the indoor air quality analysis

# read residential building stock data (from NEMS) and population data --------------------------------------------------------
data_house_raw <- readLines("Health/IAQData/RESOUT_00_ref.TXT")

## houses built before 2009
data_house_exist <- data_house_raw[which(grepl("HOUSINGSTOCK:EXISTING", data_house_raw))]
data_house_exist <- strsplit(data_house_exist, ",")
data_house_exist <- data.table(matrix(unlist(data_house_exist), nrow=length(data_house_exist), byrow=TRUE), stringsAsFactors = FALSE)
data_house_exist[, V1 := NULL]
data_house_exist[, V7 := "Old Homes"]

## houses built after 2009
data_house_new <- data_house_raw[which(grepl("HOUSINGSTOCK:NEW", data_house_raw))]
data_house_new <- strsplit(data_house_new, ",")
data_house_new <- data.table(matrix(unlist(data_house_new), nrow=length(data_house_new), byrow=TRUE), stringsAsFactors = FALSE)
data_house_new[, V1 := NULL]
data_house_new[, V7 := "New Homes"]

data_house <- rbind(data_house_exist, data_house_new)

## rename the headers
setnames(data_house, c("ITER","Year","Build.Type","Census.ID","value","Build.Group"))

data_house[, `:=`(Year = as.integer(Year),
                  Census.ID = as.integer(Census.ID),
                  value = as.numeric(value))]

## remove duplicates and aggregate
data_house <- data_house[, .(value = value[.N]),
                         by = .(Year, Build.Type, Census.ID, Build.Group)]
data_house <- data_house[, .(value = sum(value)),
                         by = .(Year, Census.ID, Build.Group)]

## calculate the total number of residents
### Average 2.6 persons per household (source: https://www.census.gov/quickfacts/fact/table/US/HCN010212)
data_house[, pop := value * 2.6]
data_house[, value := NULL]


# read US population data from NEMS --------------------------------------------------------
data_nems <- fread("NEMSPost/nems_simulation_data/ref_no_cpp_00_ref.csv",
                   strip.white = TRUE,
                   stringsAsFactors = FALSE,
                   na.strings = c("NA","NaN", ""),
                   fill = TRUE)

## process data into long format
data_nems <- data_nems[, -c(1, dim(data_nems)[2], dim(data_nems)[2]-1), with = FALSE]
col <- names(data_nems)[grepl("20", names(data_nems))]
data_nems[, (col) := lapply(.SD, as.numeric), .SDcols = col]
data_nems <- melt(data_nems, id.vars=names(data_nems)[!grepl("20", names(data_nems))], variable.name="Year")
data_nems[, Year := as.numeric(as.character(Year))]
data_nems[, RegionNum := as.integer(RegionNum)]

data_pop <- data_nems[TableNumber == 2
                      & SubDat == "population"
                      & Geogr != "united states", .(Year, RegionNum, value)]
### convert unit
data_pop[, pop := value*1e6]
data_pop[, value := NULL]

### spread total population into 10 uniform deciles (to match with IAQ concentration deciles)
data_pop_split <- copy(data_pop)
data_pop_split[, pop := pop/10]


# read percentage of houses with recirculation --------------------------------------------------------
data_recirc <- data.table(read_xlsx("Health/IAQData/BaselineIAQData/Recirculation.xlsx", col_names = TRUE))
data_recirc <- na.omit(data_recirc)
names(data_recirc) <- c("Year", "ref", "int", "opt")
data_recirc <- melt(data_recirc, id.vars = c("Year"), variable.name = "Scen", value.name = "cir")
data_recirc[, nocir := 1-cir]
data_recirc <- melt(data_recirc, id.vars = c("Year", "Scen"), variable.name = "Recirc", value.name = "cir.pct")
data_recirc[, Year := as.integer(Year)]


# read indoor PM2.5 concentration data --------------------------------------------------------
## baseline data
file_plt_base <- list.files(pattern="20+.*xlsx", path = paste0(base_path, "/Health/IAQData/BaselineIAQData/"))
data_plt_base <- list()
for (i in 1:length(file_plt_base)){
  year_label <- as.integer(str_sub(file_plt_base[i], 1, 4))
  data_read <- data.table(read_xlsx(paste0(base_path, "/Health/IAQData/BaselineIAQData/", file_plt_base[i]), col_names = FALSE, skip = 3))
  names(data_read) <- c("Decile",
                        "ref_cir_con", "ref_cir_std", "ref_nocir_con", "ref_nocir_std",
                        "int_cir_con", "int_cir_std", "int_nocir_con", "int_nocir_std",
                        "opt_cir_con", "opt_cir_std", "opt_nocir_con", "opt_nocir_std")
  data_read[, Year := year_label]
  data_read[, case := "base"]
  data_read[, sens_level := 100]
  data_plt_base[[i]] <- data_read
}
data_plt_base <- rbindlist(data_plt_base)


## sensitivity data (only for 2050)
file_plt_sens <- list.files(pattern="Year38+.*PM25Conc.csv", path = paste0(base_path, "/Health/IAQData/SensAnalysisIAQData/"))
data_plt_sens <- list()
for (i in 1:length(file_plt_sens)){
  sens_label <- str_remove_all(file_plt_sens[i], "Year38Scale")
  sens_label <- as.integer(str_remove_all(sens_label, "PM25Conc.csv"))
  
  data_read <- fread(paste0(base_path, "/Health/IAQData/SensAnalysisIAQData/", file_plt_sens[i]))
  names(data_read) <- c("Decile",
                        "ref_cir_con", "ref_cir_std", "ref_nocir_con", "ref_nocir_std",
                        "int_cir_con", "int_cir_std", "int_nocir_con", "int_nocir_std",
                        "opt_cir_con", "opt_cir_std", "opt_nocir_con", "opt_nocir_std")
  data_read[, Year := 2050]
  data_read[, case := "sens"]
  data_read[, sens_level := sens_label]
  data_plt_sens[[i]] <- data_read
}
data_plt_sens <- rbindlist(data_plt_sens)

## convert to long format
data_plt <- rbind(data_plt_base, data_plt_sens)
data_plt <- melt(data_plt, id.vars = c("Year", "case", "sens_level", "Decile"), variable.name = "Scen", value.name = "plt")

## split the first column
data_plt[, c("Scen", "Recirc", "variable") := tstrsplit(Scen, "_", fixed=TRUE)]

data_plt <- data_plt[variable == "con"]
data_plt[, `:=`(variable = NULL)]


# read indoor PM2.5 indoor/outdoor origin ratio data -------------------------------------
## baseline data
file_ioratio_base <- list.files(pattern="Year+.*xlsx", path = paste0(base_path, "/Health/IAQData/BaselineIAQData/"))
data_ioratio_base <- list()
for (i in 1:length(file_ioratio_base)){
  year_label <- str_remove(file_ioratio_base[i], "Year")
  year_label <- as.integer(str_remove(year_label, "IOpct.xlsx"))
  year_label <- year_label + 2012
  
  data_read <- data.table(read_xlsx(paste0(base_path, "/Health/IAQData/BaselineIAQData/", file_ioratio_base[i]), col_names = FALSE, skip = 3))
  names(data_read) <- c("Decile",
                        "ref_cir_indoor", "ref_cir_outdoor", "ref_nocir_indoor", "ref_nocir_outdoor",
                        "int_cir_indoor", "int_cir_outdoor", "int_nocir_indoor", "int_nocir_outdoor",
                        "opt_cir_indoor", "opt_cir_outdoor", "opt_nocir_indoor", "opt_nocir_outdoor")
  
  data_read[, Year := year_label]
  data_read[, case := "base"]
  data_read[, sens_level := 100]
  data_ioratio_base[[i]] <- data_read
}
data_ioratio_base <- rbindlist(data_ioratio_base)

## sensitivity data (only for 2050)
file_ioratio_sens <- list.files(pattern="Year38+.*IOpct.csv", path = paste0(base_path, "/Health/IAQData/SensAnalysisIAQData/"))
data_ioratio_sens <- list()
for (i in 1:length(file_ioratio_sens)){
  sens_label <- str_remove_all(file_ioratio_sens[i], "Year38Scale")
  sens_label <- as.integer(str_remove_all(sens_label, "IOpct.csv"))
  
  data_read <- fread(paste0(base_path, "/Health/IAQData/SensAnalysisIAQData/", file_ioratio_sens[i]))
  names(data_read) <- c("Decile",
                        "ref_cir_indoor", "ref_cir_outdoor", "ref_nocir_indoor", "ref_nocir_outdoor",
                        "int_cir_indoor", "int_cir_outdoor", "int_nocir_indoor", "int_nocir_outdoor",
                        "opt_cir_indoor", "opt_cir_outdoor", "opt_nocir_indoor", "opt_nocir_outdoor")
  data_read[, Year := 2050]
  data_read[, case := "sens"]
  data_read[, sens_level := sens_label]
  data_ioratio_sens[[i]] <- data_read
}
data_ioratio_sens <- rbindlist(data_ioratio_sens)

## convert to long format
data_ioratio <- rbind(data_ioratio_base, data_ioratio_sens)
data_ioratio <- melt(data_ioratio, id.vars = c("Year", "case", "sens_level", "Decile"), variable.name = "Scen", value.name = "io_ratio")

## split the first column
data_ioratio[, c("Scen", "Recirc", "variable") := tstrsplit(Scen, "_", fixed=TRUE)]


# read infiltration factor data -------------------------------------
## baseline data
file_infilt_base <- list.files(pattern="Base_+.*inf.csv", path = paste0(base_path, "/Health/IAQData/InfiltrationFactors/"))
data_infilt_base <- list()
for (i in 1:length(file_infilt_base)){
  year_label <- str_remove(file_infilt_base[i], "Base_Year")
  year_label <- as.integer(str_remove(year_label, "Finf.csv"))
  year_label <- year_label + 2012
  
  data_read <- fread(paste0(base_path, "/Health/IAQData/InfiltrationFactors/", file_infilt_base[i]))
  names(data_read) <- c("Decile",
                        "ref_cir", "ref_nocir",
                        "int_cir", "int_nocir",
                        "opt_cir", "opt_nocir")
  
  data_read[, Year := year_label]
  data_read[, case := "base"]
  data_read[, sens_level := 100]
  data_infilt_base[[i]] <- data_read
}
data_infilt_base <- rbindlist(data_infilt_base)

## sensitivity data (only for 2050)
file_infilt_sens <- list.files(pattern="Sens_Year+.*inf.csv", path = paste0(base_path, "/Health/IAQData/InfiltrationFactors/"))
data_infilt_sens <- list()
for (i in 1:length(file_infilt_sens)){
  sens_label <- str_remove_all(file_infilt_sens[i], "Sens_Year38")
  sens_label <- as.integer(str_remove_all(sens_label, "Finf.csv"))

  data_read <- fread(paste0(base_path, "/Health/IAQData/InfiltrationFactors/", file_infilt_sens[i]))
  names(data_read) <- c("Decile",
                        "ref_cir", "ref_nocir",
                        "int_cir", "int_nocir",
                        "opt_cir", "opt_nocir")
  data_read[, Year := 2050]
  data_read[, case := "sens"]
  data_read[, sens_level := sens_label]
  data_infilt_sens[[i]] <- data_read
}
data_infilt_sens <- rbindlist(data_infilt_sens)

## convert to long format
data_infilt <- rbind(data_infilt_base, data_infilt_sens)
data_infilt <- melt(data_infilt, id.vars = c("Year", "case", "sens_level", "Decile"), variable.name = "Scen", value.name = "infilt_fac")

## split the first column
data_infilt[, c("Scen", "Recirc") := tstrsplit(Scen, "_", fixed=TRUE)]


# merge PM2.5 concentration, indoor/outdoor ratio and calculate changes in pollution --------------------------------------
data_plt_inout <- merge(data_plt,
                        data_ioratio,
                        by = c("Scen", "Year", "case", "sens_level", "Decile", "Recirc"),
                        allow.cartesian=TRUE)

## calculate PM2.5 concentrations separately by indoor and outdoor origins
data_plt_inout[, plt := plt*io_ratio]
data_plt_inout[, `:=`(io_ratio = NULL)]

## calculate the changes in indoor air concentrations
data_plt_delta <- merge(data_plt_inout,
                        data_plt_inout[Scen == "ref"],
                        by = c("Year", "case", "sens_level", "Decile", "Recirc", "variable"),
                        suffixes = c("", "_ref"))

data_plt_delta[, plt_delta := plt - plt_ref]

data_plt_delta <- data_plt_delta[Scen != "ref"]
data_plt_delta[, `:=`(Scen_ref = NULL, plt = NULL, plt_ref = NULL)]

## convert long to wide
data_plt_delta <- dcast(data_plt_delta, Year + Scen + case + sens_level + Decile + Recirc ~ variable, value.var = "plt_delta")
setnames(data_plt_delta, old = c("indoor", "outdoor"), new = c("plt_in", "plt_out"))


# merge with infiltration rate data and population data --------------------------------------------------------
## infiltration rate data
data_plt_delta <- merge(data_plt_delta,
                        data_infilt,
                        by = c("Scen", "Year", "case", "sens_level", "Decile", "Recirc"),
                        allow.cartesian=TRUE)

## calculate the C-R coefficient beta adjustment factor (reference: Azimi and Stephens, 2020, Equation (5))
data_plt_delta[, adj_factor := infilt_fac*0.69 +  ## residential
                 0.49*0.18 + ## other indoor
                 1*0.076 + ## outdoor
                 0.43*0.055 ## vehicle
               ]


## deterministic values
### Case 1: homes mixed with recirculation (weighted by population)
data_plt_htype_mixed <- merge(data_plt_delta, data_recirc, by = c("Year", "Scen", "Recirc"))
data_plt_htype_mixed <- merge(data_plt_htype_mixed, data_pop_split, by = "Year", allow.cartesian=TRUE)
data_plt_htype_mixed[, pop := pop * cir.pct]
data_plt_htype_mixed[, cir.pct := NULL]
data_plt_htype_mixed[, home_type := "01_mix"]

### Case 2: all homes without recirculation
data_plt_htype_nocirc <- data_plt_delta[Recirc == "nocir"]
data_plt_htype_nocirc <- merge(data_plt_htype_nocirc, data_pop_split, by = "Year", allow.cartesian=TRUE)
data_plt_htype_nocirc[, home_type := "02_nocir"]

### Case 3: all homes with recirculation
data_plt_htype_circ <- data_plt_delta[Recirc == "cir"]
data_plt_htype_circ <- merge(data_plt_htype_circ, data_pop_split, by = "Year", allow.cartesian=TRUE)
data_plt_htype_circ[, home_type := "03_circ"]

### combine the data
data_plt_htype <- rbind(data_plt_htype_mixed, data_plt_htype_circ, data_plt_htype_nocirc)


# estimate IAQ health effects --------------------------------------------------------
## sounce: Pope et al., Lung cancer, cardiopulmonary mortality, and long-term exposure to fine particulate air pollution 
## another source: Parham Azimi & Brent Stephens, A framework for estimating the US mortality burden of fine particulate matter exposure attributable to indoor and outdoor microenvironments
## beta is calculated as log(RR)/10, where the RR is reported in the first row and last column of Table 2 in Pope et al.

## function of estimate the health effects, including 95% CI, without accounting pollution from outdoor origin
## the outputs are avoided deaths
cr_pope <- function(conc_delta, pop) {
  beta_mean <- log(1.06)/10
  beta_low <- log(1.02)/10
  beta_high <- log(1.11)/10

  incidence_mean <- -0.0074 * (exp(-beta_mean * 0.69 * conc_delta) - 1) * pop
  incidence_low <- -0.0074 * (exp(-beta_low * 0.69 * conc_delta) - 1) * pop
  incidence_high <- -0.0074 * (exp(-beta_high * 0.69 * conc_delta) - 1) * pop

  incidence <- data.table(-incidence_mean, -incidence_high, -incidence_low)
  return(incidence)
}

cr_pope_ind_out <- function(conc_indoor, conc_outdoor, adj_factor, pop) {
  ## beta from the epi literature
  beta_mean <- log(1.06)/10
  beta_low <- log(1.02)/10
  beta_high <- log(1.11)/10
  
  incidence_mean <- 0.0074 * (exp(beta_mean/adj_factor * 0.69 * conc_outdoor + beta_mean/adj_factor * 0.69 * conc_indoor) - 1) * pop
  incidence_low <-  0.0074 * (exp(beta_low/adj_factor  * 0.69 * conc_outdoor + beta_low/adj_factor  * 0.69 * conc_indoor) - 1) * pop
  incidence_high <- 0.0074 * (exp(beta_high/adj_factor * 0.69 * conc_outdoor + beta_high/adj_factor * 0.69 * conc_indoor) - 1) * pop
  
  incidence <- data.table(-incidence_mean, -incidence_high, -incidence_low)
  return(incidence)
}


## estimate regional avoided human deaths (with and without indoor/outdoor split)
data_health_indoor_rgn <- copy(data_plt_htype)

data_health_indoor_rgn[, c("ad_aggr_mean", "ad_aggr_min", "ad_aggr_max") := cr_pope(plt_in+plt_out, pop)]
data_health_indoor_rgn[, c("ad_split_mean", "ad_split_min", "ad_split_max") := cr_pope_ind_out(plt_in, plt_out, adj_factor = adj_factor, pop=pop)]

data_health_indoor_rgn <- melt(data_health_indoor_rgn,
                               id.vars = c("Year", "Scen", "Recirc", "case", "sens_level", "Decile", "RegionNum", "home_type"),
                               measure.vars = names(data_health_indoor_rgn)[names(data_health_indoor_rgn) %like% "ad_"])

## aggregate over deciles and recirculation
### negative means more deaths
data_health_indoor_rgn <- data_health_indoor_rgn[, .(value = sum(value)),
                                                 by = .(Scen, Year, case, sens_level, RegionNum, home_type, variable)]

data_health_indoor_rgn[, c("death", "cr_type", "variable") := tstrsplit(variable, "_", fixed=TRUE)]
data_health_indoor_rgn[, death := NULL]

## output regional data
data_health_indoor_rgn[Scen == "int", Scen := "s01"]
data_health_indoor_rgn[Scen == "opt", Scen := "s02"]

data_health_indoor_rgn_s03 <- data_health_indoor_rgn[Scen == "s01"]
data_health_indoor_rgn_s03[, Scen := "s03"]
data_health_indoor_rgn_s03[, `:=`(value = 0)]

data_health_indoor_rgn_s04 <- data_health_indoor_rgn[Scen == "s02"]
data_health_indoor_rgn_s04[, Scen := "s04"]

data_health_indoor_rgn <- rbind(data_health_indoor_rgn, data_health_indoor_rgn_s03, data_health_indoor_rgn_s04)

