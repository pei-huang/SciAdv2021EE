# This file reads NEMS simulation data

# Emissions data reported in NEMS runs ------------------------------------------------------------------
## data for mapping from NERC region to census region
mapping_nerc_census_raw <- read.csv("ExternalData/mapping_nerc_census.csv", header = TRUE, stringsAsFactors = FALSE)
## fips mapping
mapping_fips_state_raw <- read.csv("ExternalData/mapping_fips_state.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

## process nerc to census region mapping data weighted by electricity sales by sectors
mapping_nerc_census <- data.table(mapping_nerc_census_raw)
mapping_nerc_census <- melt(mapping_nerc_census, id.vars = names(mapping_nerc_census)[!grepl("CR", names(mapping_nerc_census))], variable.name = "RegionNum")
mapping_nerc_census[, NERC := as.numeric(gsub("NR","",mapping_nerc_census$NERC))]
mapping_nerc_census[, RegionNum := as.numeric(gsub("CR","",mapping_nerc_census$RegionNum))]

mapping_nerc_census <- mapping_nerc_census[, .(value = sum(value)), by = .(NERC,RegionNum)]
mapping_nerc_census[, proportion := value/sum(value), by = .(NERC)]

## SO2 and NOx for the power sector
data_plt_nems <- data_model_all[TableNumber == 62
                                & DaType == "emissions"
                                & SubDat %in% c("sulfur dioxide","nitrogen oxide")
                                & Geogr != "united states", c("Scen", "RegionNum","SubDat","Geogr","Year","value")]
setnames(data_plt_nems, old = "RegionNum", new = "NERC")

### merege the mapping data
data_plt_nems <- merge(data_plt_nems,
                       mapping_nerc_census[,c("RegionNum","NERC","proportion")],
                       by = "NERC",
                       all.x = T,
                       allow.cartesian = T)

### map from NERC to RegionNum
data_plt_nems <- data_plt_nems[, .(value = sum(value*proportion)), by = .(Scen,RegionNum,SubDat,Year)]

### convet from short ton to metric ton
data_plt_nems[, value := value*0.907185]

data_plt_nems[SubDat == "sulfur dioxide", SubDat := "SO2"]
data_plt_nems[SubDat == "nitrogen oxide", SubDat := "NOx"]

data_plt_nems[, Sector := "electric power"]

setnames(data_plt_nems, old = c("RegionNum", "SubDat", "Sector"), new = c("CensusID", "PLT", "SEC"))


# Emissions data from EPA NEI------------------------------------------------------------------
## Source: EPA 2014 data (https://www.epa.gov/air-emissions-inventories/2014-national-emissions-inventory-nei-data)
data_plt_nei_2014_1 <- fread("ExternalData/data_epa_nei_2014_county_1.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
data_plt_nei_2014_2 <- fread("ExternalData/data_epa_nei_2014_county_2.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
data_plt_nei_2014_3 <- fread("ExternalData/data_epa_nei_2014_county_3.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
data_plt_nei_2014_4 <- fread("ExternalData/data_epa_nei_2014_county_4.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

data_plt_nei_2014 <- rbind(data_plt_nei_2014_1, data_plt_nei_2014_2, data_plt_nei_2014_3, data_plt_nei_2014_4)
data_plt_nei_2014[, nei_year := 2014]

## Source: EPA 2017 data
data_plt_nei_2017 <- fread("ExternalData/data_epa_nei_2017_county.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
data_plt_nei_2017[, nei_year := 2017]

## combine 2014 and 2017 data
data_plt_nei <- rbind(data_plt_nei_2014, data_plt_nei_2017)

### add sector and pollutant columns
data_plt_nei[Sector %like% "Fuel Comb - Residential", SEC := "residential"]
data_plt_nei[Sector %like% "Fuel Comb - Comm/Institutional", SEC := "commercial"]
data_plt_nei[Sector %like% "Fuel Comb - Industrial Boilers", SEC := "industrial"]
data_plt_nei[Sector %like% "Fuel Comb - Electric Generation", SEC := "electric power"]
data_plt_nei[Sector %like% "Mobile", SEC := "transportation"]
data_plt_nei <- na.omit(data_plt_nei)

data_plt_nei[Pollutant %like% "Carbon Monoxide", PLT := "CO"]
data_plt_nei[Pollutant %like% "Sulfur Dioxide", PLT := "SO2"]
data_plt_nei[Pollutant %like% "Nitrogen Oxides", PLT := "NOx"]
data_plt_nei[Pollutant %like% "Volatile Organic Compounds", PLT := "VOC"]
data_plt_nei[Pollutant %like% "PM10", PLT := "PM10"]
data_plt_nei[Pollutant %like% "PM2.5", PLT := "PM2.5"]
data_plt_nei[Pollutant %like% "Ammonia", PLT := "NH3"]
data_plt_nei[Pollutant %like% "Lead", PLT := "Lead"]
data_plt_nei <- na.omit(data_plt_nei)

data_plt_nei[Sector %like% "non-Diesel", source := "gasoline"]
data_plt_nei[Sector %like% " Diesel", source := "diesel"]
data_plt_nei[Sector %like% "Locomotives", source := "diesel"]
data_plt_nei[Sector %like% "Commercial Marine Vessels", source := "diesel"]
data_plt_nei[Sector %like% "Aircraft", source := "jet fuel"]
data_plt_nei[Sector %like% "Gasoline", source := "gasoline"]

data_plt_nei[Sector %like% "Wood", source := "wood"]
data_plt_nei[Sector %like% "Natural Gas", source := "natural gas"]
data_plt_nei[Sector %like% "Oil", source := "oil"]
data_plt_nei[Sector %like% "Coal", source := "coal"]
data_plt_nei <- na.omit(data_plt_nei)

### add a column of long state name
mapping_state_census <- na.omit(unique(mapping_fips_state_raw[,c("StateShort","State","CensusID")]))
data_plt_nei <- merge(data_plt_nei, mapping_state_census, by.x = "Address", by.y = "StateShort")

### convert unit from short ton to million metric ton
data_plt_nei[, value := Emissions*0.907185/1e6]

### select relevant columns
data_plt_nei <- data_plt_nei[, .(value = sum(value)), by = .(CensusID, State, County, SEC, source, PLT, nei_year)]


# Fuel consumption data from NEMS runs ------------------------------------------------------------------
### fuel consumption from various sectors from NEMS
data_cons_pwr <- data_model_all[TableNumber == 2
                                & Sector == "electric power"
                                & Source %in% c("liquid fuels subtotal", "natural gas", "steam coal")
                                & Geogr != "united states", c("Scen","RegionNum","Sector","Source","Year","value")]
data_cons_pwr[Source == "liquid fuels subtotal", Source := "oil"]
data_cons_pwr[Source == "steam coal", Source := "coal"]

data_cons_rsd <- data_model_all[TableNumber == 2
                                & Sector == "residential"
                                & Source %in% c("liquid fuels subtotal", "natural gas", "renewable energy")
                                & Geogr != "united states", c("Scen","RegionNum","Sector","Source","Year","value")]
data_cons_rsd[Source == "liquid fuels subtotal", Source := "oil"]
data_cons_rsd[Source == "renewable energy", Source := "wood"]

data_cons_com <- data_model_all[TableNumber == 2
                                & Sector == "commercial"
                                & Source %in% c("liquid fuels subtotal", "natural gas", "coal")
                                & Geogr != "united states", c("Scen","RegionNum","Sector","Source","Year","value")]
data_cons_com[Source == "liquid fuels subtotal", Source := "oil"]

data_cons_ind <- data_model_all[TableNumber == 2
                                & Sector == "industrial"
                                & Source %in% c("liquid fuels subtotal", "natural gas subtotal", "coal subtotal")
                                & Geogr != "united states", c("Scen","RegionNum","Sector","Source","Year","value")]
data_cons_ind[Source == "liquid fuels subtotal", Source := "oil"]
data_cons_ind[Source == "natural gas subtotal", Source := "natural gas"]
data_cons_ind[Source == "coal subtotal", Source := "coal"]

data_cons_trn <- data_model_all[TableNumber == 2
                                & Sector == "transportation"
                                & Source %in% c("motor gasoline", "jet fuel", "liquid fuels subtotal")
                                & Geogr != "united states", c("Scen","RegionNum","Sector","Source","Year","value")]
data_cons_trn[Source == "liquid fuels subtotal", Source := "diesel"]
data_cons_trn[Source == "motor gasoline", Source := "gasoline"]

### merge data from various sectors
data_cons <- rbind(data_cons_pwr, data_cons_rsd, data_cons_com, data_cons_ind, data_cons_trn)

### two base years
data_cons_2014 <- data_cons[Scen == "s00" & Year == 2014,]
data_cons_2014 <- data_cons_2014[value > 0]
data_cons_2014 <- data_cons_2014[,c("RegionNum","Sector","Source","value")]

data_cons_2017 <- data_cons[Scen == "s00" & Year == 2017,]
data_cons_2017 <- data_cons_2017[value > 0]
data_cons_2017 <- data_cons_2017[,c("RegionNum","Sector","Source","value")]

### merge with base year data
data_cons <- merge(data_cons, data_cons_2014,
                   by = c("Sector","Source","RegionNum"), suffixes = c("", "_2014"), allow.cartesian=TRUE)
data_cons <- merge(data_cons, data_cons_2017,
                   by = c("Sector","Source","RegionNum"), suffixes = c("", "_2017"), allow.cartesian=TRUE)

data_cons[, scale_2014 := value/value_2014]
data_cons[, scale_2017 := value/value_2017]

data_cons <- data_cons[,c("Scen","RegionNum","Sector","Source","Year","scale_2014", "scale_2017")]


# Emissions extrapolation ------------------------------------------------------------------
data_nei_rgn <- data_plt_nei[, .(value = sum(value)),
                             by = .(CensusID, SEC, source, PLT, nei_year)]

### merge the NEI pollutant data and the NEMS consumption data
data_merge_nei <- merge(data_nei_rgn,
                        data_cons,
                        by.x = c("source","CensusID","SEC"),
                        by.y = c("Source","RegionNum","Sector"),
                        allow.cartesian = TRUE)

### calculate the scaled emissions based on 2014 and 2017 NEI data
data_merge_nei[, value := ifelse(nei_year == 2014, value*scale_2014, value*scale_2017)]

### sum over sources
data_merge_nei <- data_merge_nei[, .(value = sum(value)), by = .(Scen, CensusID, SEC, PLT, Year, nei_year)]

### remove SO2 and NOx from the electric power sector because it's directly from NEMS
data_merge_nei <- data_merge_nei[!(SEC == "electric power" & PLT %in% c("SO2", "NOx"))]

### assume 1% and 1.5% annual decrease of marginal emission of per unit of energy consumption
decay.len_2014 <- c(0, seq(0, (2050-2014), by = 1))
data_year_decay_2014 <- data.table(nei_year = 2014, Year = c(2013:2050), Decay.1 = 1-decay.len_2014*0.01, Decay.2= 1-decay.len_2014*0.015)

decay.len_2017 <- c(c(0, 0, 0, 0), seq(0, (2050-2017), by = 1))
data_year_decay_2017 <- data.table(nei_year = 2017, Year = c(2013:2050), Decay.1 = 1-decay.len_2017*0.01, Decay.2= 1-decay.len_2017*0.015)

data_year_decay <- rbind(data_year_decay_2014, data_year_decay_2017)

data_merge_nei <- merge(data_merge_nei, data_year_decay, by = c("nei_year", "Year"))
data_merge_nei[, `:=`(value.decay.1 = value*Decay.1,
                      value.decay.2 = value*Decay.2)]
data_merge_nei[, `:=`(Decay.1 = NULL, Decay.2 = NULL)]

### rbind the SO2 and NOx for the power sector from NEMS
data_plt_nems[, `:=`(value.decay.1 = value,
                     value.decay.2 = value)]
data_plt_nems[, nei_year := 2014]
data_plt_nems_2017 <- copy(data_plt_nems)
data_plt_nems_2017[, nei_year := 2017]
data_plt_nems <- rbind(data_plt_nems, data_plt_nems_2017)

data_plt_proj <- rbind(data_merge_nei, data_plt_nems)

## convert from wide to long
data_plt_proj <- melt(data_plt_proj, id.vars = c("Scen", "CensusID", "SEC", "nei_year", "Year", "PLT"), variable.name = "sensitivity")

## remove scenarios "s05", "s06"
### they are not presented for health outcomes
data_plt_proj <- data_plt_proj[!(Scen %in% scenario[c(6:7)])]

## save the projected emissions (for health effects estimations later)
fwrite(data_plt_proj, "NEMSPost/r_data_output/data_plt_proj.csv")
