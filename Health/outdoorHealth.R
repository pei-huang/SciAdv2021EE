# This file estimates the health effects of outdoor emissions

# Marginal damage data: AP3 -----------------------------------------------------------------------
## source: https://public.tepper.cmu.edu/nmuller/APModel.aspx
## marginal damages for various sources ($ per short ton)
## the data is based on the Nick Muller's AP3 (2019)
## VSL is assumed $9,186,210

## county names
county_fips <- fread("ExternalData/AP3/fips_apeep_updated.csv")
county_fips <- county_fips[, .(fips)]

## facility names
tall1_fac <- fread("ExternalData/AP3/tall_facilities_ap3.csv")
tall1_fac <- tall1_fac[, .(EIS, FIPS)]
tall1_fac[, id := seq_len(.N), by = .(EIS, FIPS)]
setnames(tall1_fac, old = 'FIPS', new = 'fips')

tall2_fac <- fread("ExternalData/AP3/tall2_facilities_ap3.csv")
tall2_fac <- tall2_fac[, .(EIS, FIPS)]
tall2_fac[, id := seq_len(.N), by = .(EIS, FIPS)]
setnames(tall2_fac, old = 'FIPS', new = 'fips')

## county emission data
### areas sources
data_emi_area <- fread("ExternalData/AP3/area_sources_2014.csv")
data_emi_area[, V6 := V6 + V7]
data_emi_area <- data_emi_area[, .(V1, V2, V4, V5, V6)]
data_emi_area <- cbind(county_fips, data_emi_area)
data_emi_area[, `:=`(Source = "area",
                     EIS = 9999999,
                     id = 1)]

### low point sources (effective height < 250m)
data_emi_low <- fread("ExternalData/AP3/low_2014.csv")
data_emi_low <- data_emi_low[, .(V1, V2, V4, V5, V6)]
data_emi_low <- cbind(county_fips, data_emi_low)
data_emi_low[, `:=`(Source = "low",
                    EIS = 9999999,
                    id = 1)]

### medium point sources (effective height 250m – 500m)
data_emi_med <- fread("ExternalData/AP3/medium_2014.csv")
data_emi_med <- data_emi_med[, .(V1, V2, V4, V5, V6)]
data_emi_med <- cbind(county_fips, data_emi_med)
data_emi_med[, `:=`(Source = "medium",
                    EIS = 9999999,
                    id = 1)]

### tall point sources (effective height >500m)
data_emi_tall1 <- fread("ExternalData/AP3/tall_2014.csv")
data_emi_tall1 <- data_emi_tall1[, .(V1, V2, V4, V5, V6)]
data_emi_tall1 <- cbind(tall1_fac, data_emi_tall1)
data_emi_tall1 <- na.omit(data_emi_tall1)
data_emi_tall1[, Source := "tall1"]

data_emi_tall2 <- fread("ExternalData/AP3/tall2_2014.csv")
data_emi_tall2 <- data_emi_tall2[, .(V1, V2, V4, V5, V6)]
data_emi_tall2 <- cbind(tall2_fac, data_emi_tall2)
data_emi_tall2 <- na.omit(data_emi_tall2)
data_emi_tall2[, Source := "tall2"]

data_emi_ap3 <- rbind(data_emi_area, data_emi_low, data_emi_med, data_emi_tall1, data_emi_tall2)
data_emi_ap3 <- melt.data.table(data_emi_ap3, id.vars = c("fips", "EIS", "Source", "id"), variable.name = "PLT", value.name = "emission")
levels(data_emi_ap3$PLT) <- c("NH3", "NOx", "PM2.5", "SO2", "VOC")

### areas sources
data_md_ap3_area <- fread("ExternalData/AP3/md_A_2014_DR-Krewski_VRMR-9186210.csv")
data_md_ap3_area <- cbind(county_fips, data_md_ap3_area)
data_md_ap3_area[, `:=`(Source = "area",
                        EIS = 9999999,
                        id = 1)]

### low point sources (effective height < 250m)
data_md_ap3_low <- fread("ExternalData/AP3/md_L_2014_DR-Krewski_VRMR-9186210.csv")
data_md_ap3_low <- cbind(county_fips, data_md_ap3_low)
data_md_ap3_low[, `:=`(Source = "low",
                       EIS = 9999999,
                       id = 1)]

### medium point sources (effective height 250m – 500m)
data_md_ap3_med <- fread("ExternalData/AP3/md_M_2014_DR-Krewski_VRMR-9186210.csv")
data_md_ap3_med <- cbind(county_fips, data_md_ap3_med)
data_md_ap3_med[, `:=`(Source = "medium",
                       EIS = 9999999,
                       id = 1)]

### tall point sources (effective height >500m)
data_md_ap3_tall1 <- fread("ExternalData/AP3/md_T_2014_DR-Krewski_VRMR-9186210.csv")
data_md_ap3_tall1 <- cbind(tall1_fac, data_md_ap3_tall1)
data_md_ap3_tall1 <- na.omit(data_md_ap3_tall1)
data_md_ap3_tall1[, Source := "tall1"]

data_md_ap3_tall2 <- fread("ExternalData/AP3/md_T2_2014_DR-Krewski_VRMR-9186210.csv")
data_md_ap3_tall2 <- cbind(tall2_fac, data_md_ap3_tall2)
data_md_ap3_tall2 <- na.omit(data_md_ap3_tall2)
data_md_ap3_tall2[, Source := "tall2"]

data_md_ap3 <- rbind(data_md_ap3_area, data_md_ap3_low, data_md_ap3_med, data_md_ap3_tall1, data_md_ap3_tall2)
data_md_ap3 <- melt.data.table(data_md_ap3, id.vars = c("fips", "EIS", "Source", "id"), variable.name = "PLT")
levels(data_md_ap3$PLT) <- c("NH3", "NOx", "PM2.5", "SO2", "VOC")

## convert from $/short ton to $/metric ton
data_md_ap3[, value := value*1.10231]

## calculate the number of deaths by dividing it with VSL
data_md_ap3[, death := value/9186210]
data_md_ap3[, value := NULL]

# merge with emissions data
data_md_ap3 <- merge(data_md_ap3, data_emi_ap3, by = c("fips", "EIS", "Source", "PLT", "id"))
# data_dup <- data_md_ap3[(duplicated(data_md_ap3[,c("fips", "EIS", "Source", "PLT", "id"), with = F])
#                           | duplicated(data_md_ap3[,c("fips", "EIS", "Source", "PLT", "id"), with = F], fromLast = TRUE)),]

## process the marginal damage data and map them into census regions level
data_md_ap3[, fips := str_pad(fips, 5, pad = "0")]
data_md_ap3[, StateID := as.integer(substr(fips, 1, 2))]

## fips to county mapping data
mapping_fips_state <- fread("ExternalData/mapping_fips_state.csv", header = TRUE, strip.white = TRUE)
mapping_fips_state <- unique(mapping_fips_state[,c("StateID","CensusID")])

data_md_ap3 <- merge(data_md_ap3, mapping_fips_state, by = "StateID", all.x = TRUE)
data_md_ap3 <- data_md_ap3[, .(death = weighted.mean(death, emission)),
                           by = .(CensusID, PLT)]

data_md_ap3[, model := "AP3"]


# Marginal damage data: EASIUR and InMAP -----------------------------------------------------------------------
## download from CACES website 2018-11-27 (https://www.caces.us/data)
## unit: USD/metric ton of emissions
## Assumed Value of statistical life: $6,299,143

## read EASIUR and InMAP marginal damage data
data_md_other_ground <- fread("ExternalData/EASIUR_InMAP/VSL_6299143_ground.csv")
data_md_other_elev <- fread("ExternalData/EASIUR_InMAP/VSL_6299143_elevated.csv")

data_md_other <- rbind(data_md_other_ground, data_md_other_elev)

## select only annual estimates
data_md_other <- data_md_other[season == "annual"]

## remove results for AP2
data_md_other <- data_md_other[model != "AP2"]

## calculate the number of deaths by dividing it with VSL
data_md_other[, death := damage/6299143]
data_md_other[, damage := NULL]

## change pollutant names
data_md_other[pollutant == "pm25", PLT := "PM2.5"]
data_md_other[pollutant == "so2", PLT := "SO2"]
data_md_other[pollutant == "nox", PLT := "NOx"]
data_md_other[pollutant == "nh3", PLT := "NH3"]
data_md_other[pollutant == "voc", PLT := "VOC"]

## merge data_md_easiur with emissions data (from AP3)
data_emi_aggr <- copy(data_emi_ap3)
data_emi_aggr[Source == "area", Source := "ground level"]
data_emi_aggr[Source %in% c("low", "medium", "tall1", "tall2"), Source := "high stack"]
data_emi_aggr <- data_emi_aggr[, .(emission = sum(emission)), by = .(fips, Source, PLT)]

data_md_other <- merge(data_md_other, data_emi_aggr, by.x = c("fips", "elevated", "PLT"), by.y = c("fips", "Source", "PLT"))

## map into census regions level
data_md_other[, fips := str_pad(fips, 5, pad = "0")]
data_md_other[, StateID := as.integer(substr(fips, 1, 2))]

## fips to county mapping data
data_md_other <- merge(data_md_other, mapping_fips_state, by = "StateID", all.x = TRUE)
data_md_other <- data_md_other[, .(death = weighted.mean(death, emission)),
                               by = .(CensusID, model, PLT)]

## merge AP3, EASIUR, InMAP model data
data_md <- rbind(data_md_other, data_md_ap3)


# Calculate health benefits and changes in health benefits: avoided human deaths -----------------------------------------------------------------------
## projected emissions data
data_plt_proj <- fread("NEMSPost/r_data_output/data_plt_proj.csv")

## merge marginal damage data with emissions data
data_health_outdoor <- merge(data_plt_proj, data_md, by = c("CensusID", "PLT"), allow.cartesian=TRUE)

## calculate premature deaths (individuals)
data_health_outdoor[, death := value*death*1e6]

## sum over sectors and pollutants
data_health_outdoor <- data_health_outdoor[, .(death = sum(death)),
                                           by = .(Scen, CensusID, Year, nei_year, model, sensitivity)]

## calculate changes in health benefits compared to the reference (2 significant digits)
### the unit is avoided human deaths
data_health_outdoor_delta <- merge(data_health_outdoor,
                                   data_health_outdoor[Scen == scenario[1]],
                                   by = c("CensusID", "Year", "nei_year", "model", "sensitivity"),
                                   suffixes = c("", ".s00"))

data_health_outdoor_delta <- merge(data_health_outdoor_delta,
                                   data_health_outdoor[Scen == scenario[8]],
                                   by = c("CensusID", "Year", "nei_year", "model", "sensitivity"),
                                   suffixes = c("", ".s08"))

data_health_outdoor_delta[, death_delta := ifelse(Scen %in% scenario[c(1:5)], -(death - death.s00), -(death - death.s08))]

data_health_outdoor_delta <- data_health_outdoor_delta[!(Scen %in% scenario[c(1,8)])]
data_health_outdoor_delta <- data_health_outdoor_delta[, `:=`(Scen.s00 = NULL, death.s00 = NULL,
                                                              Scen.s08 = NULL, death.s08 = NULL,
                                                              death = NULL)]

### add region names
data_health_outdoor_delta[data_health_outdoor_delta$CensusID == 1, RNAME := "new england"]
data_health_outdoor_delta[data_health_outdoor_delta$CensusID == 2, RNAME := "middle atlantic"]
data_health_outdoor_delta[data_health_outdoor_delta$CensusID == 3, RNAME := "east north central"]
data_health_outdoor_delta[data_health_outdoor_delta$CensusID == 4, RNAME := "west north central"]
data_health_outdoor_delta[data_health_outdoor_delta$CensusID == 5, RNAME := "south atlantic"]
data_health_outdoor_delta[data_health_outdoor_delta$CensusID == 6, RNAME := "east south central"]
data_health_outdoor_delta[data_health_outdoor_delta$CensusID == 7, RNAME := "west south central"]
data_health_outdoor_delta[data_health_outdoor_delta$CensusID == 8, RNAME := "mountain"]
data_health_outdoor_delta[data_health_outdoor_delta$CensusID == 9, RNAME := "pacific"]


## national totals
data_health_outdoor_delta_nation <- data_health_outdoor_delta[, .(death_delta = sum(death_delta)),
                                                              by = .(Scen, Year, nei_year, model, sensitivity)]

