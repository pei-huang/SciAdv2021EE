# This file generate tables for health effects

# Table 2A: outdoor health effects -----------------------------------------------------
### calculate national mean, min, max across the three IAM models
data_health_outdoor_delta_main <- data_health_outdoor_delta_nation[Scen %in% scenario[c(1:5)]
                                                         & nei_year == 2014
                                                         & sensitivity == "value"]

data_health_outdoor_delta_main <- data_health_outdoor_delta_main[, .(death_delta_mean = signif(mean(death_delta),2),
                                                           death_delta_min = signif(min(death_delta),2),
                                                           death_delta_max = signif(max(death_delta),2)),
                                                       by = .(Scen, Year)]

data_health_outdoor_delta_main <- melt(data_health_outdoor_delta_main, id.vars = c("Scen", "Year"))
data_health_outdoor_delta_main[, c("death", "type", "variable") := tstrsplit(variable, "_", fixed=TRUE)]
data_health_outdoor_delta_main[, `:=`(death = NULL, type = NULL)]

## main table for 2050 (with min and max)
### mean values
data_health_outdoor_delta_main_mean <- data_health_outdoor_delta_main[variable == "mean" & Year == 2050]
data_health_outdoor_delta_main_mean[, var_type := "01_mean"]

data_health_outdoor_delta_main_mean <- dcast(data_health_outdoor_delta_main_mean, var_type ~ Scen, value.var = "value")

data_health_outdoor_delta_main_mean[, `:=`(s01 = as.character(s01),
                                      s02 = as.character(s02),
                                      s03 = as.character(s03),
                                      s04 = as.character(s04))]

### min and max
data_health_outdoor_delta_main_minmax <- data_health_outdoor_delta_main[variable != "mean" & Year == 2050]
data_health_outdoor_delta_main_minmax[, var_type := "02_minmax"]

data_health_outdoor_delta_main_minmax <- dcast(data_health_outdoor_delta_main_minmax, var_type ~ Scen + variable, value.var = "value")

#### combine min and max and put them into parentheses
data_health_outdoor_delta_main_minmax[, `:=`(s01 = paste0("(", s01_min, ", ", s01_max, ")"),
                                        s02 = paste0("(", s02_min, ", ", s02_max, ")"),
                                        s03 = paste0("(", s03_min, ", ", s03_max, ")"),
                                        s04 = paste0("(", s04_min, ", ", s04_max, ")"))]
data_health_outdoor_delta_main_minmax <- data_health_outdoor_delta_main_minmax[, .(var_type, s01, s02, s03, s04)]

### combine mean, min and max
data_health_outdoor_delta_main_combine <- rbind(data_health_outdoor_delta_main_mean, data_health_outdoor_delta_main_minmax)
data_health_outdoor_delta_main_combine <- data_health_outdoor_delta_main_combine[order(var_type)]

fwrite(data_health_outdoor_delta_main_combine, "Health/r_data_output/Table_2_A_Outdoor.csv", row.names = FALSE)


# Table 2B: indoor health effects -----------------------------
data_health_indoor_nation <- data_health_indoor_rgn[, .(value = sum(value)),
                                                    by = .(Scen, Year, case, sens_level, home_type, cr_type, variable)]

data_health_indoor_nation_out <- data_health_indoor_nation[Year == 2050 & case == "base"]
data_health_indoor_nation_out[, value := as.integer(signif(value, 2))]

### mean values
data_health_indoor_nation_out_mean <- data_health_indoor_nation_out[variable == "mean"]
data_health_indoor_nation_out_mean[, var_type := "01_mean"]

data_health_indoor_nation_out_mean <- dcast(data_health_indoor_nation_out_mean, cr_type + home_type + var_type ~ Scen, value.var = "value")

data_health_indoor_nation_out_mean[, `:=`(s01 = as.character(s01),
                                          s02 = as.character(s02),
                                          s03 = as.character(s03),
                                          s04 = as.character(s04))]

### min and max
data_health_indoor_nation_out_minmax <- data_health_indoor_nation_out[variable != "mean"]
data_health_indoor_nation_out_minmax[, var_type := "02_minmax"]

data_health_indoor_nation_out_minmax <- dcast(data_health_indoor_nation_out_minmax, cr_type + home_type + var_type ~ Scen + variable, value.var = "value")

#### combine min and max and put them into parentheses
#### the signs of numbers were switched, so here we have max on the left and min on the right
data_health_indoor_nation_out_minmax[, `:=`(s01 = paste0("(", s01_min, ", ", s01_max, ")"),
                                            s02 = paste0("(", s02_min, ", ", s02_max, ")"),
                                            s03 = paste0("(", s03_min, ", ", s03_max, ")"),
                                            s04 = paste0("(", s04_min, ", ", s04_max, ")"))]
data_health_indoor_nation_out_minmax[, s03 := 0]
data_health_indoor_nation_out_minmax <- data_health_indoor_nation_out_minmax[, .(var_type, cr_type, home_type, s01, s02, s03, s04)]

### combine mean, min and max
data_health_indoor_nation_out_combine <- rbind(data_health_indoor_nation_out_mean, data_health_indoor_nation_out_minmax)
data_health_indoor_nation_out_combine <- data_health_indoor_nation_out_combine[order(cr_type, home_type, var_type)]

data_health_indoor_nation_out_combine_split <- data_health_indoor_nation_out_combine[cr_type == "split"]
fwrite(data_health_indoor_nation_out_combine_split, "Health/r_data_output/Table_2_B_Indoor.csv", row.names = FALSE)


# Table 2C: outdoor and indoor health effects -------------------------------------------------------------------
data_health_outdoor_indoor_rgn_split <- data_health_indoor_rgn[case == "base" & cr_type == "split"]
data_health_outdoor_indoor_nation <- data_health_outdoor_indoor_rgn_split[, .(value = sum(value)),
                                                                          by = .(Scen, Year, home_type, variable)]

## merge indoor and outdoor health effects
data_health_net <- merge(data_health_outdoor_delta_main,
                                            data_health_outdoor_indoor_nation,
                                            by = c("Scen", "Year", "variable"),
                                            suffixes = c("_outdoor", "_indoor"))

data_health_net[, net_health := value_outdoor + value_indoor]
data_health_net[, `:=`(value_outdoor = NULL, value_indoor = NULL)]

## output to the csv table
### mean values
data_indoor_outdoor_main_mean <- data_health_net[variable == "mean" & Year == 2050]
data_indoor_outdoor_main_mean[, net_health := signif(net_health,2)]
data_indoor_outdoor_main_mean[, var_type := "01_mean"]

data_indoor_outdoor_main_mean <- dcast(data_indoor_outdoor_main_mean, var_type + home_type~ Scen, value.var = "net_health")

data_indoor_outdoor_main_mean[, `:=`(s01 = as.character(s01),
                                     s02 = as.character(s02),
                                     s03 = as.character(s03),
                                     s04 = as.character(s04))]

### min and max
data_indoor_outdoor_main_minmax <- data_health_net[variable != "mean" & Year == 2050]
data_indoor_outdoor_main_minmax[, net_health := signif(net_health,2)]
data_indoor_outdoor_main_minmax[, var_type := "02_minmax"]

data_indoor_outdoor_main_minmax <- dcast(data_indoor_outdoor_main_minmax, var_type + home_type ~ Scen + variable, value.var = "net_health")

#### combine min and max and put them into parentheses
data_indoor_outdoor_main_minmax[, `:=`(s01 = paste0("(", s01_min, ", ", s01_max, ")"),
                                       s02 = paste0("(", s02_min, ", ", s02_max, ")"),
                                       s03 = paste0("(", s03_min, ", ", s03_max, ")"),
                                       s04 = paste0("(", s04_min, ", ", s04_max, ")"))]
data_indoor_outdoor_main_minmax <- data_indoor_outdoor_main_minmax[, .(var_type, home_type, s01, s02, s03, s04)]

### combine mean, min and max
data_indoor_outdoor_main_combine <- rbind(data_indoor_outdoor_main_mean, data_indoor_outdoor_main_minmax)
data_indoor_outdoor_main_combine <- data_indoor_outdoor_main_combine[order(home_type, var_type)]

fwrite(data_indoor_outdoor_main_combine, "Health/r_data_output/Table_2_C_Net.csv", row.names = FALSE)


# Table S3: indoor health effects (alternative specification) -----------------------------
data_health_indoor_nation_out_combine_aggr <- data_health_indoor_nation_out_combine[cr_type == "aggr"]
fwrite(data_health_indoor_nation_out_combine_aggr, "Health/r_data_output/Table_S3.csv", row.names = FALSE)

