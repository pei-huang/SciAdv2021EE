# This file generate figures for health effects

# Figure 4: map of saved lives by census divisions (2050) ------------------------------------------------------------------------------------
## outdoor mortality
data_health_outdoor_plot <- data_health_outdoor_delta[Scen %in% scenario[c(1:5)]
                                                      & nei_year == 2014
                                                      & sensitivity == "value"]
data_health_outdoor_plot <- data_health_outdoor_plot[,.(mean = mean(death_delta),
                                                        min = min(death_delta),
                                                        max = max(death_delta)),
                                                     by = .(Scen, Year, CensusID)]
data_health_outdoor_plot <- melt(data_health_outdoor_plot, id.vars = c("Scen", "Year", "CensusID"))


## indoor mortality
data_health_indoor_plot <- data_health_indoor_rgn[home_type == "01_mix"]
data_health_indoor_plot <- data_health_indoor_plot[, .(Scen, Year, RegionNum, variable, value)]

## merge indoor and outdoor mortality data
data_health_net_plot <- merge(data_health_outdoor_plot,
                              data_health_indoor_plot,
                              by.x = c("Scen", "Year", "CensusID", "variable"),
                              by.y = c("Scen", "Year", "RegionNum", "variable"))

data_health_net_plot[, value := value.x + value.y]
data_health_net_plot[, `:=`(value.x = NULL, value.y = NULL)]

## normalize to mortality rate per 100,000 residents
data_health_net_plot <- merge(data_health_net_plot,
                              data_pop,
                              by.x = c("Year", "CensusID"),
                              by.y = c("Year", "RegionNum"))

data_health_net_plot[, mort_rate := value/pop*100000]

data_health_net_plot[Scen == "s01", Scenario := paste0("(A) ", scenario_name_1[2])]
data_health_net_plot[Scen == "s02", Scenario := paste0("(B) ", scenario_name_1[3])]
data_health_net_plot[Scen == "s03", Scenario := "(C) Carbon Pricing"]
data_health_net_plot[Scen == "s04", Scenario := paste0("(D) ", scenario_name_1[3], "& Carbon Pricing")]

### main figure - map of saved lives in 2050
mapping_census_state <- fread("ExternalData/mapping_fips_state.csv", header = TRUE, strip.white = TRUE)
mapping_census_state <- unique(data.table(mapping_census_state)[, .(CensusID, State)])

data_health_census <- merge(data_health_net_plot[Year == 2050 & variable == "mean"],
                            mapping_census_state,
                            by = "CensusID",
                            allow.cartesian = TRUE)
data_health_census[, `:=`(State = tolower(State))]

## merge with map polygon data
states <- data.table(map_data("state"))
data_health_map <- merge(states,
                         data_health_census,
                         by.x = c("region"),
                         by.y = c("State"),
                         sort = FALSE,
                         allow.cartesian=TRUE,
                         all.y = TRUE)
data_health_map <- data_health_map[!(region %in% c("alaska","hawaii"))]
data_health_map <- data_health_map[order(data_health_map$order),]

## prepare shapefile data for the US census divisions
### First read in the shapefile, using the path to the shapefile and the shapefile name minus the extension as arguments
cd_shapefile <- readOGR("ExternalData/census_division_shapefiles", "cb_2018_us_division_20m")
# cd_shapefile <- read_sf("External_Data/census_division_shapefiles")

### Next the shapefile has to be converted to a data.table for use in ggplot2
shapefile_df <- fortify(cd_shapefile)
shapefile_df <- data.table(shapefile_df)

### Include only the contiguous 48 states
shapefile_df <- shapefile_df[(long > -125 & long < -65)
                             & (lat > 24 & lat < 50)]

### baseline
plot_health_map <- ggplot() +
  geom_polygon(data = data_health_map,
               aes(long, lat, group = group, fill = mort_rate),
               color = NA) + # data containing modeling results
  coord_map("albers", at0 = 45.5, lat1 = 29.5) +
  geom_polygon(data = shapefile_df,
               aes(x = long, y = lat, group = group),
               color = "grey40",
               size = 0.2,
               fill = NA) + # add a layer of census divisions
  facet_wrap(~Scenario) +
  theme(legend.position = 'right',
        plot.title = element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-2,-2,-2,-2),
        legend.key.size =  unit(0.2, "in"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        strip.text = element_text(size=8),
        legend.box = "vertical",
        legend.title=element_text(size=7),
        legend.text=element_text(size=6),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_binned(type = "viridis", name = "Avoided\npremature\nmortality\n(per 100,000\nresidents)", alpha = .5, labels = comma)

pdf("Health/r_figure_output/Figure_4.pdf", width = 6, height = 3.5)
print(plot_health_map)
dev.off()


# Figure S13: indoor and outdoor health effect over time ------------------------------------------------------------------------------------
data_health_plot_year <- copy(data_health_net)

data_health_plot_year[Scen == "s01", Scenario := scenario_name_1[2]]
data_health_plot_year[Scen == "s02", Scenario := scenario_name_1[3]]
data_health_plot_year[Scen == "s03", Scenario := "Carbon Pricing"]
data_health_plot_year[Scen == "s04", Scenario := paste(scenario_name_1[3], "Carbon Pricing", sep = " & ")]

data_health_plot_year[home_type == "01_mix", home_type_name := "(A) Current patterns of investment"]
data_health_plot_year[home_type == "02_nocir", home_type_name := "(B) No investment"]
data_health_plot_year[home_type == "03_circ", home_type_name := "(C) Investment in all homes"]
data_health_plot_year[, home_type_name := factor(home_type_name,
                                                 levels = c("(A) Current patterns of investment",
                                                            "(B) No investment",
                                                            "(C) Investment in all homes"))]

data_health_plot_year <- data_health_plot_year[Year > 2016 & variable == "mean"]

## plot in a bar chart
plot_mort_year <- ggplot(data = data_health_plot_year, mapping = aes(x = Year, y = net_health, group = Scen)) +
  geom_col(position="dodge", aes(fill = Scenario)) + 
  # geom_errorbar(mapping = aes(ymin = min, ymax = max), width = 3, position=position_dodge(width=9), size = 0.5) +
  facet_wrap(~ home_type_name) +
  ylab("Avoided Premature Deaths") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,-5,-5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=8),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(fill = guide_legend(title="", byrow = TRUE, nrow = 2, order = 1)) +
  scale_fill_npg()

pdf("Health/r_figure_output/Figure_S13.pdf", width = 6, height = 3)
print(plot_mort_year)
dev.off()



# Figure S24: indoor pollution, recirculation, and population --------------------------------------------------------
data_plt_base_plot <- data_plt[case == "base" & Year != 2016]

data_plt_base_plot[Scen == "ref", Scenario := "Reference"]
data_plt_base_plot[Scen == "int", Scenario := "Intermediate EE"]
data_plt_base_plot[Scen == "opt", Scenario := "Optimistic EE"]
data_plt_base_plot[, Scenario := factor(Scenario, levels = c("Reference", "Intermediate EE", "Optimistic EE"))]

data_plt_base_plot[Recirc == "cir", Recirc.Ind := "Yes"]
data_plt_base_plot[Recirc == "nocir", Recirc.Ind := "No"]

data_plt_base_plot[, Recirc.Ind := factor(Recirc.Ind, levels = c("Yes", "No"))]

data_plt_base_plot[, Year := as.factor(Year)]
levels(data_plt_base_plot$Year) <- c("(A) 2020", "(B) 2030", "(C) 2040", "(D) 2050")

plot_plt <- ggplot(data = data_plt_base_plot, mapping = aes(x = Decile, y = plt)) +
  geom_line(aes(color = Scenario, linetype = Recirc.Ind)) + 
  facet_wrap(~ Year, nrow = 2) +
  ylab(bquote("Indoor PM"[2.5]*" Concentration ("*mu*"g/m"^3*")")) +
  xlab("Indoor Emission Decile") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        plot.title = element_blank(),
        strip.text = element_text(size=9)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Recirculation/Filtration", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("Health/r_figure_output/Figure_S24.pdf", width = 5, height = 5)
print(plot_plt)
dev.off()


# Figure S26: plot sensitivity analysis of infiltration rate (currently no data)--------------------------------------------------------
data_plot_sens_infilt <- data_health_indoor_nation[case == "sens"
                                                   & cr_type == "split"
                                                   & Scen %in% c("s01", "s02")]
data_plot_sens_infilt[, sens_level := sens_level/100]

data_plot_sens_infilt <- dcast(data_plot_sens_infilt, Scen + sens_level + home_type ~ variable, value.var = "value")

data_plot_sens_infilt[Scen == "s01", Scenario := "Intermediate EE"]
data_plot_sens_infilt[Scen == "s02", Scenario := "Optimistic EE"]
data_plot_sens_infilt[, Scenario := factor(Scenario, levels = c("Intermediate EE", "Optimistic EE"))]

data_plot_sens_infilt[home_type == "01_mix", home_type_name := "(A) Current patterns of investment"]
data_plot_sens_infilt[home_type == "02_nocir", home_type_name := "(B) No investment"]
data_plot_sens_infilt[home_type == "03_circ", home_type_name := "(C) Investment in all homes"]
data_plot_sens_infilt[, home_type_name := factor(home_type_name,
                                                 levels = c("(A) Current patterns of investment",
                                                            "(B) No investment",
                                                            "(C) Investment in all homes"))]

plot_sens_infilt <- ggplot(data = data_plot_sens_infilt, mapping = aes(x = sens_level, y = mean)) +
  geom_ribbon(aes(ymin = min, ymax = max, fill = Scenario), alpha = 0.4) +
  geom_line(aes(color = Scenario)) +
  facet_wrap(~ home_type_name, nrow = 1) +
  scale_x_reverse() +
  ylab("Avoided premature deaths") +
  xlab(bquote("E"[Inf]*" vs. BSEI proportionality factor")) +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,-5,-5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        plot.title = element_blank(),
        strip.text = element_text(size=7)) +
  scale_color_npg() + scale_fill_npg()

pdf("Health/r_figure_output/Figure_S26.pdf", width = 6, height = 3)
print(plot_sens_infilt)
dev.off()

