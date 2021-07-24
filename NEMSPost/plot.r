# This file plots data from NEMS simulations and postprocessing

# Figure 1: energy consumption and co2 emissions (building sectors)--------------------------------------------------------------------------------------
## note: emissions from electricity generation is accounted to each building sector
## energy consumption
data_consum_all <- data_model_main[TableNumber == 2
                                   & DaType == "consumption"
                                   & Sector %in% c("residential", "commercial", "industrial")
                                   & Source %in% c("total")
                                   & Geogr == "united states", ]
data_consum_all <- data_consum_all[, .(value = sum(value)),
                                   by = .(Scen, Scenario.1, Scenario.2, Scenario.3, Year)]

plot_consum_building <- ggplot(data = data_consum_all, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  ylab("Energy Consumption (quads)") +
  theme(legend.box = "vertical",
        legend.position = c(.15, .01),
        legend.justification = c("left", "bottom"),
        legend.box.just = "left",
        legend.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = 7),
        legend.title=element_text(size=8),
        legend.key.size = unit(0.4, "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 2, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


## carbon emissions
data_co2_all <- data_co2_sec_all[Fuel == "Total"
                                 & Sector %in% c("Commercial", "Industrial", "Residential")]

data_co2_all <- data_co2_all[, .(value = sum(value)),
                             by = .(Scen, Scenario.1, Scenario.2, Scenario.3, Year)]

plot_co2_building <- ggplot(data = data_co2_all, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2), show.legend = FALSE) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  ylab("Carbon Dioxide Emissions (MMT)") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 2, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

## output results
plot_combine <- ggarrange(plot_consum_building,
                          plot_co2_building,
                          labels = c("(A)", "(B)"),
                          font.label = list(size = 10, color = "black", face = "plain"),
                          nrow = 1)

pdf("NEMSPost/r_figure_output/Figure_1.pdf", width = 6, height = 2.8)
print(plot_combine)
dev.off()


# Figure 2: pollution results over time --------------------------------------------------------------------------------------
## prepare plot data
data_plt_proj_plot <- data_plt_proj[Scen %in% scenario[c(1:5)]]

### add proper scenario names
for(i in 1:length(unique(data_plt_proj$Scen))){
  data_plt_proj_plot[Scen == unique(data_plt_proj$Scen)[i], `:=`(Scenario.1 = scenario_name_1[i],
                                                                 Scenario.2 = scenario_name_2[i],
                                                                 Scenario.3 = scenario_name_3[i])]
}

### add proper names for pollutants for printing
data_plt_proj_plot[data_plt_proj_plot$PLT == "PM2.5", PLT.Name := "(A)~PM[2.5]"]
data_plt_proj_plot[data_plt_proj_plot$PLT == "PM10", PLT.Name := "(B)~PM[10]"]
data_plt_proj_plot[data_plt_proj_plot$PLT == "NOx", PLT.Name := "(C)~NO[x]"]
data_plt_proj_plot[data_plt_proj_plot$PLT == "SO2", PLT.Name := "(D)~SO[2]"]
data_plt_proj_plot[data_plt_proj_plot$PLT == "VOC", PLT.Name := "(E)~VOC"]
data_plt_proj_plot[data_plt_proj_plot$PLT == "NH3", PLT.Name := "(F)~NH[3]"]
data_plt_proj_plot[data_plt_proj_plot$PLT == "CO", PLT.Name := "(G)~CO"]
data_plt_proj_plot[data_plt_proj_plot$PLT == "Lead", PLT.Name := "(H)~Lead"]

### reorder pollutants
data_plt_proj_plot[, PLT := factor(PLT, levels = c("PM2.5", "PM10", "NOx", "SO2", "VOC", "NH3", "CO", "Lead"))]
data_plt_proj_plot[, PLT.Name := factor(PLT.Name)]

### aggregate the data
data_plt_proj_plot_sec <- data_plt_proj_plot[, .(value = sum(value)),
                                             by = .(Scen, Scenario.1, Scenario.2, Scenario.3,
                                                    Year, nei_year, SEC, PLT, PLT.Name, sensitivity)]
data_plt_proj_plot_nation <- data_plt_proj_plot[, .(value = sum(value)),
                                                by = .(Scen, Scenario.1, Scenario.2, Scenario.3,
                                                       Year, nei_year, PLT, PLT.Name, sensitivity)]

## print pollution in the US (over time)
### main figure
data_plt_proj_plot_main <- data_plt_proj_plot_nation[nei_year == 2014
                                                     & sensitivity == "value"
                                                     & PLT %in% c("PM2.5", "PM10", "NOx", "SO2", "VOC", "NH3", "CO")]

plot_us_total_main <- ggplot(data = data_plt_proj_plot_main, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
  ylab("Pollutant emissions (MMT)") +
  ggtitle("all sectors: united states") +
  theme(legend.box = "vertical",
        legend.position = c(.4, .01),
        legend.justification = c("left", "bottom"),
        legend.box.just = "left",
        legend.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = 8),
        legend.title=element_text(size=9),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 2, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_2.pdf", width = 6, height = 5.8)
print(plot_us_total_main)
dev.off()


# Figure S1: energy consumption and co2 emissions (economy-wide)--------------------------------------------------------------------------------------
## energy consumption
data_consum_all <- data_model_main[TableNumber == 2
                                   & DaType == "consumption"
                                   & Sector == "all sectors"
                                   & Source %in% c("total")
                                   & Geogr == "united states", ]

plot_consum_all <- ggplot(data = data_consum_all, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  ylab("Energy Consumption (quads)") +
  theme(legend.box = "vertical",
        legend.position = c(.15, .01),
        legend.justification = c("left", "bottom"),
        legend.box.just = "left",
        legend.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = 7),
        legend.title=element_text(size=8),
        legend.key.size = unit(0.4, "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 2, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


## carbon emissions
data_co2_all <- data_model_main[TableNumber == 2
                                & DaType == "emissions"
                                & Geogr == "united states", ]

plot_co2_all <- ggplot(data = data_co2_all, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2), show.legend = FALSE) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  ylab("Carbon Dioxide Emissions (MMT)") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 2, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

## output results
plot_combine <- ggarrange(plot_consum_all,
                          plot_co2_all,
                          labels = c("(A)", "(B)"),
                          font.label = list(size = 10, color = "black", face = "plain"),
                          nrow = 1)

pdf("NEMSPost/r_figure_output/Figure_S1.pdf", width = 6, height = 2.8)
print(plot_combine)
dev.off()


# Figure S2: energy consumption by sector --------------------------------------------------------------------------------------
data_consum_sec <- data_model_main[TableNumber == 2
                                   & DaType == "consumption"
                                   & !(Sector %in% c("total energy", "all sectors"))
                                   & Source %in% c("total")
                                   & Geogr == "united states", ]

data_consum_sec <- data_consum_sec[Sector != "other"]

data_consum_sec[, Sector := factor(Sector, levels = c("residential", "commercial", "industrial",
                                                      "electric power", "transportation"))]
levels(data_consum_sec$Sector) <- c("(A) Residential", "(B) Commercial", "(C) Industrial",
                                    "(D) Electric Power", "(E) Transportation")

plot_consum_sec <- ggplot(data = data_consum_sec, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector, scale = "free") +
  ylab("Energy consumption (quads)") +
  ggtitle("Energy Consumption by sector") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S2.pdf", width = 6, height = 5)
print(plot_consum_sec)
dev.off()


# Figure S3: energy consumption by end use: residential-------------------------------------------------------------------------------------
data_res_con <- data_model_main[TableNumber == 4
                                & DaType == "consumption"
                                & SubSec != ""
                                & Source == "total energy use by end use"
                                & Geogr == "united states", ]

data_res_con <- data_res_con[SubSec != "other uses"]
data_res_con[, SubSec := str_to_title(SubSec)]

data_res_con[, SubSec := as.factor(SubSec)]
levels(data_res_con$SubSec) <- c("(A) Clothes Dryers", "(B) Clothes Washers", "(C) Computers",
                                 "(D) Cooking", "(E) Dishwashers", "(F) Fans And Pumps",
                                 "(G) Freezers", "(H) Lighting", "(I) Refrigeration",
                                 "(J) Space Cooling", "(K) Space Heating", "(L) Televisions",
                                 "(M) Water Heating")

plot_res_con <- ggplot(data = data_res_con, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ SubSec) +
  ylab("Energy consumption (quads)") +
  ggtitle("Residential : Total Energy Use by End Use") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S3.pdf", width = 7, height = 7)
print(plot_res_con)
dev.off()


# Figure S4: energy consumption by end use: commercial-------------------------------------------------------------------------------------
data_com_con <- data_model_main[TableNumber == 5
                                & DaType == "consumption"
                                & SubSrc != ""
                                & Source == "total energy use by end use"
                                & Geogr == "united states", ]

data_com_con <- data_com_con[SubSrc != "other uses"]
data_com_con[, SubSrc := str_to_title(SubSrc)]

data_com_con[, SubSrc := as.factor(SubSrc)]
levels(data_com_con$SubSrc) <- c("(A) Cooking", "(B) Lighting", "(C) Office Equipment (Non-PC)",
                                 "(D) Office Equipment (PC)", "(E) Refrigeration", "(F) Space Cooling",
                                 "(G) Space Heating", "(H) Ventilation", "(I) Water Heating")

plot_com_con <- ggplot(data = data_com_con, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ SubSrc) +
  ylab("Energy consumption (quads)") +
  ggtitle("Commercial: Total Energy Use by End Use") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S4.pdf", width = 6, height = 6)
print(plot_com_con)
dev.off()


# Figure S5: energy consumption by fuels: total--------------------------------------------------------------------------------------
data_cons_fuel <- data_model_main[TableNumber == 2
                                  & DaType == "consumption"
                                  & Sector == "total energy"
                                  & Source %in% c("liquid fuels subtotal",
                                                  "natural gas subtotal",
                                                  "coal subtotal",
                                                  "nuclear",
                                                  "biofuels heat and coproducts",
                                                  "renewable energy")
                                  & Geogr == "united states", ]

data_cons_fuel[, Source := as.factor(Source)]
levels(data_cons_fuel$Source) <- c("(A) Biofuels",
                                   "(B) Coal",
                                   "(C) Liquid Fuels",
                                   "(D) Natural Gas",
                                   "(E) Nuclear",
                                   "(F) Renewables")

plot_cons_fuel <- ggplot(data = data_cons_fuel, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source) +
  ylab("Energy consumption (quads)") +
  ggtitle("Energy Consumption by fuel type") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S5.pdf", width = 6, height = 5)
print(plot_cons_fuel)
dev.off()


# Figure S6: carbon emissions by sector-------------------------------------------------------------------------------------
data_co2_sec <- data_model_main[TableNumber == 17
                                & Sector %in% c("residential",
                                                "commercial",
                                                "industrial",
                                                "transportation",
                                                "electric power")
                                & Source == ""
                                & Geogr == "united states", ]

data_co2_sec[, Sector := str_to_title(Sector)]

data_co2_sec[, Sector := as.factor(Sector)]
levels(data_co2_sec$Sector) <- c("(A) Commercial", "(B) Electric Power", "(C) Industrial",
                                 "(D) Residential (PC)", "(E) Transportation")

plot_co2_sec <- ggplot(data = data_co2_sec, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector) +
  ylab("Carbon emissions (MMT)") +
  ggtitle("Carbon Dioxide by Sector") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


pdf("NEMSPost/r_figure_output/Figure_S6.pdf", width = 6, height = 5)
print(plot_co2_sec)
dev.off()


# Figure S7: pollution results over time (only for lead) --------------------------------------------------------------------------------------
data_plt_proj_plot_si <- data_plt_proj_plot_nation[nei_year == 2014
                                                   & sensitivity == "value"
                                                   & PLT %in% c("Lead")]

plot_us_total_si <- ggplot(data = data_plt_proj_plot_si, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  # facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
  ylab("Lead emissions (MMT)") +
  ggtitle("all sectors: united states") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S7.pdf", width = 5, height = 5)
print(plot_us_total_si)
dev.off()


# Figure S8: pollution results over time in residential ---------------------------------------
data_plt_proj_plot_res <- data_plt_proj_plot_sec[SEC == "residential"
                                                 & nei_year == 2014
                                                 & sensitivity == "value"]

plot_plt_res <- ggplot(data = data_plt_proj_plot_res, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
  ylab("Pollutant emissions (MMT)") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) + 
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S8.pdf", width = 6, height = 6)
print(plot_plt_res)
dev.off()


# Figure S9: pollution results over time in commercial ---------------------------------------
data_plt_proj_plot_com <- data_plt_proj_plot_sec[SEC == "commercial"
                                                 & nei_year == 2014
                                                 & sensitivity == "value"]

plot_plt_com <- ggplot(data = data_plt_proj_plot_com, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
  ylab("Pollutant emissions (MMT)") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) + 
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S9.pdf", width = 6, height = 6)
print(plot_plt_com)
dev.off()


# Figure S10: pollution results over time in industrial ---------------------------------------
data_plt_proj_plot_ind <- data_plt_proj_plot_sec[SEC == "industrial"
                                                 & nei_year == 2014
                                                 & sensitivity == "value"]

#"electric power" "industrial"     "commercial"     "residential"   

plot_plt_ind <- ggplot(data = data_plt_proj_plot_ind, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
  ylab("Pollutant emissions (MMT)") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) + 
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S10.pdf", width = 6, height = 6)
print(plot_plt_ind)
dev.off()


# Figure S11: pollution results over time in electric power ---------------------------------------
data_plt_proj_plot_ele <- data_plt_proj_plot_sec[SEC == "electric power"
                                                 & nei_year == 2014
                                                 & sensitivity == "value"]

plot_plt_ele <- ggplot(data = data_plt_proj_plot_ele, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
  ylab("Pollutant emissions (MMT)") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) + 
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S11.pdf", width = 6, height = 6)
print(plot_plt_ele)
dev.off()


# Figure S12: pollution in census region level in map --------------------------------------------------------------------------------------
## prepare data for calculating pollution changes in a year
data_plt_census <- data_plt_proj_plot[Year == 2050
                                      & nei_year == 2014
                                      & sensitivity == "value"]

data_plt_census <- data_plt_census[, .(value = sum(value)),
                                   by = .(Scen, CensusID, PLT, PLT.Name)]

data_plt_census <- merge(data_plt_census,
                         data_plt_census[Scen == scenario[1]],
                         by = c("CensusID", "PLT", "PLT.Name"),
                         suffixes = c("", "_s00"))

data_plt_census[, change := value - value_s00]
data_plt_census[, change.pct := (value - value_s00)/value_s00*100]

data_plt_census <- data_plt_census[Scen != scenario[1]]
data_plt_census <- data_plt_census[!is.na(change.pct)]

data_plt_census <- data_plt_census[, `:=`(value = NULL, value_s00 = NULL, Scen_s00 = NULL)]

mapping_census_state <- unique(data.table(mapping_fips_state_raw)[, .(CensusID, State)])

data_plt_census <- merge(data_plt_census,
                         mapping_census_state,
                         by = "CensusID",
                         allow.cartesian = TRUE)
data_plt_census[, `:=`(State = tolower(State))]

## merge with map polygon data
states <- data.table(map_data("state"))
data_plt_map <- merge(states,
                      data_plt_census,
                      by.x = c("region"),
                      by.y = c("State"),
                      sort = FALSE,
                      allow.cartesian=TRUE,
                      all.y = TRUE)
data_plt_map <- data_plt_map[!(region %in% c("alaska","hawaii"))]
data_plt_map <- data_plt_map[order(data_plt_map$order),]

### scale unit for lead from million metric tons to metric tons
data_plt_map[PLT == "Lead", change := change*1e6]

### add full scenario names
data_plt_map[Scen == scenario[2], Scenario.Label := scenario_name_1[2]]
data_plt_map[Scen == scenario[3], Scenario.Label := scenario_name_1[3]]
data_plt_map[Scen == scenario[4], Scenario.Label := "Carbon Pricing"]
data_plt_map[Scen == scenario[5], Scenario.Label := paste(scenario_name_1[3], "Carbon Pricing", sep = " & ")]

### prepare shapefile data for the US census divisions
#### First read in the shapefile, using the path to the shapefile and the shapefile name minus the extension as arguments
cd_shapefile <- readOGR("ExternalData/census_division_shapefiles", "cb_2018_us_division_20m")

#### Next the shapefile has to be converted to a data.table for use in ggplot2
shapefile_df <- fortify(cd_shapefile)
shapefile_df <- data.table(shapefile_df)

#### Include only the contiguous 48 states
shapefile_df <- shapefile_df[(long > -125 & long < -65)
                             & (lat > 24 & lat < 50)]

for(i in 1:length(unique(data_plt_map[,PLT]))){
  plt_id <- unique(data_plt_map[,PLT.Name])[i]
  data_plt_map_sub <- data_plt_map[PLT.Name == plt_id]
  
  ### absolute change
  plot_map_abs <- ggplot() +
    geom_polygon(data = data_plt_map_sub,
                 aes(long, lat, group = group, fill = change),
                 color = NA) + # data containing modeling results
    coord_map("albers", at0 = 45.5, lat1 = 29.5) +
    labs(title = paste0(toupper(letters)[i], ": ", unique(data_plt_map_sub[,PLT]))) +
    geom_polygon(data = shapefile_df,
                 aes(x = long, y = lat, group = group),
                 color = "grey40",
                 size = 0.2,
                 fill = NA) + # add a layer of census divisions
    facet_wrap(~Scenario.Label, nrow = 1) +
    theme(legend.position = 'right',
          plot.title = element_text(size=8),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-2,-2,-2,-2),
          legend.key.size =  unit(0.1, "in"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          strip.text = element_text(size=6.5),
          legend.box = "vertical",
          legend.title=element_text(size=7),
          legend.text=element_text(size=6),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) + 
    scale_fill_binned(name = ifelse(plt_id == "Lead", "Ton", "MMT"),
                      type = "viridis", alpha = .5,
                      labels = scales::number_format(accuracy = 0.001))
  assign(paste0("plot_map_abs_", i), plot_map_abs)
}

## output to PDF
pdf("NEMSPost/r_figure_output/Figure_S12.pdf", width = 6, height = 5)
grid.arrange(plot_map_abs_1, plot_map_abs_2, plot_map_abs_3, plot_map_abs_4, nrow=4)
grid.arrange(plot_map_abs_5, plot_map_abs_6, plot_map_abs_7, plot_map_abs_8, nrow=4)
dev.off()


# Figure S14: carbon emissions (delta) by sector (excluding the power sector) ------------------------------------------------
data_co2_fuel_sec_delta <- data_co2_sec_all[Scen != "s01"]

data_co2_fuel_sec_delta_s00 <- data_co2_fuel_sec_delta[Scen == "s00"]
data_co2_fuel_sec_delta_s00[, `:=`(Scen = NULL, Scenario.1 = NULL, Scenario.2 = NULL, Scenario.3 = NULL)]

data_co2_fuel_sec_delta <- merge(data_co2_fuel_sec_delta, data_co2_fuel_sec_delta_s00, by = c("Sector", "Fuel", "Year"))
data_co2_fuel_sec_delta[, delta := value.x - value.y]
data_co2_fuel_sec_delta[, `:=`(value.x = NULL, value.y = NULL)]

## emissions from electricity are accounted to each end-use sector
data_co2_total_sec_delta <- data_co2_fuel_sec_delta[Fuel == "Total"]

data_co2_total_sec_delta[, Sector := str_to_title(Sector)]
data_co2_total_sec_delta[, Sector := as.factor(Sector)]
levels(data_co2_total_sec_delta$Sector) <- c("(A) Commercial", "(B) Industrial", "(C) Residential",
                                             "(D) Transportation")

plot_co2_total_sec_delta <- ggplot(data = data_co2_total_sec_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector) +
  ylab("Difference in CO2 emissions (MMT)") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S14.pdf", width = 6, height = 5)
print(plot_co2_total_sec_delta)
dev.off()


# Figure S15: average energy price (delta) by fuel for industrial ----------------------------------------------------------------
data_price_ind <- data_model_main[TableNumber == 3
                                  & SubDat == "real"
                                  & DaType == "price"
                                  & Sector == "industrial"
                                  & !(Source %in% c("sector average prices",
                                                    "sector average fossil prices",
                                                    "coal to liquids",
                                                    "residual fuel oil",
                                                    "distillate fuel oil",
                                                    "metallurgical coal"))
                                  & Geogr == "united states", ]
data_price_ind[Source == "other industrial coal", Source := "Coal"]

data_price_ind[, Source := str_to_title(Source)]
data_price_ind[, Source := as.factor(Source)]
levels(data_price_ind$Source) <- c("(A) Coal", "(B) Electricity", "(C) Natural Gas",
                                   "(D) Propane")

data_price_ind_delta <- data_price_ind[Scen != "s01", .(Scen, Source, Year, value)]
data_price_ind_delta_s00 <- data_price_ind_delta[Scen == "s00"]
data_price_ind_delta_s00[, `:=`(Scen = NULL)]

data_price_ind_delta <- merge(data_price_ind_delta, data_price_ind_delta_s00, by = c("Source", "Year"))
data_price_ind_delta[, delta := value.x - value.y]
data_price_ind_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_price_ind_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                       Scenario.2 = scenario_name_2[i],
                                                       Scenario.3 = scenario_name_3[i])]
}

plot_price_ind_delta <- ggplot(data = data_price_ind_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source) +
  ylab("Difference in energy price (2016$/MMBtu)") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S15.pdf", width = 6, height = 5)
print(plot_price_ind_delta)
dev.off()


# Figure S16: energy consumption (delta) for industrial by fuel ----------------------------------------------------------------------------------
data_consum_ind <- data_model_main[TableNumber == 2
                                   & DaType == "consumption"
                                   & Sector %in% c("industrial")
                                   & Source %in% c("liquid fuels subtotal",
                                                   "natural gas subtotal",
                                                   "coal subtotal",
                                                   "renewable energy",
                                                   "electricity")
                                   & Geogr == "united states", ]

data_consum_ind[, Source := factor(Source, levels = c("coal subtotal", "electricity", "liquid fuels subtotal",
                                                      "natural gas subtotal", "renewable energy"))]
levels(data_consum_ind$Source) <- c("(A) Coal", "(B) Electricity", "(C) Liquid Fuels",
                                    "(D) Natural Gas", "(E) Renewable")

data_consum_ind_delta <- data_consum_ind[Scen != "s01", .(Scen, Source, Year, value)]
data_consum_ind_delta_s00 <- data_consum_ind_delta[Scen == "s00"]
data_consum_ind_delta_s00[, `:=`(Scen = NULL)]

data_consum_ind_delta <- merge(data_consum_ind_delta, data_consum_ind_delta_s00, by = c("Source", "Year"))
data_consum_ind_delta[, delta := value.x - value.y]
data_consum_ind_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_consum_ind_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                        Scenario.2 = scenario_name_2[i],
                                                        Scenario.3 = scenario_name_3[i])]
}

plot_consum_ind_delta <- ggplot(data = data_consum_ind_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source) +
  ylab("Difference in energy consumption (quads)") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S16.pdf", width = 6, height = 5)
print(plot_consum_ind_delta)
dev.off()


# Figure S17: carbon emissions (delta) for industrial by fuel -------------------------------------------------------------------------------------
data_co2_fuel_sec_delta_sub <- data_co2_fuel_sec_delta[Sector == "Industrial" & Fuel != "Total"]

data_co2_fuel_sec_delta_sub[, Fuel := str_to_title(Fuel)]
data_co2_fuel_sec_delta_sub[, Fuel := as.factor(Fuel)]
levels(data_co2_fuel_sec_delta_sub$Fuel) <- c("(A) Coal", "(B) Electricity", "(C) Natural Gas",
                                              "(D) Oil")

plot_co2_fuel_sec_delta <- ggplot(data = data_co2_fuel_sec_delta_sub, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Fuel, nrow = 2) +
  ylab("Difference in carbon emissions (MMT)") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S17.pdf", width = 6, height = 5)
print(plot_co2_fuel_sec_delta)
dev.off()


# Figure S27: sensitivity - energy consumption and co2 emissions--------------------------------------------------------------------------------------
## energy consumption
data_consum_sens <- data_model_all[Scen %in% scenario[c(1,3,8,9)]
                                   & TableNumber == 2
                                   & DaType == "consumption"
                                   & Sector == "all sectors"
                                   & Source %in% c("total")
                                   & Geogr == "united states", ]
data_consum_sens[, Scenario.3 := ifelse(Scenario.3 == "Low Renewables Cost", "Yes", "No")]

plot_consum_sens <- ggplot(data = data_consum_sens, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.3)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  ylab("Energy Consumption (quads)") +
  theme(legend.box = "vertical",
        legend.position = c(.15, .01),
        legend.justification = c("left", "bottom"),
        legend.box.just = "left",
        legend.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = 7),
        legend.title=element_text(size=8),
        legend.key.size = unit(0.4, "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 2, order = 1),
         linetype = guide_legend(title="Low Renewables Cost", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


## co2 emissions
data_co2_sens <- data_model_all[Scen %in% scenario[c(1,3,8,9)]
                                &TableNumber == 2
                                & DaType == "emissions"
                                & Geogr == "united states", ]

data_co2_sens[, Scenario.3 := ifelse(Scenario.3 == "Low Renewables Cost", "Yes", "No")]

plot_co2_sens <- ggplot(data = data_co2_sens, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.3), show.legend = FALSE) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  ylab("Carbon Dioxide Emissions (MMT)") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 2, order = 1),
         linetype = guide_legend(title="Low Renewables Cost", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

## output results
plot_combine <- ggarrange(plot_consum_sens,
                          plot_co2_sens,
                          labels = c("(A)", "(B)"),
                          font.label = list(size = 10, color = "black", face = "plain"),
                          nrow = 1)

pdf("NEMSPost/r_figure_output/Figure_S27.pdf", width = 6, height = 2.8)
print(plot_combine)
dev.off()


# Figure S28: pollution results over time (NEI 2017 as the base) --------------------------------------------------------------------------------------
data_plt_proj_plot_nei2017 <- data_plt_proj_plot_nation[nei_year == 2017
                                                        & sensitivity == "value"]

plot_us_total_nei2017 <- ggplot(data = data_plt_proj_plot_nei2017, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
  ylab("Pollutant emissions (MMT)") +
  ggtitle("all sectors: united states") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) + 
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S28.pdf", width = 6, height = 6)
print(plot_us_total_nei2017)
dev.off()


# Figure S29: pollution results over time (a decreasing rate of marginal emission) -----------------------------------------------------------------
## print US total with a decreasing rate of marginal emission of energy consumption
data_plt_proj_plot_decay1 <- data_plt_proj_plot_nation[nei_year == 2014
                                                       & sensitivity == "value.decay.1"]

plot_us_total_decay1 <- ggplot(data = data_plt_proj_plot_decay1, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
  ylab("Pollutant emissions (MMT)") +
  ggtitle("all sectors: united states") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) + 
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S29.pdf", width = 6, height = 6)
print(plot_us_total_decay1)
dev.off()


# Figure S30: pollution results over time (a decreasing rate of marginal emission) ----------------------------
data_plt_proj_plot_decay2 <- data_plt_proj_plot_nation[nei_year == 2014
                                                       & sensitivity == "value.decay.2"]

plot_us_total_decay2 <- ggplot(data = data_plt_proj_plot_decay2, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
  ylab("Pollutant emissions (MMT)") +
  ggtitle("all sectors: united states") +
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
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  expand_limits(y = 0) + 
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("NEMSPost/r_figure_output/Figure_S30.pdf", width = 6, height = 6)
print(plot_us_total_decay2)
dev.off()



