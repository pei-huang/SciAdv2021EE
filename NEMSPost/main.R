# The main file for postprocessing NEMS simulation data

# required packages------------------------------------
library(data.table)
library(ggplot2)
library(stringr)
library(ggsci)
library(ggpubr)
library(gridExtra)
library(rgdal)

# set working directory------------------------------------
base_path <- "PATH OF YOUR LOCAL REPOSITORY/SciAdv2021EE/"
setwd(base_path)

dir.create(file.path(base_path, "NEMSPost/r_data_output"), showWarnings = FALSE)
dir.create(file.path(base_path, "NEMSPost/r_figure_output"), showWarnings = FALSE)


# read scenario settings --------------------------------
source("NEMSPost/setScenarios.R")

# read NEMS simulation data-------------------------------------
source("NEMSPost/readNEMS.R")

# project local air pollutant emissions -------------------------
source("NEMSPost/projectEmissions.R")

# make the figures in the paper  --------------------------------
source("NEMSPost/plot.R")
