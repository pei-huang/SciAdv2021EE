# The main file for estimating health effects

# required packages------------------------------------
library(data.table)
library(ggplot2)
library(stringr)
library(readxl)
library(ggsci)
library(rgdal)
library(scales)

# set working directory------------------------------------
base_path <- "PATH OF YOUR LOCAL REPOSITORY/SciAdv2021EE/"
setwd(base_path)

dir.create(file.path(base_path, "Health/r_data_output"), showWarnings = FALSE)
dir.create(file.path(base_path, "Health/r_figure_output"), showWarnings = FALSE)

# read scenario settings --------------------------------
source("NEMSPost/setScenarios.R")

# outdoor health effects -------------------------------------
source("Health/outdoorHealth.R")

# indoor health effects -------------------------------------
source("Health/indoorHealth.R")

# make the tables in the paper -------------------------------------
source("Health/table.R")

# make the figures in the paper -------------------------------------
source("Health/plot.R")
