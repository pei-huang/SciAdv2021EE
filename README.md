# SciAdv2021EE

This repository contains the data and model code to replicate the results in the paper: K. T. Gillingham, P. Huang, C. Buehler, J. Peccia, D. R. Gentner, The climate and health benefits from intensive building energy efficiency improvements. *Science Advances* **7**, eabg0947 (2021). It consists of three main components—postprossessing the scenario results from NEMS simulations, indoor air quality analysis, and health effects estimations—and some external data.

## NEMSCode

This folder contains the modifications of input data and model code that simulate the main results for the three designed scenarios in the paper—Intermediate EE, Optimistic EE, and Carbon Pricing. In order to replicate the NEMS simulation results, you need to download the NEMS source code (www.eia.gov/outlooks/aeo/info_nems_archive.php) and configure it in your computer, which may involve certain costs. You can then replace the corresponding files in the NEMS model directory with the provided files in this folder to generate the NEMS simulation results.

## NEMSPost

This folder contains the NEMS simulation data and source code to project energy-related local air pollutant emissions. The NEMS simulation data are contained in the subfolder *nems_simulation_data*. The *main.R* file controls the data build stream, including setting scenario names, reading and processing raw NEMS simulation data, projecting local air pollutant emissions, and generating the main and supplementary figures in the paper. This data stream also generates some intermediate data files, which are read into a subsequent routine—Health. Hence, the data build stream has to be run before the one in the *Health* folder.

## IAQ

This folder consists of the input data and source code for the indoor air quality analysis. The data build stream is controled by “Main.m”. The routine generates all indoor air quality data and the associated figures presented in the paper.

## Health

This folder contains the routine of estimating the health effects based on the outdoor emissions data projected in the **NEMSPost** routine and indoor air quality data simulated in the **IAQ** routine. The folder contains the source code to generate the tables and figures presented in the paper.

## ExternalData

This folder contains the data from other sources, which facilitate our projection and estimations, including marginal damage data from AP3, EASIUR, and InMAP, EPA National Emissions Inventory data, map shape files, and some mapping files.