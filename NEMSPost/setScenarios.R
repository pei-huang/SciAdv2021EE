# set up scenario names ----------------
scenario <- c("s00", "s01", "s02", "s03", "s04", "s05", "s06", "s07", "s08")
scenario_main <- paste0("s", str_pad(c(0:4), 2, "left", pad = "0"))
scenario_sens <- paste0("s", str_pad(c(5:8), 2, "left", pad = "0"))

## EE scenarios
scenario_name_1 <- c("Reference",
                     "Intermediate EE",
                     "Optimistic EE",
                     "Reference",
                     "Optimistic EE",
                     "Optimistic EE",
                     "Optimistic EE",
                     "Reference",
                     "Optimistic EE")
scenario_name_1 <- factor(scenario_name_1,
                          levels = unique(scenario_name_1))

## carbon pricing scenarios
scenario_name_2 <- c("No",
                     "No",
                     "No",
                     "Yes",
                     "Yes",
                     "No",
                     "No",
                     "No",
                     "No")
scenario_name_2 <- factor(scenario_name_2,
                          levels = unique(scenario_name_2))

## sensitivity scenarios
scenario_name_3 <- c("Equipment & Shell",
                     "Equipment & Shell",
                     "Equipment & Shell",
                     "Equipment & Shell",
                     "Equipment & Shell",
                     "Equipment",
                     "Shell",
                     "Low Renewables Cost",
                     "Low Renewables Cost")
scenario_name_3 <- factor(scenario_name_3,
                          levels = unique(scenario_name_3))