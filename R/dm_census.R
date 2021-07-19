library(tidycensus)
library(tidyverse)
library(dplyr)
library(magrittr)

#census_api_key("9f4ab0bf5eb0ba86e6bcf3d14e14683c45956f5b", install=TRUE)

var <- load_variables(2019, "acs5", cache = TRUE)

acs19 <- get_acs(geography = "county",
								 variables = c(medincome = "B19013_001", male="B01001_002", female="B01001_026",
								 							medage="B01002_001", white="B01001A_001", black="B01001B_001",
								 							hispanic="B01001I_001", asian="B01001D_001", total="B01001_001",
								 							other="B01001F_001"),
								 year = 2019,
								 moe_level=95)
acs19_subset <- acs19[, c("GEOID","NAME", "variable","estimate")]
acs19_spread <- spread(acs19_subset, key = variable, value = estimate)

county_data <- mutate(acs19_spread, percent_asian=asian/total, percent_white=white/total,
											percent_black=black/total, percent_hispanic=hispanic/total,
											percent_male=male/total, percent_female=female/total,
											percent_other=other/total)


