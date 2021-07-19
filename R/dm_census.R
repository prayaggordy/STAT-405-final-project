library(tidycensus)
library(tidyverse)
library(magrittr)

acs19 <- get_acs(geography = "county",
								 variables = c(med_income = "B19013_001", male = "B01001_002", female = "B01001_026",
								 							 med_age = "B01002_001", white = "B01001A_001", black = "B01001B_001",
								 							 hispanic = "B01001I_001", asian = "B01001D_001", total = "B01001_001",
								 							 other = "B01001F_001"),
								 year = 2019,
								 moe_level = 95)
acs19_subset <- acs19[, c("GEOID","NAME", "variable","estimate")]
acs19_spread <- spread(acs19_subset, key = variable, value = estimate)

county_data <- mutate(acs19_spread, percent_asian=asian/total, percent_white=white/total,
											percent_black=black/total, percent_hispanic=hispanic/total,
											percent_male=male/total, percent_female=female/total,
											percent_other=other/total)

county_data <- county_data[,c(1,2,12,4,3,6,10,7,5,11,14,15,13,16,19,17,18,8,9)]
