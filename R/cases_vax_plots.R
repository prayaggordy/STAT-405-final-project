library(usmap); library(ggplot2); library(dplyr); library(tidyr)


cases <- covid %>% filter(date==max(date)) %>% dplyr::select(2:6)
total_pop <- dplyr::select(census_county, 1, 3)
cases_vax <- vaccination %>% filter(date==max(date)) %>%
	inner_join(cases, by="fips") %>% inner_join(total_pop, by="fips") %>%
	dplyr::mutate(cases_100k=(cases*100000)/total,
								deaths_100k=(deaths*100000)/total,
								cases_fully_vax=(cases/total)/fully_vax)%>%
	filter(!is.na(deaths),state!="Hawaii",state!="Alaska")


png("south_fully_vax_map.png")
plot_usmap(regions="county", include=.south_region,data = cases_vax,
					 values = "fully_vax") +
	scale_fill_stepsn(breaks=c(0.25,0.45,0.65),
										colours=c("red4","red1","steelblue2","blue2"),
										name="Fully Vaccinated Proportion") +
	theme(legend.position = "right")
dev.off()

png("northeast_cases_vax_map.png")
plot_usmap(regions="county", include=.northeast_region,data = cases_vax,
					 values = "cases_fully_vax") +
	scale_fill_stepsn(breaks=c(0.23,0.31,0.40),
										colours=c("red4","red1","steelblue2","blue2"),
										name="Fully Vaccinated Proportion") +
	theme(legend.position = "right")
dev.off()

png("midwest_fully_vax_map.png")
plot_usmap(regions="county", include=.midwest_region,
					 data = cases_vax, values = "fully_vax") +
	scale_fill_stepsn(breaks=c(0.25,0.45,0.65),
										colours=c("red4","red1","steelblue2","blue2"),
										name="Fully Vaccinated Proportion") +
	theme(legend.position = "right")
dev.off()

png("west_fully_vax_map.png")
plot_usmap(regions="county", include=.west_region, exclude=c("HI","AK"),
					 data = cases_vax, values = "fully_vax") +
	scale_fill_stepsn(breaks=c(0.25,0.45,0.65),
										colours=c("red4","red1","steelblue2","blue2"),
										name="Fully Vaccinated Proportion") +
	theme(legend.position = "right")
dev.off()

png("south_cases_vax_map.png")
plot_usmap(regions="county", include=.south_region,data = cases_vax,
					 values = "cases_fully_vax") +
	scale_fill_stepsn(breaks=c(0.23,0.30,0.39),
										colours=c("blue2","steelblue2","red2","red4"),
										name="Cases per 100k vs. Vaccinations") +
	theme(legend.position = "right")
dev.off()

png("northeast_cases_vax_map.png")
plot_usmap(regions="county", include=.northeast_region,data = cases_vax,
					 values = "cases_fully_vax") +
	scale_fill_stepsn(breaks=c(0.23,0.30,0.39),
										colours=c("blue2","steelblue2","red2","red4"),
										name="Cases per 100k vs. Vaccinations") +
	theme(legend.position = "right")
dev.off()

png("west_cases_vax_map.png")
plot_usmap(regions="county", include=.west_region,data = cases_vax,
					 values = "cases_fully_vax", exclude=c("HI","AK")) +
	scale_fill_stepsn(breaks=c(0.23,0.30,0.39),
										colours=c("blue2","steelblue2","red2","red4"),
										name="Cases per 100k vs. Vaccinations") +
	theme(legend.position = "right")
dev.off()

png("midwest_cases_vax_map.png")
plot_usmap(regions="county", include=.midwest_region,data = cases_vax,
					 values = "cases_fully_vax") +
	scale_fill_stepsn(breaks=c(0.23,0.30,0.39),
										colours=c("blue2","steelblue2","red2","red4"),
										name="Cases per 100k vs. Vaccinations") +
	theme(legend.position = "right")
dev.off()
