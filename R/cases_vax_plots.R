library(usmap); library(ggplot2); library(dplyr); library(tidyr)

make_map <- function(df,
										 scale_breaks = config$maps$scale_breaks,
										 fills = config$maps$fills) {
	data("county_laea")

	df <- county_laea %>%
		dplyr::rename(fips = GEOID) %>%
		dplyr::inner_join(df, by = "fips")

	ggplot(df, aes(fill = fully_vax)) +
		geom_sf(size = 0.1, color = "white") +
		scale_fill_stepsn(breaks = scale_breaks$fully_vax,
											colours = fills,
											name = "Fully Vaccinated Proportion")
}

plot_regions <- function(df_covid = covid,
												 df_census = census_county,
												 df_vax = vaccination,
												 xwalk = xwalk_region) {

	df <- df_census %>%
		dplyr::inner_join(df_covid %>%
												dplyr::filter(date == max(date)),
											by = "fips") %>%
		dplyr::inner_join(df_vax %>%
												dplyr::filter(date == max(date)),
											by = "fips") %>%
		dplyr::mutate(state_fips = stringr::str_sub(fips, end = 2)) %>%
		dplyr::inner_join(xwalk, by = "state_fips") %>%
		dplyr::mutate(dplyr::across(.cols = c(cases, deaths),
																.fns = ~ (./total*100000)/fully_vax,
																.names = "{.col}_fully_vax")) %>%
		dplyr::select(fips, region, tidyselect::ends_with("fully_vax"))
}

# cases <- covid %>% filter(date==max(date)) %>% dplyr::select(2:6)
# total_pop <- dplyr::select(census_county, 1, 3)
# cases_vax <- vaccination %>% filter(date==max(date)) %>%
# 	inner_join(cases, by="fips") %>% inner_join(total_pop, by="fips") %>%
# 	dplyr::mutate(cases_100k=(cases*100000)/total,
# 								deaths_100k=(deaths*100000)/total,
# 								cases_fully_vax=(cases/total)/fully_vax,
# 								deaths_fully_vax=(deaths/total)/fully_vax)%>%
# 	filter(!is.na(deaths),state!="Hawaii",state!="Alaska")
#
#
# png("south_fully_vax_map.png")
# plot_usmap(regions="county", include=.south_region,data = cases_vax,
# 					 values = "fully_vax") +
	# scale_fill_stepsn(breaks=c(0.25,0.45,0.65),
	# 									colours=c("red4","red1","steelblue2","blue2"),
	# 									name="Fully Vaccinated Proportion") +
# 	theme(legend.position = "right")
# dev.off()
#
# png("northeast_cases_vax_map.png")
# plot_usmap(regions="county", include=.northeast_region,data = cases_vax,
# 					 values = "cases_fully_vax") +
# 	scale_fill_stepsn(breaks=c(0.23,0.31,0.40),
# 										colours=c("red4","red1","steelblue2","blue2"),
# 										name="Fully Vaccinated Proportion") +
# 	theme(legend.position = "right")
# dev.off()
#
# png("midwest_fully_vax_map.png")
# plot_usmap(regions="county", include=.midwest_region,
# 					 data = cases_vax, values = "fully_vax") +
# 	scale_fill_stepsn(breaks=c(0.25,0.45,0.65),
# 										colours=c("red4","red1","steelblue2","blue2"),
# 										name="Fully Vaccinated Proportion") +
# 	theme(legend.position = "right")
# dev.off()
#
# png("west_fully_vax_map.png")
# plot_usmap(regions="county", include=.west_region, exclude=c("HI","AK"),
# 					 data = cases_vax, values = "fully_vax") +
# 	scale_fill_stepsn(breaks=c(0.25,0.45,0.65),
# 										colours=c("red4","red1","steelblue2","blue2"),
# 										name="Fully Vaccinated Proportion") +
# 	theme(legend.position = "right")
# dev.off()
#
# png("south_cases_vax_map.png")
# plot_usmap(regions="county", include=.south_region,data = cases_vax,
# 					 values = "cases_fully_vax") +
# 	scale_fill_stepsn(breaks=c(0.23,0.30,0.39),
# 										colours=c("blue2","steelblue2","red2","red4"),
# 										name="Cases per 100k vs. Vaccinations") +
# 	theme(legend.position = "right")
# dev.off()
#
# png("northeast_cases_vax_map.png")
# plot_usmap(regions="county", include=.northeast_region,data = cases_vax,
# 					 values = "cases_fully_vax") +
# 	scale_fill_stepsn(breaks=c(0.23,0.30,0.39),
# 										colours=c("blue2","steelblue2","red2","red4"),
# 										name="Cases per 100k vs. Vaccinations") +
# 	theme(legend.position = "right")
# dev.off()
#
# png("west_cases_vax_map.png")
# plot_usmap(regions="county", include=.west_region,data = cases_vax,
# 					 values = "cases_fully_vax", exclude=c("HI","AK")) +
# 	scale_fill_stepsn(breaks=c(0.23,0.30,0.39),
# 										colours=c("blue2","steelblue2","red2","red4"),
# 										name="Cases per 100k vs. Vaccinations") +
# 	theme(legend.position = "right")
# dev.off()
#
# png("midwest_cases_vax_map.png")
# plot_usmap(regions="county", include=.midwest_region,data = cases_vax,
# 					 values = "cases_fully_vax") +
# 	scale_fill_stepsn(breaks=c(0.23,0.30,0.39),
# 										colours=c("blue2","steelblue2","red2","red4"),
# 										name="Cases per 100k vs. Vaccinations") +
# 	theme(legend.position = "right")
# dev.off()
#
# png("south_deaths_vax_map.png")
# plot_usmap(regions="county", include=.south_region,data = cases_vax,
# 					 values = "deaths_fully_vax") +
# 	scale_fill_stepsn(breaks=c(0.23,0.30,0.39),
# 										colours=c("blue2","steelblue2","red2","red4"),
# 										name="Deaths per 100k vs. Vaccinations") +
# 	theme(legend.position = "right")
# dev.off()
#
# png("northeast_deaths_vax_map.png")
# plot_usmap(regions="county", include=.northeast_region,data = cases_vax,
# 					 values = "deaths_fully_vax") +
# 	scale_fill_stepsn(breaks=c(0.23,0.30,0.39),
# 										colours=c("blue2","steelblue2","red2","red4"),
# 										name="Deaths per 100k vs. Vaccinations") +
# 	theme(legend.position = "right")
# dev.off()
#
# png("west_deaths_vax_map.png")
# plot_usmap(regions="county", include=.west_region,data = cases_vax,
# 					 values = "deaths_fully_vax", exclude=c("HI","AK")) +
# 	scale_fill_stepsn(breaks=c(0.23,0.30,0.39),
# 										colours=c("blue2","steelblue2","red2","red4"),
# 										name="Deaths per 100k vs. Vaccinations") +
# 	theme(legend.position = "right")
# dev.off()
#
# png("midwest_deaths_vax_map.png")
# plot_usmap(regions="county", include=.midwest_region,data = cases_vax,
# 					 values = "deaths_fully_vax") +
# 	scale_fill_stepsn(breaks=c(0.23,0.30,0.39),
# 										colours=c("blue2","steelblue2","red2","red4"),
# 										name="Deaths per 100k vs. Vaccinations") +
# 	theme(legend.position = "right")
# dev.off()
