library(ggplot2); library(dplyr); library(tidyr)

make_map <- function(df,
										 region_name,
										 variable,
										 variable_pretty,
										 fill_rev = config$maps$fills$rev,
										 title = "Counties in the {region_name} by {tolower(variable_pretty)}",
										 scale_breaks = config$maps$scale_breaks,
										 fills = config$maps$fills$cols) {

	data("county_laea")

	if (fill_rev[[variable]])
		fills <- rev(fills)

	df <- county_laea %>%
		dplyr::rename(fips = GEOID) %>%
		dplyr::inner_join(df, by = "fips") %>%
		dplyr::filter(grepl(pattern = region_name, x = region))

	invisible(variable)

	ggplot(df, aes(fill = !!ensym(variable))) +
		geom_sf(size = 0.1, color = "white") +
		scale_fill_stepsn(breaks = scale_breaks[[variable]],
											colours = fills) +
		theme_minimal() +
		theme(panel.grid = element_blank(),
					axis.title = element_blank(),
					axis.text = element_blank()) +
		labs(title = stringr::str_wrap(glue::glue(title), 80),
				 fill = stringr::str_wrap(variable_pretty, 10))
}

plot_regions <- function(df_covid = covid,
												 df_census = census_county,
												 df_vax = vaccination,
												 xwalk = xwalk_region,
												 map_vars = config$maps$cns) {

	df <- df_census %>%
		dplyr::inner_join(df_covid %>%
												dplyr::group_by(fips) %>%
												dplyr::filter(date == max(date)) %>%
												dplyr::ungroup(),
											by = "fips") %>%
		dplyr::inner_join(df_vax %>%
												dplyr::group_by(fips) %>%
												dplyr::filter(date == max(date)) %>%
												dplyr::ungroup(),
											by = "fips") %>%
		dplyr::mutate(state_fips = stringr::str_sub(fips, end = 2)) %>%
		dplyr::inner_join(xwalk, by = "state_fips") %>%
		dplyr::mutate(dplyr::across(.cols = c(cases, deaths),
																.fns = ~ (./total*100000)/fully_vax,
																.names = "{.col}_fully_vax")) %>%
		dplyr::select(fips, region, tidyselect::ends_with("fully_vax"))

	region_list <- list(midwest = "Midwest",
											northeast = "Northeast",
											south = "South",
											west = "West")

	lapply(setNames(names(map_vars), names(map_vars)),
				 function(v) {
				 	lapply(region_list,
				 				 function(r) {
				 				 	make_map(df = df,
				 				 					 region_name = r,
				 				 					 variable = v,
				 				 					 variable_pretty = map_vars[[v]])
				 				 }
				 )}
	)
}
