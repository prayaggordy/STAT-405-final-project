library(ggplot2); library(dplyr); library(tidyr)
config <- read_yaml("config.yaml")

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
		dplyr::filter(grepl(pattern = region_name, x = region)) %>%
		dplyr::mutate(bins = cut(.data[[variable]],
														 breaks = c(-Inf, scale_breaks[[variable]]$cuts, Inf),
														 labels = scale_breaks[[variable]]$labels))

	if (region_name == "Region")
		region_name <- "US"

	invisible(variable)

	ggplot(df, aes(fill = bins)) +
		geom_sf(size = 0.1, color = "white") +
		scale_fill_manual(values = fills) +
		theme_minimal() +
		theme(panel.grid = element_blank(),
					axis.title = element_blank(),
					axis.text = element_blank()) +
		labs(title = stringr::str_wrap(glue::glue(title), 50),
				 fill = stringr::str_wrap(variable_pretty, 10)) +
		guides(fill = guide_legend(reverse = T))
}

plot_regions <- function(df_covid = covid,
												 df_census = census_county,
												 df_vax = vax_today,
												 df_vacches = vaccine_hesitancy,
												 xwalk = xwalk_region,
												 map_vars = config$maps$cns) {

	df <- df_census %>%
		dplyr::inner_join(df_covid %>%
												dplyr::group_by(fips) %>%
												dplyr::filter(date == max(date)) %>%
												dplyr::ungroup(),
											by = "fips") %>%
		dplyr::inner_join(df_vax, by = "fips") %>%
		dplyr::inner_join(df_vacches, by = "fips") %>%
		dplyr::mutate(state_fips = stringr::str_sub(fips, end = 2)) %>%
		dplyr::inner_join(xwalk, by = "state_fips") %>%
		dplyr::mutate(dplyr::across(.cols = c(cases, deaths),
																.fns = ~ (./total*100000)/fully_vax,
																.names = "{.col}_fully_vax")) %>%
		dplyr::select(fips, region, hesitant_unsure, tidyselect::ends_with("fully_vax")) %>%
		tidyr::drop_na()

	region_list <- list(midwest = "Midwest",
											northeast = "Northeast",
											south = "South",
											west = "West",
											us = "Region")

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
