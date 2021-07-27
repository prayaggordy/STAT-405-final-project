plot_deaths_pc_vaccine <- function(df_covid = covid,
																	 df_census = census_county,
																	 df_vax = vaccination) {
	df <- df_covid %>%
		filter(date == max(date)) %>%
		dplyr::select(fips, deaths) %>%
		inner_join(df_census %>%
							 	dplyr::select(fips, total),
							 by = "fips") %>%
		dplyr::mutate(deaths_pc = deaths/total*100000) %>%
		dplyr::inner_join(df_vax %>%
												dplyr::filter(date == max(date)),
											by = "fips")

	ggplot(df, aes(x = deaths_pc, y = fully_vax)) +
		geom_point(alpha = 0.75) +
		geom_smooth(se = F, color = "cadetblue4", size = 2) +
		labs(x = "Deaths per 100,000 residents",
				 y = "Percent fully vaccinated",
				 title = "Even counties with large numbers of deaths remain under-vaccinated") +
		theme_minimal() +
		scale_y_continuous(labels = scales::percent) +
		theme(plot.title.position = "plot")
}