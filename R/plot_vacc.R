plot_deaths_pc_vaccine <- function(df_covid = covid,
																	 df_census = census_county,
																	 df_vax = vax_today) {
	df <- df_covid %>%
		filter(date == max(date)) %>%
		dplyr::select(fips, deaths) %>%
		inner_join(df_census %>%
							 	dplyr::select(fips, total),
							 by = "fips") %>%
		dplyr::mutate(deaths_pc = deaths/total*100000) %>%
		dplyr::inner_join(df_vax, by = "fips")

	ggplot(df, aes(x = deaths_pc, y = fully_vax)) +
		geom_point(alpha = 0.5) +
		geom_smooth(se = F, color = "cadetblue4", size = 2) +
		labs(x = "Deaths per 100,000 residents",
				 y = "Percent fully vaccinated",
				 title = "Even counties with large numbers of deaths remain under-vaccinated") +
		theme_minimal() +
		scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
		theme(plot.title.position = "plot")
}

plot_vax_over_time <- function(df_vax = vaccination,
															 df_hes = vaccine_hesitancy) {
	df <- vaccination %>%
		dplyr::filter(fully_vax != 0) %>%
		dplyr::inner_join(vaccine_hesitancy %>%
												dplyr::select(fips, svi_category),
											by = "fips") %>%
		tidyr::drop_na() %>%
		dplyr::mutate(svi_category = forcats::as_factor(svi_category) %>%
										forcats::fct_relevel("Very High Vulnerability",
																				 "High Vulnerability",
																				 "Moderate Vulnerability",
																				 "Low Vulnerability",
																				 "Very Low Vulnerability"))

	ggplot(df, aes(x = date, y = fully_vax, color = svi_category)) +
		geom_smooth(se = F) +
		theme_minimal() +
		labs(x = "Date",
				 y = "Percent fully vaccinated",
				 title = "Counties in each SVI categories have diverged",
				 subtitle = "Lower-vulnerability counties are now more vaccinated",
				 color = "SVI category") +
		scale_x_date(date_labels = "%B") +
		scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
		theme(plot.title.position = "plot")
}