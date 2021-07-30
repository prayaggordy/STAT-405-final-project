plot_vacches_race <- function(df_vacches = vaccine_hesitancy,
															df_census = census_county,
															df_pres = pres) {
	df_vacches %>%
		dplyr::inner_join(df_census, by = "fips") %>%
		dplyr::inner_join(df_pres, by = "fips") %>%
		dplyr::filter(percent_trump < 0.4 & percent_black > 0.05) %>%
		ggplot(aes(x = percent_black, y = hesitant_unsure)) +
		geom_point(alpha = 1/3) +
		geom_smooth(method = "lm", se = F, color = "cadetblue3", size = 2) +
		labs(x = "Percent Black",
				 y = "Percent Hesitant or Unsure About COVID-19 Vaccine",
				 title = stringr::str_wrap("Vaccine Hesitancy Versus Race for Democratic Counties with Significant Black Populations", 55)) +
		theme_minimal() +
		scale_x_continuous(labels = scales::percent) +
		scale_y_continuous(labels = scales::percent) +
		theme(plot.title.position = "plot",
					plot.title = element_text(size = 14))
}
