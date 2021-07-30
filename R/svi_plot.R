make_df_for_sviplot <- function(df_hes = vaccine_hesitancy,
																df_vax = vax_today) {

	df_vax %>%
		inner_join(df_hes, by = "fips") %>%
		filter(!is.na(svi_category)) %>%
		mutate(svi_category = forcats::as_factor(svi_category) %>%
					 	forcats::fct_relevel("Very High Vulnerability",
					 											 "High Vulnerability",
					 											 "Moderate Vulnerability",
					 											 "Low Vulnerability",
					 											 "Very Low Vulnerability"))
}

make_svi_race_violinplot <- function(df_hes = vaccine_hesitancy,
																		 df_vax = vax_today,
																		 df_census = census_county,
																		 vax_cutoff = 0.15) {
	make_df_for_sviplot(df_hes = df_hes,
											df_vax = df_vax) %>%
		dplyr::inner_join(df_census, by = "fips") %>%
		dplyr::mutate(vulnerability = ifelse(svi_category %in%
																				 	c("Low Vulnerability",
																				 		"Very Low Vulnerability"),
																				 "Non-Vulnerable",
																				 "Vulnerable"),
									vax_bin = ifelse(fully_vax < vax_cutoff,
																	 "Low vaccination",
																	 "Moderate to high vaccination")) %>%
		ggplot(aes(x = vax_bin,
							 y = percent_black,
							 fill = stringr::str_wrap(vax_bin, 10))) +
		geom_violin(size = 0.1) +
		facet_wrap(~ vulnerability) +
		theme_minimal() +
		labs(y = "Percent Black",
				 title = "Black residents make up a disproportionate share of low-vaccination, vulnerable counties",
				 fill = "Vaccination level") +
		theme(axis.title.x = element_blank(),
					plot.title = element_text(size = 14),
					plot.title.position = "plot") +
		scale_fill_manual(values = c("#f80808", "#0bc202")) +
		scale_y_continuous(labels = scales::percent_format(accuracy = 1))
}

get_vuln_not_vuln <- function() {

	df <- make_df_for_sviplot()

	vuln <- df %>%
		dplyr::filter(!((svi_category == "Low Vulnerability") |
											(svi_category == "Very Low Vulnerability"))
									)
	not_vuln <- df %>%
		dplyr::filter((svi_category == "Low Vulnerability") |
										(svi_category == "Very Low Vulnerability")
									)

	list(vuln = vuln$fully_vax, not_vuln = not_vuln$fully_vax)
}

plot_svi <- function(df_hes = vaccine_hesitancy,
										 df_vax = vax_today,
										 chart_colors = config$charts$cols$svi) {

	make_df_for_sviplot(df_hes = df_hes, df_vax = df_vax) %>%
		ggplot(aes(x = fully_vax, fill = svi_category)) +
		geom_histogram() +
		facet_wrap(~svi_category, scales = "free_y") +
		theme_minimal() +
		scale_x_continuous(labels = scales::percent) +
		labs(x = "Percent of population fully vaccinated",
				 y = "Number of counties",
				 title = "Distribution of vaccination among counties in each SVI category",
				 fill = "SVI category") +
		scale_fill_manual(values = chart_colors)
}







