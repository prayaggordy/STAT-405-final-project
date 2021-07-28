library(ggplot2); library(RSQLite); library(yaml); library(MASS); library(grid); library(gridExtra); library(dplyr); library(magrittr)
config <- read_yaml("config.yaml")


make_df_for_sviplot <- function(df_hes = vaccine_hesitancy,
																df_vax = vaccination) {

	df_vax %>%
		dplyr::filter(date == max(date)) %>%
		inner_join(df_hes, by = "fips") %>%
		filter(!is.na(svi_category)) %>%
		mutate(svi_category = forcats::as_factor(svi_category) %>%
					 	forcats::fct_relevel("Very High Vulnerability",
					 											 "High Vulnerability",
					 											 "Moderate Vulnerability",
					 											 "Low Vulnerability",
					 											 "Very Low Vulnerability")) -> svi_df
	svi_df
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
										 df_vax = vaccination) {

	make_df_for_sviplot(df_hes = df_hes, df_vax = df_vax) %>%
		ggplot(aes(x = fully_vax, fill = svi_category)) +
		geom_histogram() +
		facet_wrap(~svi_category, scales = "free_y") +
		theme_minimal() +
		scale_x_continuous(labels = scales::percent) +
		labs(x = "Percent of population fully vaccinated",
				 y = "Number of counties",
				 title = "Distribution of vaccination among counties in each SVI category",
				 fill = "SVI category")
}







