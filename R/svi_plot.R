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
					 											 "Very Low Vulnerability")) %>% df_vacches
	df_vacches
}

plot_svi <- function(){
		df_vacches <- make_df_for_sviplot()
		ggplot(data = df_vacches, aes(x = fully_vax, fill = svi_category)) +
		geom_histogram() +
		facet_wrap(~svi_category, scales = "free_y") +
		theme_minimal() +
		scale_x_continuous(labels = scales::percent) +
		labs(x = "Percent of population fully vaccinated",
				 y = "Number of counties",
				 title = "Distribution of vaccination among counties in each SVI category",
				 fill = "SVI category")
}







