library(ggplot2); library(RSQLite); library(yaml); library(MASS); library(grid); library(gridExtra); library(dplyr); library(magrittr)
config <- read_yaml("config.yaml")


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


get_df_svi_race <- function(df_svi = make_df_for_sviplot(), svi_upper, svi_lower){
	db <- paste0(config$paths$proc, config$data$db)
	dcon <- dbConnect(SQLite(), dbname = db)
	query <- "
	SELECT fips, percent_black
	FROM census_county;"
	res<- dbSendQuery(conn=dcon, query)
	cens <- dbFetch(res, -1)
	dbClearResult(res)
	df_svi %>%
		dplyr::filter((svi >= svi_lower) & (svi<=svi_upper)) %>%
		inner_join(cens, by = "fips") -> svi_df
	svi_df
}

make_svi_race_violinplot <- function(svi_upper, svi_lower, title){
	df = get_df_svi_race(svi_upper = svi_upper, svi_lower=svi_lower)
	vaccine_tag <- function(num){
		if (num < 0.15){
			returnval <- "Low vaccination"
		}
		else{
			returnval <- "Moderate to high vaccination"
		}
		returnval
	}
	new_df <- mutate(df, vacc_tag =sapply(fully_vax, vaccine_tag))
	ggplot(new_df, aes(x=vacc_tag, y=percent_black, fill = vacc_tag)) +
		theme_minimal()+
		labs(x = "Vaccination Level",
				 y = "Percent Black",
				 title = title,
				 fill = "Vaccination Level")+
		scale_y_continuous(labels = scales::percent)+
		geom_violin()
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







