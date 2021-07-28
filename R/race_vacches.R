library(ggplot2); library(RSQLite); library(yaml); library(MASS); library(grid); library(gridExtra)
config <- read_yaml("config.yaml")

get_vacches_data <- function() {
	db <- paste0(config$paths$proc, "finalproject.db")
	dcon <- dbConnect(SQLite(), dbname = db)

	query <- paste("
	SELECT `County Name` as County, `FIPS Code` as fips,
	`Estimated hesitant` as hesitant, `Estimated hesitant or unsure` as hes_unsure,
	`Estimated strongly hesitant` as strong_hes,
	`Percent non-hispanic Black` as pct_black,
	`Percent non-hispanic White` as pct_white
	FROM vaccine_hesitancy;")
	res <- dbSendQuery(conn = dcon, query)
	vacches <- dbFetch(res, -1)
	dbClearResult(res)
	dbDisconnect(dcon)

	vacches
}


get_vacches_poli_df <- function(df_pres = pres){
	vacches <- get_vacches_data()
	pres <- mutate(df_pres, fips =as.numeric(fips))
	inner_join(pres, vacches)
}

plot_vacches_race <- function(){
	df <- get_vacches_poli_df()
	df_democrat_notallwhite <- filter(get_vacches_poli_df(), percent_trump < 0.4 & pct_black > 0.05)
	ggplot(df_democrat_notallwhite, aes(x = pct_black, y = hes_unsure)) +
		geom_rect(mapping = aes(xmin = 0, xmax = 0.9,
														ymin = -0.01, ymax = 0.35,
														fill = I('lightskyblue2')),
							color = 'lightskyblue2') +
		geom_point(alpha = 1/3) +
		geom_smooth(method = "lm", se = F, color = "black") +
		labs(x = "Percent Black",
				 y = "Percent Hesitant or Unsure About COVID-19 Vaccine",
				 title = "Vaccine Hesitancy Versus Race for Democratic Counties with Significant Black Populations") +
		theme_minimal() +
		scale_x_continuous(labels = scales::percent) +
		scale_y_continuous(labels = scales::percent)
}

plot_vacches_race()




