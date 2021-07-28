library(ggplot2); library(RSQLite); library(yaml); library(MASS); library(grid); library(gridExtra)
config <- read_yaml("config.yaml")

get_vacches_data <- function(dcon){

	query <- paste("
	SELECT `County Name` as County, `FIPS Code` as FIPS,
	`Estimated hesitant` as hesitant, `Estimated hesitant or unsure` as hes_unsure,
	`Estimated strongly hesitant` as strong_hes,
	`Percent non-hispanic Black` as pct_black,
	`Percent non-hispanic White` as pct_white
	FROM vaccine_hesitancy;")
	res <- dbSendQuery(conn = dcon, query)
	vacches <- dbFetch(res, -1)
	dbClearResult(res)

	vacches
}


get_vacches_poli_df <- function(dcon){
	db <- paste0(config$paths$proc, "finalproject.db")
	dcon <- dbConnect(SQLite(), dbname = db)

	pres_df <- get_trump_percents(dcon, 2020)
	vacches <- get_vacches_data(dcon)
	inner_join(pres_df, vacches, by = FIPS)

	dbDisconnect(dcon)
}

#head(get_vacches_poli_df())
