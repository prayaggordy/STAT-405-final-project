library(RSQLite); library(yaml); library(readr); library(dplyr);
config <- read_yaml("config.yaml")



get_trump_percents <- function(dcon, year){


	query <- paste("
	SELECT county_name as County, county_fips as FIPS, SUM(candidatevotes)/totalvotes as trump_pct
	FROM (select *
				FROM countypres_2016_2020
				WHERE candidate LIKE 'DONALD%'AND year =", year,")
	GROUP BY county_fips;")
	res <- dbSendQuery(conn = dcon, query)
	tot <- dbFetch(res, -1)
	dbClearResult(res)
	tot
}



get_vacc_pres_df <- function(year){
	db <- paste0(config$paths$proc, "finalproject.db")
	dcon <- dbConnect(SQLite(), dbname = db)
	pres <- get_trump_percents(dcon, year)
	print(head(pres))
	dbDisconnect(dcon)

	vacc <- get_vaccine_df()
	vacc_pres <- inner_join(pres, vacc, by = c('FIPS'))
	vacc_pres
}


