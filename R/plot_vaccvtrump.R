#Series_Complete_Pop_Pct, FIPS, Recip_County from vaccination

#Percent_Vaccinated, County_Name from tx_vaccination

library(magrittr); library(RSQLite); library(yaml); library(readr)
config <- read_yaml("config.yaml")


get_vacc_df <- function(dcon){
	res <- dbSendQuery(conn = dcon, "
	SELECT FIPS, Recip_County as County, Series_Complete_Pop_Pct as pct_vacc
	FROM vaccination;
	")
	vacc <- dbFetch(res, -1)
	dbClearResult(res)
	vacc
}

get_tx_vacc_df <- function(dcon){
	res <- dbSendQuery(conn = dcon, "
	SELECT County_Name as County, Percent_Vaccinated as pct_vacc
	FROM tx_vaccination;
	")
	tx_vacc <- dbFetch(res, -1)
	dbClearResult(res)
	tx_vacc
}

process_tx_vacc <- function(df){

}

get_ca_vacc_df <- function(dcon){
	res <- dbSendQuery(conn = dcon, "
	SELECT County, FIPS, Two_Doses
	FROM small_ca_vacc;
	")
	small_ca_vacc <- dbFetch(res, -1)
	dbClearResult(res)
	ca_vacc
}

get_vaccine_df <- function(){
	dcon <- dbConnect(SQLite(), dbname = paste0(config$paths$processed, "finalproject.db"))
	vacc <- get_vacc_df(dcon)
	tx_vacc <- get_tx_vacc_df(dcon)
	ca_vacc <- get_ca_vacc_df(dcon)


}





Peopl