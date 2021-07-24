#Series_Complete_Pop_Pct, FIPS, Recip_County from vaccination

#Percent_Vaccinated, County_Name from tx_vaccination

library(magrittr); library(RSQLite); library(yaml); library(readr); library(dplyr); library(stringr)
config <- read_yaml("config.yaml")


get_main_vacc_df <- function(dcon){
	res <- dbSendQuery(conn = dcon, "
	SELECT Recip_County as County, FIPS, MAX(Series_Complete_Pop_Pct) as pct_vacc
	FROM vaccination
	GROUP BY FIPS;
	")
	vacc <- dbFetch(res, -1)
	dbClearResult(res)
	print(head(vacc))
	vacc <- mutate(vacc, County = str_replace_all(County, " County", ""), FIPS = as.numeric(FIPS), pct_vacc = pct_vacc/100)
	print(vacc)
}

get_fips_lookup <- function(dcon){
	res <- dbSendQuery(conn = dcon, "
	SELECT county_name as County, county_fips as FIPS
	FROM countypres_2016_2020;
	")
	fips_lookup <- dbFetch(res, -1)
	dbClearResult(res)
	fips_lookup
}

get_tx_vacc_df <- function(dcon){
		res <- dbSendQuery(conn = dcon, "
		SELECT a.County_Name as County, b.county_fips as FIPS, a.Percentage_Vaccinated as pct_vacc
		FROM tx_vaccination AS a
		INNER JOIN (SELECT *
            		FROM countypres_2016_2020
            		WHERE state_po = 'TX' AND party = 'DEMOCRAT' AND year = 2016) AS b
		ON UPPER(a.County_Name )= b.county_name
		ORDER BY a.County_Name ASC;
		")
		tx_proc <- dbFetch(res, -1)
		#tx_proc <- mutate(tx_proc, pct_vacc = pct_vacc*100)
		dbClearResult(res)
		tx_proc
}


get_ca_vacc_df <- function(dcon){
	res <- dbSendQuery(conn = dcon, "
	SELECT a.County, a.FIPS, a.Two_Doses, b.total
	FROM small_ca_vacc as a
	INNER JOIN census as b
	ON a.FIPS = b.FIPS;
	")
	small_ca_vacc <- dbFetch(res, -1)
	dbClearResult(res)
	small_ca_vacc <- transmute(small_ca_vacc, County = County, FIPS = FIPS, pct_vacc = Two_Doses/total)
	small_ca_vacc
}

get_vaccine_df <- function(){
	db <- paste0(config$paths$proc, "finalproject.db")
	dcon <- dbConnect(SQLite(), dbname = db)
	vacc <- get_main_vacc_df(dcon)
	tx_vacc <- get_tx_vacc_df(dcon)
	ca_vacc <- get_ca_vacc_df(dcon)
	dbDisconnect(dcon)

	all_vacc <- full_join(vacc, tx_vacc)
	all_vacc <- full_join(all_vacc, ca_vacc)
	all_vacc
}



