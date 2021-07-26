add_pres_results <- function(dcon,
														 df,
														 name = "countypres") {

	dbWriteTable(conn = dcon, name = name, value = df,
							 append = TRUE, row.names = FALSE)

	query <- "
CREATE TABLE countypres_2016_2020 AS
SELECT *
FROM countypres
WHERE year = 2020 OR year = 2016;"
	dbSendQuery(dcon, query)

	dbRemoveTable(conn = dcon, name = name)
}

add_simple <- function(dcon, df, name) {
	dbWriteTable(conn = dcon, name = name, value = df,
							 append = TRUE, row.names = FALSE)
}

add_cases <- function(dcon,
											df,
											name = "cases") {
	dbWriteTable(conn = dcon, name = name, value = df,
							 append = TRUE, row.names = FALSE)

	query <- "
CREATE TABLE cases2021 AS
SELECT *
FROM cases
WHERE date LIKE '___1%';"
	dbSendQuery(dcon, query)

	dbRemoveTable(conn = dcon, name = name)
}

create_sql <- function(path_proc = config$paths$proc,
											 fn = config$data$db,
											 update = F) {
	if (!file.exists(paste0(path_proc, fn)) | update) {
		file.remove(paste0(path_proc, fn))

		dcon <- dbConnect(SQLite(), dbname = paste0(config$paths$proc, config$data$db))
		add_pres_results(dcon = dcon,
										 df = pres)
		add_simple(dcon = dcon,
							 df = vaccination,
							 name = "vaccination")
		add_simple(dcon = dcon,
							 df = vaccine_hesitancy,
							 name = "vaccine_hesitancy")
		add_simple(dcon = dcon,
							 df = tx_vaccination,
							 name = "tx_vaccination")
		add_simple(dcon = dcon,
							 df = small_ca_vacc,
							 name = "small_ca_vacc")
		add_cases(dcon = dcon,
							df = covid)
		add_simple(dcon = dcon,
							 df = census,
							 name = "census")

		dbDisconnect(conn = dcon)
	}
}
