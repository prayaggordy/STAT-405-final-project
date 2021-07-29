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
CREATE TABLE cases AS
SELECT *
FROM cases
WHERE date LIKE '___1%';"
	dbSendQuery(dcon, query)

	dbRemoveTable(conn = dcon, name = name)
}

create_sql <- function(path_proc = config$paths$proc,
											 fn = config$data$db,
											 update = F) {

	fn_proc <- paste0(path_proc, fn)

	if (!file.exists(fn_proc) | update) {
		if (file.exists(fn_proc))
			file.remove(fn_proc)

		dcon <- dbConnect(SQLite(), dbname = fn_proc)
		print(dbListTables(dcon))
		add_simple(dcon = dcon,
							 df = xwalk_region,
							 name = "xwalk_region")
		add_simple(dcon = dcon,
							 df = xwalk_fips,
							 name = "xwalk_fips")
		add_simple(dcon = dcon,
							 df = covid,
							 name = "covid")
		add_simple(dcon = dcon,
							 df = vaccine_hesitancy,
							 name = "vaccine_hesitancy")
		add_simple(dcon = dcon,
							 df = census_county,
							 name = "census_county")
		add_simple(dcon = dcon,
							 df = census_region,
							 name = "census_region")
		add_simple(dcon = dcon,
							 df = pres,
							 name = "pres")
		add_simple(dcon = dcon,
							 df = vaccination,
							 name = "vaccination")

		dbDisconnect(conn = dcon)
	}
}
