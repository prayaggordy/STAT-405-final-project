library(magrittr)

config <- yaml::read_yaml("config.yaml")

download_data <- function(u,
													fn_full,
													update = F) {

	if (!file.exists(fn_full) | update) {
		df <- readr::read_csv(u)
		readr::write_csv(df, fn_full)
	} else {
		df <- readr::read_csv(fn_full)
	}

	df
}

download_nyt <- function(u = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
												 fn = config$data$nyt_covid,
												 path_raw = config$paths$raw,
												 path_proc = config$paths$proc,
												 update = F) {

	df <- download_data(u = u,
											fn_full = paste0(path_raw, fn),
											update = update)

	# this is where we do any data processing

	df
}
