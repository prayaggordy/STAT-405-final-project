library(magrittr)

config <- yaml::read_yaml("config.yaml")

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
