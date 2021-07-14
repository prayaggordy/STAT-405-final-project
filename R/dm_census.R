library(magrittr)

config <- yaml::read_yaml("config.yaml")

download_census <- function(u = "https://www2.census.gov/programs-surveys/cps/datasets/2021/basic/jun21pub.csv",
												 fn = config$data$census,
												 path_raw = config$paths$raw,
												 path_proc = config$paths$proc,
												 update = F) {

	df <- download_data(u = u,
											fn_full = paste0(path_raw, fn),
											update = update)

	# this is where we do any data processing

	df
}
census <- download_census()
