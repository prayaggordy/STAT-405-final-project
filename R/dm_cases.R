library(magrittr)

config <- yaml::read_yaml("config.yaml")

download_nyt <- function(u = config$urls$nyt_covid,
												 fn = config$data$nyt_covid,
												 path_raw = config$paths$raw,
												 path_proc = config$paths$proc,
												 update = F) {

	df <- download_data(u = u,
											fn_full = paste0(path_raw, fn),
											update = update) %>%
		dm_states_remove()

	df
}
