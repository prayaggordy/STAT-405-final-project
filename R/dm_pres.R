dm_pres <- function(fn = config$data$pres,
										path_raw = config$paths$raw,
										path_proc = config$paths$proc) {

	df <- read_csv(paste0(path_raw, fn)) %>%
		dplyr::filter(year == 2020,
									candidate == "DONALD J TRUMP") %>%
		dplyr::mutate(fips = stringr::str_pad(county_fips, width = 5, side = "left", pad = "0")) %>%
		dplyr::group_by(fips, totalvotes) %>%
		dplyr::summarize(candidatevotes = sum(candidatevotes)) %>%
		dplyr::ungroup() %>%
		dplyr::mutate(percent_trump = candidatevotes/totalvotes) %>%
		dplyr::select(fips, percent_trump) %>%
		dm_states_remove()

}