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

dm_states_remove <- function(df,
														 states_remove = config$states_remove) {
	df %>%
		dplyr::mutate(state_fips = stringr::str_sub(fips, end = 2)) %>%
		dplyr::filter(!(state_fips %in% states_remove)) %>%
		dplyr::select(-state_fips)
}
