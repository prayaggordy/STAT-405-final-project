download_vacches <- function(u = config$urls$vacc_hes,
														 fn = config$data$vacc_hes,
														 path_raw = config$paths$raw,
														 path_proc = config$paths$proc,
														 update = F) {

	fn_proc <- paste0(path_proc, fn)
	if (file.exists(fn_proc) & !update) {
		return(read_csv(fn_proc))
	}

	df <- download_data(u = u,
											fn_full = paste0(path_raw, fn),
											update = update)

	df <- df %>%
		janitor::clean_names() %>%
		dplyr::select(fips = fips_code,
									hesitant = estimated_hesitant,
									hesitant_unsure = estimated_hesitant_or_unsure,
									hesitant_strongly = estimated_strongly_hesitant,
									svi = social_vulnerability_index_svi,
									svi_category) %>%
		dplyr::mutate(fips = stringr::str_pad(fips, width = 5, side = "left", pad = "0")) %>%
		dm_states_remove()

	write_csv(df, fn_proc)

	df
}
