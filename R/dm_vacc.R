download_vacc <- function(u = config$urls$cdc_vacc,
													fn = config$data$cdc_vacc,
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
		dplyr::select(date,
									fips,
									first_dose = administered_dose1_pop_pct,
									fully_vax = series_complete_pop_pct) %>%
		filter(fips != "UNK") %>%
		dplyr::mutate(date = as.Date(date, "%m/%d/%Y"),
									first_dose = as.numeric(first_dose)/100,
									fully_vax = as.numeric(fully_vax)/100) %>%
		dm_states_remove()

	write_csv(df, fn_proc)

	df
}

download_tx_vacc <- function(u, fn, path_raw, path_proc, sheet, update) {

	fn_raw <- paste0(path_raw, fn)

	if (file.exists(fn_raw) & !update) {
		df <- readr::read_csv(fn_raw)
	} else {
		df <- openxlsx::read.xlsx(u, sheet = sheet)
		readr::write_csv(df, fn_raw)
	}

	df <- df %>%
		janitor::clean_names() %>%
		dplyr::slice(-c(1:3))

	df
}

dm_texas_vacc <- function(u = config$urls$tex_vacc,
													fn = config$data$tex_vacc,
													path_raw = config$paths$raw,
													path_proc = config$paths$proc,
													sheet = 2,
													update = F,
													xwalk = xwalk_fips,
													census = census_county) {

	df <- download_tx_vacc(u = u,
												 fn = fn,
												 path_raw = path_raw,
												 path_proc = path_proc,
												 sheet = sheet,
												 update = update) %>%
		dplyr::mutate(county_name = tolower(county_name)) %>%
		dplyr::inner_join(xwalk %>%
												dplyr::filter(state_name == "Texas"),
											by = c("county_name" = "county_short_lc")) %>%
		dplyr::select(fips,
									first_dose = people_vaccinated_with_at_least_one_dose,
									fully_vax = people_fully_vaccinated) %>%
		dplyr::inner_join(census, by = "fips") %>%
		dplyr::mutate(dplyr::across(c(first_dose, fully_vax), ~ as.numeric(.) / total)) %>%
		dplyr::select(fips, first_dose, fully_vax)

	readr::write_csv(df, paste0(path_proc, fn))

	df
}

dm_ca_vacc <- function(fn = config$data$small_ca_vacc,
											 path_raw = config$paths$raw,
											 path_proc = config$paths$proc,
											 census = census_county) {

	df <- readr::read_csv(paste0(path_raw, fn)) %>%
		janitor::clean_names() %>%
		dplyr::mutate(fips = stringr::str_pad(fips, width = 5, side = "left", pad = "0")) %>%
		dplyr::inner_join(census, by = "fips") %>%
		dplyr::mutate(dplyr::across(c(one_dose, two_doses), ~ . / total)) %>%
		dplyr::select(fips, first_dose = one_dose, fully_vax = two_doses)

	readr::write_csv(df, paste0(path_proc, fn))

	df
}

dm_va_vacc <- function(u = config$urls$va_vacc,
											 fn = config$data$va_vacc,
											 path_proc = config$paths$proc,
											 update = F,
											 df_census = census_county) {

	fn_proc <- paste0(path_proc, fn)
	if (file.exists(fn_proc) & !update) {
		return(readr::read_csv(fn_proc))
	}

	df <- readr::read_csv(u) %>%
		janitor::clean_names() %>%
		dplyr::mutate(administration_date = as.Date(administration_date, "%m/%d/%Y"),
									first_dose = as.numeric(vaccine_manufacturer %in% c("Pfizer", "Moderna") &
																						dose_number == 1)*vaccine_doses_administered_count,
									fully_vax = as.numeric(!first_dose)*vaccine_doses_administered_count) %>%
		dplyr::arrange(administration_date) %>%
		dplyr::inner_join(df_census %>%
												dplyr::select(fips, total),
											by = "fips") %>%
		dplyr::group_by(fips) %>%
		dplyr::mutate(dplyr::across(c(first_dose, fully_vax), ~ cumsum(.)/total)) %>%
		dplyr::ungroup() %>%
		dplyr::select(date = administration_date, fips, first_dose, fully_vax) %>%
		dplyr::distinct(date, fips, .keep_all = T)

	readr::write_csv(df, fn_proc)

	df
}

dm_combine_vacc <- function(us = us_vaccination,
														tx = tx_vaccination,
														ca = small_ca_vacc,
														va = va_vaccination,
														fn = config$data$vax_data_all,
														path_proc = config$paths$proc) {

	df <- us %>%
		dplyr::filter(!(fips %in% c(tx$fips, ca$fips, va$fips))) %>%
		dm_states_remove() %>%
		dplyr::bind_rows(tx %>%
										 	dplyr::mutate(date = max(us$date),
										 								fips = as.character(fips))) %>%
		dplyr::bind_rows(ca %>%
										 	dplyr::mutate(date = max(us$date),
										 								fips = as.character(fips))) %>%
		dplyr::bind_rows(va %>%
										 	dplyr::mutate(fips = as.character(fips))) %>%
		dplyr::group_by(date, fips) %>%
		dplyr::summarize(dplyr::across(c(first_dose, fully_vax), max)) %>%
		dplyr::ungroup()

	readr::write_csv(df, paste0(path_proc, fn))

	df
}
