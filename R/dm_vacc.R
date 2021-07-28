
#https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh/data

# California by county - https://data.democratandchronicle.com/covid-19-vaccine-tracker/california/06/

# Texas by county - https://tabexternal.dshs.texas.gov/t/THD/views/COVID-19VaccineinTexasDashboard/Summary?:origin=card_share_link&:embed=y&:isGuestRedirectFromVizportal=y


library(magrittr)

config <- yaml::read_yaml("config.yaml")

download_vacc <- function(u = "https://data.cdc.gov/api/views/8xkx-amqh/rows.csv?accessType=DOWNLOAD",
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
		mutate(date = as.Date(date, "%m/%d/%Y"),
					 across(c(first_dose, fully_vax), ~as.numeric(.)/100))

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

dm_texas_vacc <- function(u = "https://www.dshs.texas.gov/immunize/covid19/COVID-19-Vaccine-Data-by-County.xls",
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

dm_va_vacc <- function(u = "https://data.virginia.gov/api/views/28k2-x2rj/rows.csv?accessType=DOWNLOAD",
											 fn = config$data$va_vacc,
											 path_raw = config$paths$raw,
											 path_proc = config$paths$proc,
											 update = F) {

	fn_raw <- paste0(path_raw, fn)
	fn_proc <- paste0(path_proc, fn)
	if (file.exists(fn_proc) & !update) {
		return(readr::read_csv(fn_proc))
	}

	df <- readr::read_csv(fn_raw) %>%
		dplyr::group_by(administration_date, fips)
}

dm_combine_vacc <- function(us = us_vaccination,
														tx = tx_vaccination,
														ca = small_ca_vacc,
														fn = config$data$vax_data_all,
														path_proc = config$paths$proc) {
	df <- us %>%
		dplyr::filter(!(fips %in% c(tx$fips, ca$fips))) %>%
		dplyr::bind_rows(tx %>%
										 	dplyr::mutate(date = max(us$date))) %>%
		dplyr::bind_rows(ca %>%
										 	dplyr::mutate(date = max(us$date))) %>%
		dplyr::group_by(date, fips) %>%
		dplyr::summarize(dplyr::across(c(first_dose, fully_vax), max)) %>%
		dplyr::ungroup()

	readr::write_csv(df, paste0(path_proc, fn))

	df
}
