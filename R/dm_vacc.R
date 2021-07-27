
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
		select(date,
					 fips,
					 first_dose = administered_dose1_recip_18plus_pop_pct,
					 fully_vax = series_complete_18plus_pop_pct) %>%
		filter(fips != "UNK") %>%
		mutate(date = as.Date(date, "%m/%d/%Y"),
					 across(c(first_dose, fully_vax), ~as.numeric(.)/100))

	write_csv(df, fn_proc)

	df
}

download_tx_vacc <- function(u, fn, path_raw, path_proc, sheet, update) {

	fn_raw <- paste0(path_raw, fn)
	fn_proc <- paste0(path_proc, fn)
	if (file.exists(fn_proc) & !update) {
		return(readr::read_csv(fn_proc))
	}

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

	df
}

