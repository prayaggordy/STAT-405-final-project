
#https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh/data

# California by county - https://data.democratandchronicle.com/covid-19-vaccine-tracker/california/06/

# Texas by county - https://tabexternal.dshs.texas.gov/t/THD/views/COVID-19VaccineinTexasDashboard/Summary?:origin=card_share_link&:embed=y&:isGuestRedirectFromVizportal=y


library(magrittr)

config <- yaml::read_yaml("config.yaml")

download_vacches <- function(u = "https://data.cdc.gov/api/views/q9mh-h2tw/rows.csv?accessType=DOWNLOAD",
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
