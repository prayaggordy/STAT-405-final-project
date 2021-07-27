
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

