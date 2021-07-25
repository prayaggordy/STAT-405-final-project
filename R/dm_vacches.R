
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

	df <- download_data(u = u,
											fn_full = paste0(path_raw, fn),
											update = update)

	# this is where we do any data processing

	df
}
