library(magrittr); library(RSQLite); library(yaml); library(readr)
config <- read_yaml("config.yaml")
sapply(list.files(path = "R", full.names = T), source, .GlobalEnv)

xwalk_region <- dm_xwalk_regions()
xwalk_fips <- dm_xwalk_fips()

covid <- download_nyt(update = T)
vaccine_hesitancy <- download_vacches()
census_county <- download_census(geography = "county", update = T)
census_region <- download_census(geography = "region", update = T)
pres <- dm_pres()
tx_vaccination <- dm_texas_vacc(update = T)
small_ca_vacc <- dm_ca_vacc()
va_vaccination <- dm_va_vacc(update = T)
us_vaccination <- download_vacc(update = T)
vaccination <- dm_combine_vacc()
vax_today <- vaccination %>%
	dplyr::group_by(fips) %>%
	dplyr::filter(date == max(date)) %>%
	dplyr::ungroup()
region_plots <- plot_regions()

create_sql()
