library(magrittr); library(RSQLite); library(yaml); library(readr)
config <- read_yaml("config.yaml")
sapply(list.files(path = "R", full.names = T), source, .GlobalEnv)

xwalk_region <- dm_xwalk_regions()
xwalk_fips <- dm_xwalk_fips()

covid <- download_nyt()
vaccine_hesitancy <- download_vacches()
census_county <- download_census(geography = "county")
census_region <- download_census(geography = "region")
pres <- dm_pres()
tx_vaccination <- dm_texas_vacc()
small_ca_vacc <- dm_ca_vacc()
us_vaccination <- download_vacc()
vaccination <- dm_combine_vacc()

create_sql()

