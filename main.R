library(magrittr); library(RSQLite); library(yaml); library(readr)
config <- read_yaml("config.yaml")
sapply(list.files(path = "R", full.names = T), source, .GlobalEnv)

xwalk_region <- dm_xwalk_regions()
xwalk_fips <- dm_xwalk_fips()

covid <- download_nyt()
vaccine_hesitancy <- download_vacches()
census_county <- download_census(geography = "county")
census_region <- download_census(geography = "region")
pres <- read_csv(paste0(config$paths$raw, config$data$pres))
tx_vaccination <- read_csv(paste0(config$paths$raw, config$data$tex_vacc))
small_ca_vacc <- read_csv(paste0(config$paths$raw, config$data$small_ca_vacc))
vaccination <- download_vacc()

create_sql()

plot_svi()
