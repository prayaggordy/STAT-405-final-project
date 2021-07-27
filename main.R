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

svi_vacc <- get_vacc_SVI_df()
svi_vacc_cens <- get_vacc_SVI_cens_df(svi_vacc)

high_vulnerability_poli_plot(get_vacc_svi_poli_df(svi_vacc))

svi_facet_wrap(svi_vacc)
high_vulnerability_race_plot(svi_vacc_cens)

create_sql()
