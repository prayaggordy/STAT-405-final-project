library(magrittr); library(RSQLite); library(yaml); library(readr)
config <- read_yaml("config.yaml")
sapply(list.files(path = "R", full.names = T), source, .GlobalEnv)

covid <- download_nyt()
census_county <- download_census(geography = "county")
census_region <- download_census(geography = "region")
pres <- read_csv(paste0(config$paths$raw, config$data$pres))
tx_vaccination <- read_csv(paste0(config$paths$raw, config$data$tex_vacc))
small_ca_vacc <- read_csv(paste0(config$paths$raw, config$data$small_ca_vacc))
vaccination <- download_vacc()
xwalk_region <- xwalk_regions()

# create_sql(update = T)

high_vulnerability_poli_plot(get_vacc_svi_poli_df(svi_vacc))

svi_vacc <- get_vacc_SVI_df()
svi_vacc_cens <- get_vacc_SVI_cens_df(svi_vacc)

svi_facet_wrap(svi_vacc)
high_vulnerability_race_plot(svi_vacc_cens)

