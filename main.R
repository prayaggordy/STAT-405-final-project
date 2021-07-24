library(magrittr); library(RSQLite); library(yaml); library(readr)
config <- read_yaml("config.yaml")
sapply(list.files(path = "R", full.names = T), source, .GlobalEnv)

covid <- download_nyt()
census <- download_census()
pres <- read_csv(paste0(config$paths$raw, config$data$pres))
tx_vaccination <- read_csv(paste0(config$paths$raw, config$data$tex_vacc))
small_ca_vacc <- read_csv(paste0(config$paths$raw, config$data$small_ca_vacc))
vaccination <- download_vacc()

create_sql(update = T)
