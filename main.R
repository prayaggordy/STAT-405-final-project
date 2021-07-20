library(magrittr); library(RSQLite); library(yaml); library(readr)
config <- read_yaml("config.yaml")
sapply(list.files(path = "R", full.names = T), source, .GlobalEnv)

covid <- download_nyt()
census <- download_census()
pres <- read_csv(paste0(config$paths$raw, config$data$pres))
vaccination <- read_csv(paste0(config$paths$us_ts, config$data$vax_data_ts))

create_sql()
