library(magrittr)
config <- yaml::read_yaml("config.yaml")
sapply(list.files(path = "R", full.names = T), source, .GlobalEnv)

covid <- download_nyt()
census <- download_census()

