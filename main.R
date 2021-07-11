library(magrittr)

config <- yaml::read_yaml("config.yaml")

covid <- download_nyt()

