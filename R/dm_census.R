library(tidycensus); library(tidyverse); library(magrittr); library(dplyr)
library(tidyr); library(readr); library(tibble)

dm_xwalk_fips <- function() {
	tigris::fips_codes %>%
		dplyr::mutate(county_short_lc = gsub(" County", "", county) %>% tolower(),
									fips = paste0(state_code, county_code)) %>%
		dplyr::select(fips, state_name, county, county_short_lc)
}

dm_xwalk_regions <- function(u = config$urls$region_xwalk,
														 fn = config$data$region_xwalk,
														 path_raw = config$paths$raw,
														 path_proc = config$paths$proc,
														 update = F) {

	df <- download_data(u = u,
											fn_full = paste0(path_raw, fn),
											update = update)

	df <- df %>%
		janitor::clean_names() %>%
		mutate(region = paste(region, "Region")) %>%
		inner_join(tigris::fips_codes %>%
							 	dplyr::select(state_code = state, state_fips = state_code) %>%
							 	distinct(),
							 by = "state_code") %>%
		dplyr::select(state_fips, region)

	write_csv(df, paste0(path_proc, fn))

	df
}

download_census <- function(geography,
														fn = config$data$census,
														path_raw = config$paths$raw,
														path_proc = config$paths$proc,
														update = F) {

	fn_raw <- paste0(path_raw, glue::glue(fn))
	fn_proc <- paste0(path_proc, glue::glue(fn))

	if (file.exists(fn_proc) & !update)
		return(read_csv(fn_proc))

	if (!file.exists(fn_raw) | update) {
		df <- get_acs(geography = geography,
									variables = c(med_income = "B19013_001", male = "B01001_002",
																female = "B01001_026", med_age = "B01002_001",
																white = "B01001A_001", black = "B01001B_001",
																hispanic = "B01001I_001", asian = "B01001D_001",
																total = "B01001_001", other = "B01001F_001"),
									year = 2019,
									moe_level = 95)
		write_csv(df, fn_raw)
	} else {
		df <- read_csv(fn_raw)
	}

	df <- df %>%
		dplyr::select(-moe) %>%
		rename(fips = GEOID, county = NAME) %>%
		pivot_wider(names_from = variable, values_from = estimate) %>%
		mutate(across(.cols = male:hispanic, .fns = ~ ./total, .names = "percent_{.col}")) %>%
		dm_states_remove()

	write_csv(df, fn_proc)

	df
}
