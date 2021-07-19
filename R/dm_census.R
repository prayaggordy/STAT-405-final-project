library(tidycensus)
library(tidyverse)
library(magrittr)

download_census <- function(fn,
														path_raw,
														path_proc,
														update = F) {

	if (file.exists(fn_proc) & !update)
		return(readr::read_csv(fn_proc))

	fn_raw <- paste0(path_raw, fn)
	fn_proc <- paste0(path_proc, fn)

	if (!file.exists(fn_raw) | update) {
		df <- get_acs(geography = "county",
									variables = c(med_income = "B19013_001", male = "B01001_002", female = "B01001_026",
																med_age = "B01002_001", white = "B01001A_001", black = "B01001B_001",
																hispanic = "B01001I_001", asian = "B01001D_001", total = "B01001_001",
																other = "B01001F_001"),
									year = 2019,
									moe_level = 95)
		readr::write_csv(df, fn_raw)
	} else {
		df <- readr::read_csv(fn_raw)
	}

	df <- df %>%
		dplyr::select(-moe) %>%
		dplyr::rename(fips = GEOID, county = NAME) %>%
		tidyr::pivot_wider(names_from = variable, values_from = estimate) %>%
		dplyr::mutate(dplyr::across(.cols = male:hispanic, .fns = ~ ./total, .names = "percent_{.col}"))

	readr::write_csv(df, fn_proc)

	df
}
