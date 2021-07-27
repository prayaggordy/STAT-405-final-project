library(tidyr); library(dplyr); library(tidycensus); library(stringr); library(tigris)

plot_vax_by_region <- function(df = vaccination,
															 tx = tx_vaccination,
															 xwalk = xwalk_region,
															 fips = fips_codes) {
	df <- df %>%
		filter(date == max(date)) %>%
		mutate(state_fips = str_sub(fips, end = 2)) %>%
		inner_join(xwalk, by = "state_fips") %>%
		mutate(region = str_remove(region, " Region"))

	fips <- fips %>% filter(state=="TX") %>% select(2,4,5)
	fips$county_code <- paste0(fips$state_code,fips$county_code)
	fips <- select(fips, 2, 3)
	fips$county <- word(fips$county, 1)


	tx$County_Name <- rename(tx$County_Name, tx$county)
	tx <- tx %>%
		select(1, 6, 9) %>%
		inner_join(fips, by="county")
	View(tx)

	ggplot(df, aes(x = fully_vax)) +
		geom_histogram(binwidth = 0.1) +
		facet_wrap(~region, scales = "free_y") +
		theme_minimal() +
		labs(x = "Percent of adults fully vaccinated",
				 y = "Number of counties",
				 title = "Percent vaccinated by county") +
		scale_x_continuous(labels = scales::percent)
}

tx_vaccination <- tx_vaccination %>%
	right_join(fips, by="county")
tx_vaccination <- rename(tx_vaccination, fips=county_code)

tx <- inner_join(tx_vaccination,census_county, by="fips")
tx <- tx %>%
	select(1:4, 6) %>% rename(county=county.x) %>%
	mutate(fully_vax=FULLY_VACCINATED_CALC/total,
				 first_dose=TOTAL_PEOPLE_CALC/total) %>%
	select(4,7,6)

vaccination_today <- vaccination %>% filter(date=="2021-07-25") %>% arrange(fips)
vaccination_today[2524:2777, 3] <- tx$first_dose
vaccination_today[2524:2777, 4] <- tx$fully_vax

small_ca_vacc$fips <- paste0("0", small_ca_vacc$FIPS)
small_ca_vacc <- inner_join(small_ca_vacc, census_county, by="fips")
small_ca_vacc$fully_vax <- small_ca_vacc$Two_Doses/small_ca_vacc$total
small_ca_vacc$first_dose <- small_ca_vacc$One_Dose/small_ca_vacc$total
small_ca_vacc <- select(small_ca_vacc, 5,24,25)

vaccination_today[c(188,200,208,211,212,218,232,239), 3] <- small_ca_vacc$first_dose
vaccination_today[c(188,200,208,211,212,218,232,239), 4] <- small_ca_vacc$fully_vax





