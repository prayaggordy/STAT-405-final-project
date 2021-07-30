plot_vax_by_region <- function(df = vax_today,
															 xwalk = xwalk_region,
															 fips = fips_codes) {
	df <- df %>%
		mutate(state_fips = str_sub(fips, end = 2)) %>%
		inner_join(xwalk, by = "state_fips") %>%
		mutate(region = str_remove(region, " Region"))

	ggplot(df, aes(x = fully_vax, fill = region)) +
		geom_histogram(binwidth = 0.1) +
		facet_wrap(~region, scales = "free_y") +
		theme_minimal() +
		labs(x = "Percent of adults fully vaccinated",
				 y = "Number of counties",
				 title = "Percent vaccinated by county") +
		scale_x_continuous(labels = scales::percent) +
		theme(legend.position = "none")
}
