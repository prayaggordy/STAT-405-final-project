library(ggplot2); library(RSQLite); library(yaml); library(MASS); library(grid); library(gridExtra)
config <- read_yaml("config.yaml")

plot_trump_vacc <- function(df_pres = pres,
														df_vacc = vax_today) {
	df_vacc %>%
		dplyr::inner_join(df_pres, by = "fips") %>%
		ggplot(aes(x = percent_trump, y = fully_vax)) +
		geom_rect(mapping = aes(xmin = 0, xmax = 0.5,
														ymin = -0.01, ymax = 1.01,
														fill = I('lightskyblue2')),
							color = 'lightskyblue2') +
		geom_rect(mapping = aes(xmin = 0.5, xmax = 1.0,
														ymin = -0.01, ymax = 1.01,
														fill = I('lightsalmon2')),
							color = 'lightsalmon2') +
		geom_point(alpha = 1/3) +
		geom_smooth(method = "lm", se = F, color = "black") +
		labs(x = "Proportion of Votes Cast for Trump",
				 y = "Proportion of Population Vaccinated",
				 title = "Proportion Vaccinated Versus Proportion Voting for Trump by County") +
		theme_minimal() +
		scale_x_continuous(labels = scales::percent) +
		scale_y_continuous(labels = scales::percent)
}

make_race_histogram <- function(df, col_dark, col_light, title, ylim){
	ggplot(df, aes(x = percent_black)) +
		theme_minimal() +
		labs(title = title,
				 x = "Proportion Black",
				 y = "Number of counties") +
		geom_histogram(color = col_dark, fill = col_light) +
		ylim(0,ylim) +
		theme(plot.title = element_text(size = 10),
					plot.title.position = "plot") +
		scale_x_continuous(labels = scales::percent_format(accuracy = 1),
											 limits = c(0, 1))
}

make_race_hist_plots <- function(bottom_and_blue, bottom_and_red,
																 top_and_blue, top_and_red) {

	p1 <- make_race_histogram(bottom_and_blue, "black", "lightskyblue2",
														"Democrat, Bottom 10% of Residuals (Low Vax)", 6)
	p2 <- make_race_histogram(bottom_and_red, "black", "lightsalmon2",
														"Republican, Bottom 10% of Residuals (Low Vax)", 35)
	p3 <- make_race_histogram(top_and_blue, "black", "lightskyblue2",
														"Democrat, Top 10% of Residuals (High Vax)", 9)
	p4 <- make_race_histogram(top_and_red, "black", "lightsalmon2",
														"Republican, Top 10% of Residuals (High Vax)", 30)

	grid.arrange(p1, p2, p3, p4, nrow = 2)
}


race_hist_plot <- function(df_pres = pres,
													 df_vacc = vax_today,
													 df_census = census_county) {

	presvacc <- df_vacc %>%
		dplyr::inner_join(df_pres, by = "fips") %>%
		tidyr::drop_na()

	rr.huber <- rlm(presvacc$fully_vax ~ presvacc$percent_trump)

	presvacc_resid <- mutate(presvacc, resid = rr.huber$resid) %>%
		arrange(resid)

	rows <- nrow(presvacc_resid)
	ten_percent <- round(rows * 0.1)
	bottom <- slice_head(presvacc_resid, n = ten_percent) %>%
		inner_join(df_census, by = "fips")
	top <- slice_tail(presvacc_resid, n = ten_percent) %>%
		inner_join(df_census, by = "fips")

	cutoff <- 0.4

	bottom_and_blue <- filter(bottom, percent_trump < cutoff)
	bb_tag <- mutate(bottom_and_blue, orig_df = "bottom_blue")
	bottom_and_red <- filter(bottom, percent_trump > 1 - cutoff)
	br_tag <- mutate(bottom_and_red, orig_df = "bottom_red")

	top_and_red <- filter(top, percent_trump > 1 - cutoff)
	tr_tag <- mutate(top_and_red, orig_df = "top_red")
	top_and_blue <- filter(top, percent_trump < cutoff)
	tb_tag <- mutate(top_and_blue, orig_df = "top_blue")

	outliers_tagged <- bb_tag %>%
		bind_rows(br_tag) %>%
		bind_rows(tr_tag) %>%
		bind_rows(tb_tag)

	anova_model <- aov(percent_black ~ orig_df, data = outliers_tagged)

	tk <- TukeyHSD(anova_model)

	make_race_hist_plots(bottom_and_blue = bottom_and_blue,
											 bottom_and_red = bottom_and_red,
											 top_and_red = top_and_red,
											 top_and_blue = top_and_blue)

	tk
}