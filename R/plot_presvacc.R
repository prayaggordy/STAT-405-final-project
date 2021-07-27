library(ggplot2); library(RSQLite); library(yaml); library(MASS); library(grid); library(gridExtra)
config <- read_yaml("config.yaml")

plot_trump_vacc <- function(df_pres = pres,
														df_vacc = vaccination) {
	df_vacc %>%
		dplyr::filter(date == max(date)) %>%
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

#blue_demo <- inner_join(filter(presvacc, trump_pct < 0.4), demos)
#lowvacc_demo <- inner_join(filter(presvacc, pct_vacc < 0.4), demos)
#lowvacc_blue <- filter(lowvacc_demo, trump_pct < 0.4)
#lowvacc_red <- filter(lowvacc_demo, trump_pct > 0.6)


get_census_data<- function(){
	db <- paste0(config$paths$proc, "finalproject.db")
	dcon <- dbConnect(SQLite(), dbname = db)
	res <- dbSendQuery(conn = dcon, "
	SELECT fips as FIPS, total, med_age, med_income, percent_white, percent_black, percent_hispanic, percent_asian
	FROM census;
	")
	cens <- mutate(dbFetch(res, -1), FIPS = as.numeric(FIPS))
	dbClearResult(res)
	cens
}



make_race_histogram <- function(df, col_dark, col_light, title, ylim){
	ggplot(df, aes(x=percent_black))+
		theme_minimal()+
		ggtitle(title)+
		xlab("Proportion Black")+
		geom_histogram(color = col_dark, fill = col_light)+
		xlim(0,1)+
		ylim(0,ylim)
}

make_race_hist_plots <- function(){

	p1 <- make_race_histogram(bottom_and_blue, "black", "lightskyblue2", "Democrat, Bottom 10% of Residuals (Low Vax)", 6)
	p2 <- make_race_histogram(bottom_and_red, "black", "lightsalmon2", "Republican, Bottom 10% of Residuals (Low Vax)", 35)
	p3 <- make_race_histogram(top_and_blue, "black", "lightskyblue2", "Democrat, Top 10% of Residuals (High Vax)", 9)
	p4 <- make_race_histogram(top_and_red, "black", "lightsalmon2", "Republican, Top 10% of Residuals (High Vax)", 30)

	grid.arrange(p1, p2, p3, p4, nrow=2)
}

#
# presvacc <- filter(get_vacc_pres_df(2020.0), !is.na(FIPS) & !is.na(pct_vacc) & !is.na(trump_pct))
# demos <- get_census_data()
# presvacc_2016 <- filter(get_vacc_pres_df(2016.0), !is.na(FIPS) & !is.na(pct_vacc) & !is.na(trump_pct))
#
# resid_by_county <- data.frame(County = presvacc$County.y, FIPS = presvacc$FIPS, trump_pct = presvacc$trump_pct, resid = rr.huber$resid)
# resid_sorted <- resid_by_county[order(resid_by_county$resid),]
# rows <- nrow(resid_sorted)
# ten_percent <- round(rows*0.1)
# bottom_resid <- resid_sorted[1:ten_percent,]
# top_resid <- resid_sorted[(rows-ten_percent):rows,]
#
# bottom <- inner_join(bottom_resid, demos, by='FIPS')
# top <- inner_join(top_resid, demos, by = 'FIPS')
#
#
# bottom_and_blue <- filter(bottom, trump_pct < 0.4)
# bottom_and_red <- filter(bottom, trump_pct >0.6)
#
# top_and_red <- filter(top, trump_pct > 0.6)
# top_and_blue <- filter(top, trump_pct < 0.4)
#
# plot_trump_vacc()
# make_race_hist_plots()
