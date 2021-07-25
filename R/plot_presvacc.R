library(ggplot2); library(RSQLite); library(yaml); library(MASS)
config <- read_yaml("config.yaml")

presvacc <- filter(get_vacc_pres_df(2020.0), !is.na(FIPS) & !is.na(pct_vacc) & !is.na(trump_pct))
demos <- get_census_data()
presvacc_2016 <- filter(get_vacc_pres_df(2016.0), !is.na(FIPS) & !is.na(pct_vacc) & !is.na(trump_pct))



blue_demo <- inner_join(filter(presvacc, trump_pct < 0.4), demos)
lowvacc_demo <- inner_join(filter(presvacc, pct_vacc < 0.4), demos)
lowvacc_blue <- filter(lowvacc_demo, trump_pct < 0.4)
lowvacc_red <- filter(lowvacc_demo, trump_pct > 0.6)


par(mfrow = c(1, 3))

hist(lowvacc_blue$percent_black+lowvacc_blue$percent_hispanic)
hist(blue_demo$percent_black + blue_demo$percent_hispanic)
hist(demos$percent_black+demos$percent_hispanic)




resid_by_county <- data.frame(County = presvacc$County.y, FIPS = presvacc$FIPS, trump_pct = presvacc$trump_pct, resid = rr.huber$resid)
resid_sorted <- resid_by_county[order(resid_by_county$resid),]
rows <- nrow(resid_sorted)
ten_percent <- round(rows*0.1)
bottom_resid <- resid_sorted[1:ten_percent,]
top_resid <- resid_sorted[(rows-ten_percent):rows,]

bottom <- inner_join(bottom_resid, demos, by='FIPS')
top <- inner_join(top_resid, demos, by = 'FIPS')


bottom_and_blue <- filter(bottom, trump_pct < 0.4)
bottom_and_red <- filter(bottom, trump_pct >0.6)

top_and_red <- filter(top, trump_pct > 0.6)
top_and_blue <- filter(top, trump_pct < 0.4)



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



par(mfrow = c(2, 2))
hist(bottom_and_blue$percent_black+bottom_and_blue$percent_hispanic,
		 main= 'Democrat, Bottom 10% of Residuals (Low Vax)',
		 xlab = "Proportion Black or Hispanic",
		 col = 'lightskyblue2',
		 cex.main = 1,
		 xlim = c(0,1))
hist(bottom_and_red$percent_black+bottom_and_red$percent_hispanic,
		 main = 'Republican, Bottom 10% of Residuals (Low Vax)',
		 xlab = "Proportion Black or Hispanic",
		 col = "lightsalmon2",
		 xlim = c(0,1))
hist(top_and_red$percent_black+top_and_red$percent_hispanic,
		 main = "Republican, Top 10% of Residuals (High Vax)",
		 xlab = "Proportion Black or Hispanic",
		 col = 'lightsalmon2',
		 xlim = c(0,1))
hist(top_and_blue$percent_black+top_and_blue$percent_hispanic,
		 main = "Democrat, Top 10% of Residuals (High Vax)",
		 xlab = "Proportion Black or Hispanic",
		 col = 'lightskyblue2',
		 xlim = c(0,1))


ggplot(presvacc)+aes(x=trump_pct, y = pct_vacc)+
	ggtitle("Proportion Vaccinated Versus Proportion Voting for Trump by County")+
	ylab("Proportion of Population Vaccinated") +
	xlab("Proportion of Votes Cast for Trump")+
	scale_color_manual(values = c("blue","red"))+
	geom_rect(mapping=aes(xmin=0, xmax=0.5, ymin=-0.01, ymax=1.01, fill=I('lightskyblue2')), color = 'lightskyblue2')+
	geom_rect(mapping=aes(xmin=0.5, xmax=1.0, ymin=-0.01, ymax=1.01, fill=I('lightsalmon2')), color = 'lightsalmon2')+
	theme(axis.text = element_text(size = 8), axis.title = element_text(size = 10), title = element_text(size = 12))+
	geom_abline(intercept = inter_2020, slope = slope_2020)+
	geom_point(alpha = 1/2)


ggplot(presvacc_2016)+aes(x=trump_pct, y = pct_vacc)+
	ggtitle("Proportion Vaccinated Versus Proportion Voting for Trump by County")+
	ylab("Proportion of Population Vaccinated") +
	xlab("Proportion of Votes Cast for Trump")+
	scale_color_manual(values = c("blue","red"))+
	geom_rect(mapping=aes(xmin=0, xmax=0.5, ymin=-0.01, ymax=1.01, fill=I('lightskyblue2')), color = 'lightskyblue2')+
	geom_rect(mapping=aes(xmin=0.5, xmax=1.0, ymin=-0.01, ymax=1.01, fill=I('lightsalmon2')), color = 'lightsalmon2')+
	theme(axis.text = element_text(size = 8), axis.title = element_text(size = 10), title = element_text(size = 12))+
	geom_abline(intercept = inter_2016, slope = slope_2016)+
	geom_point(alpha = 1/2)


