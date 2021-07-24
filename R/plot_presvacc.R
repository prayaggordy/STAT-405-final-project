library(ggplot2); library(RSQLite); library(yaml); library(MASS)
config <- read_yaml("config.yaml")

presvacc <- filter(get_vacc_pres_df(2020.0), !is.na(FIPS))


rr.huber <- rlm(pct_vacc~trump_pct, data = presvacc)
resid_by_county <- data.frame(County = presvacc$County.y, resid = rr.huber$resid)
head(resid_by_county)

what <- filter(presvacc, pct_vacc <0.1)
what

ggplot(presvacc)+aes(x=trump_pct, y = pct_vacc)+
	ggtitle("Proportion Vaccinated Versus Proportion Voting for Trump by County")+
	ylab("Proportion of Population Vaccinated") +
	xlab("Proportion of Votes Cast for Trump")+
	scale_color_manual(values = c("blue","red"))+
	geom_rect(mapping=aes(xmin=0, xmax=0.5, ymin=-0.01, ymax=1.01, fill=I('lightskyblue2')), color = 'lightskyblue2')+
	geom_rect(mapping=aes(xmin=0.5, xmax=1.0, ymin=-0.01, ymax=1.01, fill=I('lightsalmon2')), color = 'lightsalmon2')+

	geom_point(alpha = 1/2)




