library(ggplot2); library(RSQLite); library(yaml)
config <- read_yaml("config.yaml")

presvacc <- get_vacc_pres_df(2020.0)
head(presvacc)
str(presvacc)
presvacc_2020 <- filter(presvacc, year > 2016)
head(presvacc_2020)


ggplot(presvacc)+aes(x=pct_trump, y = pct_vacc)+
	facet_wrap(~state, scales = "free_y", ncol = 4)+
	ggtitle("Pp")+
	ylab("Number of Complaints") +
	xlab("Submission Method")+
	scale_fill_manual(values = alpha(cbPalette)) +
	guides(fill = FALSE)+
	geom_bar(aes(submitted_via, fill = submitted_via))