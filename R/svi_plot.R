library(ggplot2); library(RSQLite); library(yaml); library(MASS); library(grid); library(gridExtra); library(dplyr); library(magrittr)

get_vacc_SVI_df <- function(){
	vacc <- get_vaccine_df()

	db <- paste0(config$paths$proc, "finalproject.db")
	dcon <- dbConnect(SQLite(), dbname = db)

	res <- dbSendQuery(conn = dcon, "
	SELECT `Social Vulnerability Index (SVI)` as SVI, `SVI Category` as SVI_category, `FIPS Code` as FIPS
	FROM vaccine_hesitancy;
	")

	svi <- dbFetch(res, -1)
	dbClearResult(res)
	dbDisconnect(dcon)
	svi %>% inner_join(vacc) %>% filter(!is.na(FIPS) & !is.na(SVI) & !is.na(pct_vacc)) -> svi_vacc
	svi_vacc
}

get_vacc_SVI_cens_df <- function(svi_vacc){
	db <- paste0(config$paths$proc, "finalproject.db")
	dcon <- dbConnect(SQLite(), dbname = db)

	res <- dbSendQuery(conn = dcon, "
	SELECT total, percent_black, fips as FIPS
	FROM census;
	")

	cens <- dbFetch(res, -1)
	dbClearResult(res)
	dbDisconnect(dcon)
	cens %>% mutate(FIPS = as.numeric(FIPS)) %>% inner_join(svi_vacc) %>% filter(!is.na(FIPS) & !is.na(SVI) & !is.na(pct_vacc)) -> svi_vacc_cens
	svi_vacc_cens
}

get_vacc_svi_poli_df <- function(svi_vacc){
	presvacc <- filter(get_vacc_pres_df(2020.0), !is.na(FIPS) & !is.na(pct_vacc) & !is.na(trump_pct))
	inner_join(svi_vacc, presvacc)
}

plot_svi_vacc <- function(svi_vacc){
	ggplot(svi_vacc)+aes(x=SVI, y = pct_vacc)+
		ggtitle("Proportion Vaccinated Versus Social Vulnerability Index")+
		ylab("Proportion of Population Vaccinated") +
		xlab("Social Vulnerability Index")+
		theme_minimal()+
		geom_point(alpha = 1/3)
}

svi_facet_wrap <- function(svi_vacc){
	ggplot(transform(svi_vacc,
									 SVI_category=factor(SVI_category,levels=
									 											c("Very High Vulnerability","High Vulnerability",
									 												"Moderate Vulnerability", "Low Vulnerability",
									 												"Very Low Vulnerability"))), aes(fill = SVI_category)) +
		guides(fill=guide_legend(title="SVI Category"))+
		ggtitle("Vaccination by Social Vulnerability Index Category")+
		theme_minimal()+
		scale_fill_manual(values = c("darkred", "orangered2", "orange", "yellow", "greenyellow"))+
		xlab("Proportion of Population Vaccinated")+
		geom_histogram(aes(pct_vacc))+
		facet_wrap(~SVI_category)
}

high_vulnerability_race_plot <- function(svi_vacc_cens){
	vuln_counties_low_vacc <- filter(svi_vacc_cens, SVI >= 0.4 & pct_vacc <0.2)
	ggplot(vuln_counties_low_vacc, aes(x=percent_black, y=pct_vacc))+
		geom_point()
}

high_vulnerability_poli_plot <- function(vacc_svi_poli_df){
	vuln_counties_low_vacc <- filter(vacc_svi_poli_df, SVI >= 0.4 & pct_vacc <0.2)
	ggplot(vuln_counties_low_vacc, aes(x=trump_pct))+
		geom_histogram()
}

high_vulnerability_poli_plot(get_vacc_svi_poli_df(svi_vacc))



svi_vacc <- get_vacc_SVI_df()
svi_vacc_cens <- get_vacc_SVI_cens_df(svi_vacc)

svi_facet_wrap(svi_vacc)
high_vulnerability_race_plot(svi_vacc_cens)


