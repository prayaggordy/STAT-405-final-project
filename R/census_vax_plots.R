library(tidyr); library(dplyr); library(tidycensus)

plot_vax_by_region <- function(df,
															 xwalk_region) {
	df <- df %>%
		filter()
}

vax <- vaccination %>%
	filter(Date=="07/24/2021") %>%
	arrange(FIPS)
vax <- rename(vax, fips=FIPS)
census_vax <- left_join(census, vax, by="fips")

NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
						 "Rhode Island","Vermont","New Jersey","New York",
						 "Pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
						 "Iowa","Kansas","Minnesota","Missouri","Nebraska",
						 "North Dakota","South Dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
						 "ND","SD")
MW.ref <- c(MW.name,MW.abrv)

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
						"Maryland","North Carolina","South Carolina","Virginia",
						"West Virginia","Alabama","Kentucky","Mississippi",
						"Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
						"KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
						"Utah","Nevada","Wyoming","Alaska","California",
						"Hawaii","Oregon","Washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA",
						"HI","OR","WA")
W.ref <- c(W.name,W.abrv)

region.list <- list(
	Northeast=NE.ref,
	Midwest=MW.ref,
	South=S.ref,
	West=W.ref)

census_vax$regions <- sapply(census_vax$Recip_State,
										 function(x) names(region.list)[grep(x,region.list)])
census_vax2 <- select(census_vax, 1, c(3:20), c(22:24), 29, 31, 32, 34, 46)

ggplot(data=census_vax2) + aes(x=Series_Complete_18PlusPop_Pct) + geom_histogram(binwidth=10) +
	facet_wrap(~regions)
