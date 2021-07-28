library(knitr)
library(kableExtra)
library(purrr)
library(broom)
library(dplyr)
library(magrittr)

get_kable <- function(ttest) {
	map_df(list(ttest), tidy) %>%
		select(estimate, estimate1, estimate2, p.value, conf.low, conf.high, alternative) %>%
		kable(col.names = c("Difference in Means",
												"Mean, Vulnerable",
												"Mean, Not Vulnerable",
												"p value",
												"95% Confidence Lower Bound",
												"95% Confidence Upper Bound",
												"Alternative Hypothesis"))
}



