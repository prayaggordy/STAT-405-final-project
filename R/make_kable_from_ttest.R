library(knitr)
library(kableExtra)
library(purrr)
library(broom)
library(dplyr)
library(magrittr)

get_kable <- function(ttest) {
	map_df(list(ttest), broom::tidy) %>%
		dplyr::select(`Difference in Means` = estimate,
									`Mean, Vulnerable` = estimate1,
									`Mean, Not Vulnerable` = estimate2,
									`p value` = p.value,
									`95% Confidence Lower Bound` = conf.low,
									`95% Confidence Upper Bound` = conf.high,
									`Alternative Hypothesis` = alternative) %>%
		kable(digits = 3) %>%
		kableExtra::kable_styling(full_width = T)
}



