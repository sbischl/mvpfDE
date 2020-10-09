# This File downloads the inverse Pareto Coefficient from the WID Database for the years 1990-2015

# There is a r package that can do this, which is on Github but not on CRAN:
# Need to install devtools and the package from github if not installed already:
# install.packages("devtools")
# devtools::install_github("WIDworld/wid-r-tool")

library(wid)
library(dplyr)


relevant_year <- 2001 # Year of the Tax Reform
top_tax_theshold <- 54998  # Income at which the top tax rate starts. (As defined by the tax schedule)

# The relevant threshold is the gross income at which the top tax rate is actually payed. Due to deductions,
# the gross income has to be significantly higher than top_tax_theshold.
# In 2020, the top tax rate starts at about 55000. But to actually pay the top tax rate the income has to
# be about 70000. This is roughly + 30%. Assume that this was also true in the 90s and 2000s.
relevant_threshold <- top_tax_theshold * 1.3



# The statistic of interest is the beta coefficient (which is equal to a / (a - 1)), where a is the Pareto coefficient
# We are interested pretax income. -> The correpsonding database key, is "b" for beta and ptinc for "pre tax income"
indicator = "bptinc"
# From the summary table at https://wid.world/summary-table/ this income is definied as follows:
# "Pre-tax national income is the sum of all pre-tax personal income flows accruing to the owners of the production factors,
# labor and capital, before taking into account the operation of the tax/transfer system, but after taking into account
# the operation of pension system. The central difference between personal factor income and pre-tax income is the treatment
# of pensions, which are counted on a contribution basis by factor income and on a distribution basis by pre-tax income."

# We also need the average income of each percentile
indicator <- c(indicator, "aptinc")


data <- download_wid(indicators = indicator,
             areas = "DE",
             years = 1990:2015)

# The authors of the WID fit a generalized Pareto distribution where the Pareto coefficient can vary with each percentile.
# We are interested in the percentile at which the top tax rate starts.
# -> The first step is to find the percentile that is closest to the threshold above which the top marginal rate has to be payed.
income_data <- data %>% filter(variable == "aptinc992j", year == relevant_year)

# Adjust price. From the documentation we know that all prices are in 2016 local currency.
income_data$value <- deflate(from = 2016, to = relevant_year) * income_data$value

closest_percentile <- income_data %>% filter(abs(value - relevant_threshold) == min(abs(value - relevant_threshold))) %>% select(percentile)

# Get the beta assosciated with the closest percentile
beta <- data %>% filter(variable == "bptinc992j", year == relevant_year, percentile == as.character(closest_percentile)) %>% select(value)
beta <- as.numeric(beta)
# beta = alpha / (alpha - 1) solved for alpha gives
alpha <- beta / (beta - 1)

cat("The relevant alpha in", relevant_year, "for a top tax starting at", top_tax_theshold, "Euro is equal to", alpha, "\n")