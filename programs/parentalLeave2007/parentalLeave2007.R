#----------------------------------------------------------------------------------------------------------------------#
# Parental Leave Reform (Elterngeld) 2007
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Frodermann et al. (2020)
# Kluve & Tamm (2013)

parentalLeave2007 <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Net income of high and low income mothers before birth used to calculate
  # the subsidy amount. This is after birth. To calculate the "Elterngeld" the last net income before birth is relevant.
  # But since most Mothers probably return to the job they had before, the net incomes should approximately match
  net_income_high <- 1636.57 # Frodermann et al (2020) Table 13:
  net_income_low <- 645.56 # Frodermann et al (2020) Table 13:


  # Features of Erziehungsgeld (i.e. the predecessor of Elterngeld):
  previous_subsidy_amount <- 300
  previous_subsidy_period <- 24

  # The study reports effect for high and low income mothers:
  share_high_income_mothers <- 40075 / (40075 + 31328) # Frodermann et al. (2020) Appendix Table 10, t-1

  # Features of Elterngeld
  subsidy_amount <- 0.67 * (share_high_income_mothers * net_income_high + (1 - share_high_income_mothers) * net_income_low)
  subsidy_period <- 12


  # Control Means:
  low_income_mother_2before <- 24.04
  low_income_mother_1before <- 23.84
  low_income_mother_1after <- 15.08
  low_income_mother_2after <- 15.89
  low_income_mother_3after <- 18.30
  low_income_mother_4after <- 19.90
  low_income_mother_5after <- 21.74
  low_income_mother_6after <- 22.84
  low_income_mother_7after <- 23.51
  low_income_mother_8after <- 24.11
  low_income_mother_9after <- 27.81

  high_income_mother_2before <- 110.98
  high_income_mother_1before <- 118.16
  high_income_mother_1after <- 56.13
  high_income_mother_2after <- 60.05
  high_income_mother_3after <- 65.35
  high_income_mother_4after <- 68.03
  high_income_mother_5after <- 66.56
  high_income_mother_6after <- 67.25
  high_income_mother_7after <- 67.30
  high_income_mother_8after <- 67.30
  high_income_mother_9after <- 74.03

  # Effect on log earnings:
  log_daily_earnings_effect_2before_high_income_mother <- estimates$log_daily_earnings_effect_2before_high_income_mother
  log_daily_earnings_effect_1before_high_income_mother <- estimates$log_daily_earnings_effect_1before_high_income_mother
  log_daily_earnings_effect_1after_high_income_mother <- estimates$log_daily_earnings_effect_1after_high_income_mother
  log_daily_earnings_effect_2after_high_income_mother <- estimates$log_daily_earnings_effect_2after_high_income_mother
  log_daily_earnings_effect_3after_high_income_mother <- estimates$log_daily_earnings_effect_3after_high_income_mother
  log_daily_earnings_effect_4after_high_income_mother <- estimates$log_daily_earnings_effect_4after_high_income_mother
  log_daily_earnings_effect_5after_high_income_mother <- estimates$log_daily_earnings_effect_5after_high_income_mother
  log_daily_earnings_effect_6after_high_income_mother <- estimates$log_daily_earnings_effect_6after_high_income_mother
  log_daily_earnings_effect_7after_high_income_mother <- estimates$log_daily_earnings_effect_7after_high_income_mother
  log_daily_earnings_effect_8after_high_income_mother <- estimates$log_daily_earnings_effect_8after_high_income_mother
  log_daily_earnings_effect_9after_high_income_mother <- estimates$log_daily_earnings_effect_9after_high_income_mother

  log_daily_earnings_effect_2before_low_income_mother <- estimates$log_daily_earnings_effect_2before_low_income_mother
  log_daily_earnings_effect_1before_low_income_mother <- estimates$log_daily_earnings_effect_1before_low_income_mother
  log_daily_earnings_effect_1after_low_income_mother <- estimates$log_daily_earnings_effect_1after_low_income_mother
  log_daily_earnings_effect_2after_low_income_mother <- estimates$log_daily_earnings_effect_2after_low_income_mother
  log_daily_earnings_effect_3after_low_income_mother <- estimates$log_daily_earnings_effect_3after_low_income_mother
  log_daily_earnings_effect_4after_low_income_mother <- estimates$log_daily_earnings_effect_4after_low_income_mother
  log_daily_earnings_effect_5after_low_income_mother <- estimates$log_daily_earnings_effect_5after_low_income_mother
  log_daily_earnings_effect_6after_low_income_mother <- estimates$log_daily_earnings_effect_6after_low_income_mother
  log_daily_earnings_effect_7after_low_income_mother <- estimates$log_daily_earnings_effect_7after_low_income_mother
  log_daily_earnings_effect_8after_low_income_mother <- estimates$log_daily_earnings_effect_8after_low_income_mother
  log_daily_earnings_effect_9after_low_income_mother <- estimates$log_daily_earnings_effect_9after_low_income_mother

  prices_year <- 2013

  # Age in 2008 of mothers whose child was born in the first quarter 2007 (=Treatment Group)
  average_age <- 33.23 * share_high_income_mothers + 25.34 * (1 - share_high_income_mothers)
  average_earnings_before_birth <- (113.62* 365 * share_high_income_mothers + 22.18 * 365 * (1 - share_high_income_mothers)) / 12

  #--------------------------------------------------------------------------------------------------------------------#
  # Government Net Cost:
  #--------------------------------------------------------------------------------------------------------------------#

  elterngeld_benefit <- discountMonthlyCashFlow(subsidy_amount, subsidy_period)

  # Erziehungsgeld is means-tested. If the net income exceeds certain thresholds, the benefit is reduced.
  # During the first six months, couples whose combined income exceeded 30.000 € got reduced benefits
  # From month 7 till month 24 this threshold was 16500€. If it exceeded 22.086€ no benefit was payed.
  # It is hard to tell which share of individuals falls in which group. To simplify we assume that for the first six months
  # all received "Erziehungsgeld". For the months 7 - 24 only the low income mothers as defined in Frodermann et al. (2020)
  erziehungsgeld_benefit <- discountMonthlyCashFlow(previous_subsidy_amount, 6) +
    (1 - share_high_income_mothers) * discountMonthlyCashFlow(previous_subsidy_amount, 18)

    # The government has to pay the new benefit (Elterngeld) instead of the old scheme (Erziehungsgeld).
  program_cost <- elterngeld_benefit - erziehungsgeld_benefit
  government_net_costs <- program_cost

  # The parental leave reform had an impact on mothers earnings.
  # The effect prior to birth are ignored. These are zero, and should be zero because otherwise the
  # identification assumption of the paper would be violated.
  
  daily_earnings_effect_1after_high_income_mother <- exp(log_daily_earnings_effect_1after_high_income_mother + log(high_income_mother_1after)) - high_income_mother_1after
  daily_earnings_effect_2after_high_income_mother <- exp(log_daily_earnings_effect_2after_high_income_mother + log(high_income_mother_2after)) - high_income_mother_2after
  daily_earnings_effect_3after_high_income_mother <- exp(log_daily_earnings_effect_3after_high_income_mother + log(high_income_mother_3after)) - high_income_mother_3after
  daily_earnings_effect_4after_high_income_mother <- exp(log_daily_earnings_effect_4after_high_income_mother + log(high_income_mother_4after)) - high_income_mother_4after
  daily_earnings_effect_5after_high_income_mother <- exp(log_daily_earnings_effect_5after_high_income_mother + log(high_income_mother_5after)) - high_income_mother_5after
  daily_earnings_effect_6after_high_income_mother <- exp(log_daily_earnings_effect_6after_high_income_mother + log(high_income_mother_6after)) - high_income_mother_6after
  daily_earnings_effect_7after_high_income_mother <- exp(log_daily_earnings_effect_7after_high_income_mother + log(high_income_mother_7after)) - high_income_mother_7after
  daily_earnings_effect_8after_high_income_mother <- exp(log_daily_earnings_effect_8after_high_income_mother + log(high_income_mother_8after)) - high_income_mother_8after
  daily_earnings_effect_9after_high_income_mother <- exp(log_daily_earnings_effect_9after_high_income_mother + log(high_income_mother_9after)) - high_income_mother_9after

  daily_earnings_effect_1after_low_income_mother <- exp(log_daily_earnings_effect_1after_low_income_mother + log(low_income_mother_1after)) - low_income_mother_1after
  daily_earnings_effect_2after_low_income_mother <- exp(log_daily_earnings_effect_2after_low_income_mother + log(low_income_mother_2after)) - low_income_mother_2after
  daily_earnings_effect_3after_low_income_mother <- exp(log_daily_earnings_effect_3after_low_income_mother + log(low_income_mother_3after)) - low_income_mother_3after
  daily_earnings_effect_4after_low_income_mother <- exp(log_daily_earnings_effect_4after_low_income_mother + log(low_income_mother_4after)) - low_income_mother_4after
  daily_earnings_effect_5after_low_income_mother <- exp(log_daily_earnings_effect_5after_low_income_mother + log(low_income_mother_5after)) - low_income_mother_5after
  daily_earnings_effect_6after_low_income_mother <- exp(log_daily_earnings_effect_6after_low_income_mother + log(low_income_mother_6after)) - low_income_mother_6after
  daily_earnings_effect_7after_low_income_mother <- exp(log_daily_earnings_effect_7after_low_income_mother + log(low_income_mother_7after)) - low_income_mother_7after
  daily_earnings_effect_8after_low_income_mother <- exp(log_daily_earnings_effect_8after_low_income_mother + log(low_income_mother_8after)) - low_income_mother_8after
  daily_earnings_effect_9after_low_income_mother <- exp(log_daily_earnings_effect_9after_low_income_mother + log(low_income_mother_9after)) - low_income_mother_9after

  # Since many of the mothers are working part time and ~70% are married, the marginal income tax would depend heavily on the income of the father, which we do not have.
  # -> Use constant marginal tax rate
  tax_revenue_effect_1after <- global_flat_tax * (daily_earnings_effect_1after_high_income_mother * share_high_income_mothers + 
    daily_earnings_effect_1after_low_income_mother * (1- share_high_income_mothers))
  tax_revenue_effect_2after <- global_flat_tax * (daily_earnings_effect_2after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_2after_low_income_mother * (1- share_high_income_mothers))
  tax_revenue_effect_3after <- global_flat_tax * (daily_earnings_effect_3after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_3after_low_income_mother * (1- share_high_income_mothers))
  tax_revenue_effect_4after <- global_flat_tax * (daily_earnings_effect_4after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_4after_low_income_mother * (1- share_high_income_mothers))
  tax_revenue_effect_5after <- global_flat_tax * (daily_earnings_effect_5after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_5after_low_income_mother * (1- share_high_income_mothers))
  tax_revenue_effect_6after <- global_flat_tax * (daily_earnings_effect_6after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_6after_low_income_mother * (1- share_high_income_mothers))
  tax_revenue_effect_7after <- global_flat_tax * (daily_earnings_effect_7after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_7after_low_income_mother * (1- share_high_income_mothers))
  tax_revenue_effect_8after <- global_flat_tax * (daily_earnings_effect_8after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_8after_low_income_mother * (1- share_high_income_mothers))
  tax_revenue_effect_9after <- global_flat_tax * (daily_earnings_effect_9after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_9after_low_income_mother * (1- share_high_income_mothers))

  # The tax revenue increase covers the tax collected from the change in earnings. It is not entirely clear if the fact
  # that women return to the labor market earlier is already included here. Probably it is.
  tax_revenue_increase <- sum(c(tax_revenue_effect_1after, tax_revenue_effect_2after, tax_revenue_effect_3after, tax_revenue_effect_4after,
                            tax_revenue_effect_5after, tax_revenue_effect_6after, tax_revenue_effect_7after, tax_revenue_effect_8after,
                            tax_revenue_effect_9after) * 365 * discountVector(9))

  government_net_costs = government_net_costs - tax_revenue_increase

  # Since many of the mothers are working part time and ~70% are married, the marginal income tax would depend heavily on the income of the father, which we do not have.
  # -> Use constant marginal tax rate
  net_income_effect_1after <- (1 - global_flat_tax) * (daily_earnings_effect_1after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_1after_low_income_mother * (1- share_high_income_mothers))
  net_income_effect_2after <- (1 - global_flat_tax) * (daily_earnings_effect_2after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_2after_low_income_mother * (1- share_high_income_mothers))
  net_income_effect_3after <- (1 - global_flat_tax) * (daily_earnings_effect_3after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_3after_low_income_mother * (1- share_high_income_mothers))
  net_income_effect_4after <- (1 - global_flat_tax) * (daily_earnings_effect_4after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_4after_low_income_mother * (1- share_high_income_mothers))
  net_income_effect_5after <- (1 - global_flat_tax) * (daily_earnings_effect_5after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_5after_low_income_mother * (1- share_high_income_mothers))
  net_income_effect_6after <- (1 - global_flat_tax) * (daily_earnings_effect_6after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_6after_low_income_mother * (1- share_high_income_mothers))
  net_income_effect_7after <- (1 - global_flat_tax) * (daily_earnings_effect_7after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_7after_low_income_mother * (1- share_high_income_mothers))
  net_income_effect_8after <- (1 - global_flat_tax) * (daily_earnings_effect_8after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_8after_low_income_mother * (1- share_high_income_mothers))
  net_income_effect_9after <- (1 - global_flat_tax) * (daily_earnings_effect_9after_high_income_mother * share_high_income_mothers +
    daily_earnings_effect_9after_low_income_mother * (1- share_high_income_mothers))

  net_income_increase <- sum(c(net_income_effect_1after, net_income_effect_2after, net_income_effect_3after, net_income_effect_4after,
                              net_income_effect_5after, net_income_effect_6after, net_income_effect_7after, net_income_effect_8after,
                              net_income_effect_9after) * 365 * discountVector(9))

  willingness_to_pay <- net_income_increase

  #--------------------------------------------------------------------------------------------------------------------#
  # Willingness to Pay:
  #--------------------------------------------------------------------------------------------------------------------#

  # Assume that the valuation is equal to the increase in subsidy payed + the effect on net earnings. An argument could
  # be made claiming that the earnings change is somehow based on changed effort or labor supply and the result of an
  # optimization problem in which case the evelope theorem would apply, and the WTP for the changed earnings would be zero.
  # However, this would be against the results of the paper which explicitly says that the increased earnings do not
  # come from increased labor supply.
  subsidy_valuation <- program_cost
  willingness_to_pay <- willingness_to_pay + subsidy_valuation

  # All considered effects only take the mother into account. The reform also includes a so-called daddy bonus. The daddy bonus
  # allows the partner who did not make use of the full 12 months to take two months off and also receive the same subsidy of 67%
  # of the last net wage. It might be possible that some of the positive effect on earnings is offset by lower earnings of the
  # the father. However, Kluve & Tamm (2013) find no significant effect on fathers' labor supply.

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        net_income_increase = net_income_increase,
                        tax_revenue_increase = - tax_revenue_increase,
                        program_cost = program_cost,
                        prices_year = prices_year)

  return(return_values)
}