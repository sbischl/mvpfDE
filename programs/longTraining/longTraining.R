#----------------------------------------------------------------------------------------------------------------------#
# Long Training (Active Labor Market Policy):
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Lechner et. al. (2011)

longTraining <- function (bootstrap_replication = 0, use_constant_ols_return_to_schooling = FALSE) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#


  prices_year <- 1993 #Autors use a dataset containing individuals who participated in an government sponsored
  # training program between 1992 and 1994, see p.774

  # Unfortunaltely, the paper does not report the earnings change for each year / month after training. Instead only
  # the cumulated earnigs change is reported. The earnings_effect denotes how much more euro an individual who participated in
  # long training earned on average over the following 8 years compared to the matched individuals who did not participate
  # in any training program.
  earnings_effect <- estimates$earnings_effect_8years_long_training

  # Earnings of control group conditional on being employed.
  earnings_control_group <- 1396 # Lechner et. al. (2011) Table 5.

  # Long run employment rate of individuals who do not participate in any training program:
  employment_rate <- 0.9 # This can be inferred from Figure 2 depicting the share of unemployed over time in Lechner et. al. (2011)

  # Average age of training participants:
  average_age <- round(35.2) # Lechner et. al. (2011) Appendix Table B.1

  # Training cost:
  training_cost <- 9930 # Lechner et. al. (2011) Appendix Table 6

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # Simple earnings projection by evenly splitting the cumulative earnings gain over 8 years and adding
  # the gain to the control group average income
  yearly_earnings_training <- (earnings_effect / 8) + 12 * earnings_control_group * employment_rate
  yearly_earnings_no_training <- 12 * earnings_control_group * employment_rate

  yearly_tax_training <- getTaxPayment(yearly_earnings_training, prices_year = prices_year)
  yearly_net_earnings_training <- yearly_earnings_training - yearly_tax_training

  yearly_tax_no_training <- getTaxPayment(yearly_earnings_no_training, prices_year = prices_year)
  yearly_net_earnings_no_training <- yearly_earnings_no_training - yearly_tax_no_training

  government_net_costs <- - sum(rep(yearly_tax_training - yearly_tax_no_training, 8) * discountVector(8))
  willingness_to_pay <- sum(rep(yearly_net_earnings_training - yearly_net_earnings_no_training, 8) * discountVector(8))

  # Alternative: Use the income projection which assumes changing wages (because of economic growth and increasing age)
  # Since wages grow, but the impact magnitude is calculated from the control group wage in the first period, the
  # total effec is overestimated.

  relative_earnings_impact <- (earnings_effect / 8) / (12 * earnings_control_group * employment_rate)
  lifetime_impact <- project_lifetime_impact(impact_age = average_age,
                                             impact_magnitude = relative_earnings_impact,
                                             control_income = earnings_control_group * employment_rate * 12,
                                             start_projection_year = 1993,
                                             end_projection_age = average_age + 7,
                                             prices_year = prices_year)

  #willingness_to_pay <- lifetime_impact$present_value_net_earnings_impact
  #government_net_costs <- - lifetime_impact$present_value_tax_payment_impact

  #--------------------------------------------------------------------------------------------------------------------#
  # Training cost
  #--------------------------------------------------------------------------------------------------------------------#

  government_net_costs <- government_net_costs + training_cost

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs)
  return(return_values)
}
