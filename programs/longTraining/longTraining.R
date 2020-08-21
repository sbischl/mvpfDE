#----------------------------------------------------------------------------------------------------------------------#
# Long Training (Active Labor Market Policy):
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Lechner et. al. (2011)

longTraining <- function (bootstrap_replication = 0, extend_effect = 0) {
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
  average_age <- 35.2 # Lechner et. al. (2011) Appendix Table B.1

  # Training cost:
  training_cost <- 9930 # Lechner et. al. (2011) Appendix Table 6

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # Simple earnings projection by evenly splitting the cumulative earnings gain over 8 years and adding
  # the gain to the control group average income

  reform_impact <- project_medium_run_impact(absolute_impact_magnitude = earnings_effect / 8,
                                              control_income = employment_rate * earnings_control_group * 12,
                                              number_of_periods = 8 + extend_effect,
                                              prices_year = prices_year)

  #--------------------------------------------------------------------------------------------------------------------#
  # Training cost
  #--------------------------------------------------------------------------------------------------------------------#

  government_net_costs <- government_net_costs + training_cost

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs)
  return(return_values)
}
