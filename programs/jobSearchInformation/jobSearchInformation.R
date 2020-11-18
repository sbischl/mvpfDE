#----------------------------------------------------------------------------------------------------------------------#
# Job Search Information Brochure:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Altmann et al. (2018)

jobSearchInformation <- function (bootstrap_replication = 0, extend_effect = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  prices_year <- 2011 # The experiment was conducted in 2010 / 2011. Earnings were measured one year after.

  # Altmann et al. (2018) reports cumulated earnings effects 1 year and 2 years after the treatment. The 2 years after
  # effect is super imprecise
  earnings_effect <- estimates$cumulative_earnings_effect_52weeks

  # Earnings of control group conditional on being employed.
  earnings_control_group <- (52.38 * 365) / 12 # Altmann et al. (2018) Table 1

  # Average age of training participants:
  average_age <- 36.92 # Altmann et al. (2018) Table 1

  # The program cost is virtually zero. According to Altmann et al. (2018) less then 1€ per brochure. Assume symbolic cost of 1€
  program_cost <- 1

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  reform_impact <- project_medium_run_impact(absolute_impact_magnitude = earnings_effect,
                                             yearly_control_income = earnings_control_group * 12,
                                             number_of_periods = 1 + extend_effect,
                                             prices_year = prices_year)

  # The effect on willingness to pay and government net cost is given by the increase in tax revenue and net earnings
  net_income_increase <- reform_impact$present_value_net_earnings_impact
  tax_revenue_increase <- reform_impact$present_value_tax_payment_impact

  government_net_costs <- -tax_revenue_increase
  willingness_to_pay <- net_income_increase

  #--------------------------------------------------------------------------------------------------------------------#
  # Brochure cost
  #--------------------------------------------------------------------------------------------------------------------#

  government_net_costs <- government_net_costs + program_cost

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = program_cost,
                        net_income_increase = net_income_increase,
                        tax_revenue_increase = -tax_revenue_increase,
                        prices_year = prices_year)
  return(return_values)
}
