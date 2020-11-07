#----------------------------------------------------------------------------------------------------------------------#
# Pension Information Letter:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Dolls et al. (2019)

expectedPensionLetter <- function (bootstrap_replication = 0, extend_effect = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  prices_year <- 2005 # Data from 2001 to 2010. The treatment occured in 2005

  # Dolls et al. (2019), Table 5 reports effect on earnings up to 3 years after receiving the letter
  gross_earnings_effect_year_0 <- estimates$gross_earnings_effect_year_0
  gross_earnings_effect_year_1 <- estimates$gross_earnings_effect_year_1
  gross_earnings_effect_year_2 <- estimates$gross_earnings_effect_year_2
  gross_earnings_effect_year_3 <- estimates$gross_earnings_effect_year_3

  # Earnings of control group conditional on being employed.
  earnings_control_group <- 24048.84 / 12 # Dolls et al. (2019) Table 3 "2005"

  # Average age of training participants:
  average_age <- 27 # Age of cut-off

  # The program cost is equal to sending the letter. Dolls et al. (2019) argue that aggregating the information needed to
  # send the letter has to be collected anyways (see footnote 22) -> Assume symbolic cost of 1€
  program_cost <- 1

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  reform_impact <- project_medium_run_impact(absolute_impact_magnitude = c(gross_earnings_effect_year_0,
                                                                           gross_earnings_effect_year_1,
                                                                           gross_earnings_effect_year_2,
                                                                           gross_earnings_effect_year_3),
                                             yearly_control_income = earnings_control_group * 12,
                                             number_of_periods = 4,
                                             prices_year = prices_year)

  # Assume individuals value their earnings change euro for euro. This is upper bound as individuals who earn
  # more also probably work more. The MVPF is infinity anyways due to the sizeable effect on tax revenue and the essentially
  # zero cost of sending the letter
  net_income_increase <- reform_impact$present_value_net_earnings_impact
  tax_revenue_increase <- reform_impact$present_value_tax_payment_impact

  government_net_costs <- -tax_revenue_increase
  willingness_to_pay <- net_income_increase

  #--------------------------------------------------------------------------------------------------------------------#
  # Letter Cost
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
