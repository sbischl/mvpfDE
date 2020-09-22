#----------------------------------------------------------------------------------------------------------------------#
# Relocation Assistance (Active Labor Market Policy):
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Caliendo et al. (2011)

relocationAssistance <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Average Program Cost in 2006
  program_cost <- 1177 # Caliendo et al. (2011) p. 138

  # Average age of subsidy receivers:
  average_age <- 37.71 # Caliendo et al. (2011) Table 3

  prices_year <- 2006 # Caliendo et al. (2011) study entries into unemployment in 2005 & 2006

  # Caliendo et al. (2011) Table 3
  average_income_treatment <- (86.3 * 365) / 12

  # Effect of relocation Assistance on log wage 24 months after transition to employment:
  log_wage_effect_24months <- estimates$log_wage_effect_24months

  # Calculate the income, individuals would have received had they not received the relocation subsidy
  average_income_control <- average_income_treatment * (1 / (1 +log_wage_effect_24months))

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # Forecast the observed effect on log wage for 2 years
  reform_impact <- project_medium_run_impact(impact_magnitude = log_wage_effect_24months,
                                             yearly_control_income = average_income_control * 12,
                                             number_of_periods = 2,
                                             prices_year = prices_year)

  tax_revenue_increase <- reform_impact$present_value_tax_payment_impact
  net_income_increase <- reform_impact$present_value_net_earnings_impact

  #--------------------------------------------------------------------------------------------------------------------#
  # MVPF Calculation
  #--------------------------------------------------------------------------------------------------------------------#

  # Valuation of the subsidy is difficult. Individuals who would have moved anyways should value it euro for euro.
  # Individuals who are incentivized to move should be indifferent. (That is if everyone is rational and lending is possible without
  # restrictions) Problem: We do not know how many marginal / inframarginal movers there are. We only know the effect of
  # receiving the relocation subsidy.
  # -> Assume that subsidy covers for immediate moving costs (as intended by subsidy) and movers value
  # the effect on net income euro for euro
  willingness_to_pay <- net_income_increase
  government_net_costs <- program_cost - tax_revenue_increase

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = program_cost,
                        tax_revenue_increase = -tax_revenue_increase,
                        net_income_increase = net_income_increase)

  return(return_values)
}