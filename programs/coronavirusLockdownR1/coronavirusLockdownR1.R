#----------------------------------------------------------------------------------------------------------------------#
# Coronavirus Lockdown R = 0.627 vs R = 1
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Dorn et al. (2020)
coronavirusLockdownR1 <- function (bootstrap_replication = 0, only_risk_value = FALSE) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  # All of these calculations are obviously very very uncertain, and depend heavily on the made assumptions.

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions
  #--------------------------------------------------------------------------------------------------------------------#
  lockdown_cost_R0627 <- 287.6 * 10^9 + 1 / (1 + discount_rate) * 45.7 * 10^9 # Dorn et al. (2020) Appendix Table 1
  lockdown_cost_R1 <- 272.7 * 10^9 + 1 / (1 + discount_rate) * 239.2 * 10^9 # Dorn et al. (2020) Appendix Table 1
  lockdown_cost_difference <- lockdown_cost_R0627 - lockdown_cost_R1

  averted_deaths <-  22511 -  4940 # Dorn et al. (2020), p. 6
  population_germany <- 83 * 10^6

  # Value of a statistical life:
  risk_value_fatal <- global_risk_value_fatal
  resource_cost_fatal <- global_resource_cost_fatal

  if (only_risk_value) {
    risk_value_fatal <- risk_value_fatal + resource_cost_fatal
    resource_cost_fatal <- 0
  }

  # Technically this would be 2020 prices, but the currently used CPI from destatis does not include 2020 yet
  prices_year <- 2019

  #--------------------------------------------------------------------------------------------------------------------#
  # Willingness to Pay
  #--------------------------------------------------------------------------------------------------------------------#

  valuation_lower_risk_of_dying <- averted_deaths * (risk_value_fatal + (1 - global_flat_tax)  * resource_cost_fatal) / population_germany
  income_loss <- (1 - global_flat_tax) * lockdown_cost_difference / population_germany
  willingness_to_pay <- valuation_lower_risk_of_dying - income_loss

  #--------------------------------------------------------------------------------------------------------------------#
  # Government Net Cost
  #--------------------------------------------------------------------------------------------------------------------#

  fiscal_lockdown_cost <- lockdown_cost_difference * global_flat_tax / population_germany
  tax_revenue_effect <- averted_deaths * global_flat_tax * resource_cost_fatal / population_germany
  government_net_costs <- fiscal_lockdown_cost - tax_revenue_effect

  return_values <- list(willingness_to_pay = willingness_to_pay,
                        government_net_costs = government_net_costs,
                        valuation_lower_risk_of_dying = valuation_lower_risk_of_dying,
                        income_loss = income_loss,
                        program_cost = fiscal_lockdown_cost,
                        tax_revenue_increase = -tax_revenue_effect,
                        prices_year = prices_year)

  return(return_values)
}