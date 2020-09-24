#----------------------------------------------------------------------------------------------------------------------#
# Start Up Subsidy (Active Labor Market Policy):
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Caliendo & Künn (2011)

bridgingAllowance <- function (bootstrap_replication = 0,
                            private_subsidy_valuation = 0,
                            effect_duration = 4, # How long the measured earnings gain persists
                            effect_delay = 2 # How long it takes for earnings gains to kick in
) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Subsidy Amounts, Caliendo & Künn (2011) Figure 1, Note
  yearly_subsidy <- (2056 * 12) / 2

  # Average age of training participants:
  average_age <- 40.17 # Caliendo & Künn (2011)  Table A1

  prices_year <- 2003 # Caliendo & Künn (2011) study entries into the start up subsidy program in the third quarter of 2003.

  # In Table 4, Caliendo & Künn (2011) report the working income defined as net income for the treated
  average_net_income_treatment <- 2336

  # From Caliendo & Künn (2011) Table 6 we know that the causal effect of receiving the bridging allowance subsidy program
  # is 618
  average_net_income_control <- average_net_income_treatment - 618

  # Effect of program participation on working income (=net income)
  net_income_effect <- estimates$working_income_effect


  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # To project the earnings, we need the effect of the brdiging allowance subsidy on gross earnings:
  yearly_gross_income_control <- getGrossIncome(average_net_income_control *12)
  yearly_gross_income_treatment <- getGrossIncome((average_net_income_control + net_income_effect)*12)
  yearly_income_effect <- yearly_gross_income_treatment - yearly_gross_income_control

  reform_impact <- project_medium_run_impact(absolute_impact_magnitude = c(rep(0, effect_delay), rep(yearly_income_effect, effect_duration)),
                                             yearly_control_income = yearly_gross_income_control,
                                             number_of_periods = effect_delay + effect_duration,
                                             prices_year = prices_year)


  tax_revenue_increase <- reform_impact$present_value_tax_payment_impact
  government_net_costs <- -tax_revenue_increase
  net_income_increase <- reform_impact$present_value_net_earnings_impact
  willingness_to_pay <- net_income_increase

  #--------------------------------------------------------------------------------------------------------------------#
  # Effects of the Transfer
  #--------------------------------------------------------------------------------------------------------------------#

  # Cost of paying the subsidy for the government:
  government_net_costs <- government_net_costs + yearly_subsidy

  # The valuation of the subsidy for the start up owners is unclear. Following the logic of the envelope
  # theorem the marginal start up founders are indifferent between founding the start up and looking for employment on the labor
  # market. However, one might argue that there are optimiztion frictions and individuals value the transfer. In this case
  # we would have to account for the additional costs that come with setting up a new company.
  # To get some idea, how the private valuation of the subsidy influence the results, private_subsidy_valuation denotes
  # the share of the subsidy the start up founders value euro for euro.
  subsidy_valuation <- private_subsidy_valuation * yearly_subsidy
  willingness_to_pay <- willingness_to_pay + subsidy_valuation

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = yearly_subsidy,
                        tax_revenue_increase = -tax_revenue_increase,
                        net_income_increase = net_income_increase,
                        prices_year = prices_year)

  return(return_values)
}