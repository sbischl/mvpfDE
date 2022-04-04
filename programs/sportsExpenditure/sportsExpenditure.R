#----------------------------------------------------------------------------------------------------------------------#
# Sports Expenditure:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Pawlowski et al. (2019)
# published version Pawlowski et al. (2021) in Labor Economics

sportsExpenditure <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Program Cost is the difference in per capita sports expenditure between the high and low category as defined by
  # Pawlowski et al. (2019), see p. 14
  program_cost <- ((85 + 31) / 2 - (20 + 0) / 2) * 12

  # Pawlowski et al. (2019) do not report the average age. Only age brackets in their share in each of the subsamples:
  # Try to guess average age from those brackets:
  average_age <- 0.17 * ((20 + 29) / 2) + 0.41 * ((30 + 39)  / 2) + 0.42 * ((40 + 52) / 2) # Pawlowski et al. (2019) Table B2

  prices_year <- 2004

  # Share men
  share_men <- 3071 / (3071 + 3427)

  # What share of the population is working
  share_working_population <- 0.5

  # Pawlowski et al. (2019) Table B2
  average_income_treatment <- 2731

  # Effect of relocation Assistance on log wage 24 months after transition to employment:
  # This effect is crazy high. Spending about 50 € more on sports per capita per year increases gross earnings of men by
  # 260€ per month. No significant effect for women. This seems pretty unreasonably high, but this is the effect that is suggested by the paper.
  # And yes this is the monthly effect not yearly.
  gross_wage_effect_men <- estimates$earnings_effect_high_vs_low
  gross_wage_effect_women <- 0
  gross_wage_effect <- share_men * gross_wage_effect_men + (1- share_men) * gross_wage_effect_women

  # Calculate the earnings, individuals would have received had the government not invested into sports facilities
  average_income_control <- average_income_treatment - gross_wage_effect

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # The sports expenditure and the benefits occurs every year. When looking at Figure 2 (Figure 5 in pusblished verison)
  # it appears that there is delay of about 2 years of the expenditure to show its effect.
  # -> Have to discount by (1 + discount_factor)^-2, which is equivalent to assuming 0 effect for two years

  reform_impact <- project_medium_run_impact(absolute_impact_magnitude = c(0,0, gross_wage_effect * 12),
                                             yearly_control_income = average_income_control * 12,
                                             number_of_periods = 3,
                                             prices_year = prices_year)
  # The tax revenue increase and net income increase only affects the working population. The cost however is per capita.
  # So that includes children, students, retirees etc.
  tax_revenue_increase <- share_working_population * reform_impact$present_value_tax_payment_impact
  net_income_increase <-  share_working_population * reform_impact$present_value_net_earnings_impact

  #--------------------------------------------------------------------------------------------------------------------#
  # MVPF Calculation
  #--------------------------------------------------------------------------------------------------------------------#

  # Assume that invidivuals value the effect on net earnings.
  willingness_to_pay <- net_income_increase
  # Government net cost is the expenditure MINUS the effect on tax revenue
  government_net_costs <- program_cost - tax_revenue_increase

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = program_cost,
                        tax_revenue_increase = -tax_revenue_increase,
                        net_income_increase = net_income_increase,
                        prices_year = prices_year)

  return(return_values)
}