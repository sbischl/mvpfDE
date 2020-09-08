#----------------------------------------------------------------------------------------------------------------------#
# Start Up Grant - Follow Up Program to Start Up Subsidy and Briding Allowance after 2006 (Active Labor Market Policy):
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Caliendo et. al. (2016)

startupGrant <- function(bootstrap_replication = 0,
                         private_subsidy_valuation = 0,
                         effect_duration = 4, # How long the measured earnings gain persists
                         effect_delay = 2 # How long it takes for earnings gains to kick in
) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Number of particpants:
  participants_women <- 222 # Caliendo et. al. (2015) Table 2
  participants_men <- 367 # Caliendo et. al. (2015) Table 2
  participants <- participants_women + participants_men
  men_share <- (participants_men / participants)
  women_share <-  (participants_women / participants)

  # Average age of program participants:
  average_age_men <- 40.92 # Caliendo et. al. (2015) Table 3
  average_age_women <- 41.05 # Caliendo et. al. (2015) Table 3
  average_age <- women_share * average_age_women + men_share * average_age_men

  prices_year <- 2009 # Caliendo et. al. (2015) study entries into the start up subsidy program in the first quarter of 2009.

  # Average Unemployment Benefit before receiving the subsidy.
  unemployment_benefit_men <- 1093 # Caliendo et. al. (2015) Table 2
  unemployment_benefit_women <- 803 # Caliendo et. al. (2015) Table 2
  unemployment_benefit <- women_share * unemployment_benefit_women + men_share * unemployment_benefit_men


  # Income of Participants
  average_net_income_participants_men <- 3189 # Caliendo et. al. (2015) Table 4
  average_net_income_participants_women <- 1988 # Caliendo et. al. (2015) Table 4
  average_net_income_participants <- women_share * average_net_income_participants_women + men_share * average_net_income_participants_men

  # Net income of the matched control group
  average_net_income_control_men <- average_net_income_participants_men -  736.64
  average_net_income_control_women <- average_net_income_participants_women -  608.57
  average_net_income_control <-  women_share * average_net_income_control_women + men_share * average_net_income_control_men

  # Effect of program participation on net income
  net_income_effect_men <- estimates$net_earned_income_effect_men_40months
  net_income_effect_women <- estimates$net_earned_income_effect_women_40months
  net_income_effect <-  women_share * net_income_effect_women + men_share * net_income_effect_men



  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # To project the earnings, we need the effect of the start up subsidy on gross earnings:
  yearly_gross_income_control <- getGrossIncome(average_net_income_control *12)
  yearly_gross_income_treatment <- getGrossIncome((average_net_income_control + net_income_effect)*12)
  yearly_income_effect <- yearly_gross_income_treatment - yearly_gross_income_control

  # To make this comparable to the previous study by Caliendo & Künn (2011) who studied the predecessors of the
  # "Gründungszuschuss", i.e. ""Existenzgründungszuschuss" and "Überbrückungsgeld", only the 40 month 'long run'
  # effect is used although Caliendo et. al. (2015) also measure the impact on earnings 21 months after start up.
  reform_impact <- project_medium_run_impact(absolute_impact_magnitude = c(rep(0, effect_delay), rep(yearly_income_effect, effect_duration)),
                                             yearly_control_income = yearly_gross_income_control,
                                             number_of_periods = effect_delay + effect_duration,
                                             prices_year = prices_year)

  tax_revenue_increase <- - reform_impact$present_value_tax_payment_impact
  government_net_costs <- tax_revenue_increase
  net_income_increase <- reform_impact$present_value_net_earnings_impact
  willingness_to_pay <- net_income_increase

  #--------------------------------------------------------------------------------------------------------------------#
  # Effects of the Transfer
  #--------------------------------------------------------------------------------------------------------------------#

  # For the first 6 months beneficiaries receive their last payed unemployment benefit + 300 euro to cover for social security contributions.
  # After this initial period, they may receive 300 euro for another 9 months depending on whether their business is successful.
  subsidy_year_1 <- 6 * unemployment_benefit + 12 * 300
  subsidy_year_2 <- 3 * 300


  # Discounted cost of paying the subsidy for the government:
  subsidy <- sum(c(subsidy_year_1, subsidy_year_2) * discountVector(2))
  government_net_costs <- government_net_costs + subsidy

  # The valuation of the subsidy for the start up owners is unclear. Following the logic of the envelope
  # theorem the marginal start up founders are indifferent between founding the start up and looking for employment on the labor
  # market. However, one might argue that there are optimiztion frictions and individuals value the transfer. In this case
  # we would have to account for the additional costs that come with setting up a new company.
  # To get some idea, how the private valuation of the subsidy influence the results, private_subsidy_valuation denotes
  # the share of the subsidy the start up founders value euro for euro.
  subsidy_valuation <- private_subsidy_valuation * subsidy
  willingness_to_pay = willingness_to_pay + subsidy_valuation

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = subsidy,
                        tax_revenue_increase = tax_revenue_increase,
                        net_income_increase = net_income_increase)

  return(return_values)
}