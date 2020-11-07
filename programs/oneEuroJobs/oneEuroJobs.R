#----------------------------------------------------------------------------------------------------------------------#
# One Euro Jobs:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Hohmeyer & Wolff (2010)

oneEuroJobs <- function (bootstrap_replication = 0, extend_effect = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Monthly Cost from Table 2, Hohmeyer & Wolff (2010):
  # The study looks at entries to the labor market programs in mid 2005. Take the average of monthly cost from 2005
  # and 2006:
  monthly_cost <- (386 + 337) / 2

  # Program Duration:
  program_duration <- 7 # Hohmeyer & Wolff (2010) Table 3, 6 to 7 months

  total_west <- 316.0
  total_east <- 287.9
  share_female_conditional_on_west <- 0.342
  share_female_conditional_on_east <- 0.449

  share_female_and_west <- total_west / (total_west + total_east) * share_female_conditional_on_west
  share_male_and_west <- total_west / (total_west + total_east) * (1 - share_female_conditional_on_west)
  share_female_and_east <- total_east / (total_west + total_east) * share_female_conditional_on_east
  share_male_and_east <- total_east / (total_west + total_east) * (1 - share_female_conditional_on_east)

  # Annual effect on gross earnings:
  earnings_effect_year1 <- share_female_and_west * estimates$monthly_gross_earnings_effect_female_west_year1 +
    share_male_and_west * estimates$monthly_gross_earnings_effect_male_west_year1 +
    share_female_and_east * estimates$monthly_gross_earnings_effect_female_east_year1 +
    share_male_and_east * estimates$monthly_gross_earnings_effect_male_east_year1

  earnings_effect_year2 <- share_female_and_west * estimates$monthly_gross_earnings_effect_female_west_year2 +
    share_male_and_west * estimates$monthly_gross_earnings_effect_male_west_year2 +
    share_female_and_east * estimates$monthly_gross_earnings_effect_female_east_year2 +
    share_male_and_east * estimates$monthly_gross_earnings_effect_male_east_year2

  earnings_effect_year3 <- share_female_and_west * estimates$monthly_gross_earnings_effect_female_west_year3 +
    share_male_and_west * estimates$monthly_gross_earnings_effect_male_west_year3 +
    share_female_and_east * estimates$monthly_gross_earnings_effect_female_east_year3 +
    share_male_and_east * estimates$monthly_gross_earnings_effect_male_east_year3
  
  # Annual effect on benefit payed:
  benefit_effect_year1 <- share_female_and_west * estimates$monthly_benefit_receipt_effect_female_west_year1 +
    share_male_and_west * estimates$monthly_benefit_receipt_effect_male_west_year1 +
    share_female_and_east * estimates$monthly_benefit_receipt_effect_female_east_year1 +
    share_male_and_east * estimates$monthly_benefit_receipt_effect_male_east_year1

  benefit_effect_year2 <- share_female_and_west * estimates$monthly_benefit_receipt_effect_female_west_year2 +
    share_male_and_west * estimates$monthly_benefit_receipt_effect_male_west_year2 +
    share_female_and_east * estimates$monthly_benefit_receipt_effect_female_east_year2 +
    share_male_and_east * estimates$monthly_benefit_receipt_effect_male_east_year2

  benefit_effect_year3 <- share_female_and_west * estimates$monthly_benefit_receipt_effect_female_west_year3 +
    share_male_and_west * estimates$monthly_benefit_receipt_effect_male_west_year3 +
    share_female_and_east * estimates$monthly_benefit_receipt_effect_female_east_year3 +
    share_male_and_east * estimates$monthly_benefit_receipt_effect_male_east_year3

  # Calculated from Table 5, see excel file in same Folder:
  average_age_female_west <- 36.4365
  average_age_male_west <- 36.6045
  average_age_female_east <- 39.671
  average_age_male_east <- 38.446

  average_age <- share_female_and_west * average_age_female_west +
    share_male_and_west * average_age_male_west +
    share_female_and_east * average_age_female_east +
    share_male_and_east * average_age_male_east

  prices_year <- 2005 # Hohmeyer & Wolff (2010), Footnote 31 "Nominal earnings were deflated by the consumer price index,
  # which was normalised to one in April 2005."

  control_income <- 1200

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  reform_impact <- project_medium_run_impact(absolute_impact_magnitude = c(earnings_effect_year1,
                                                                           earnings_effect_year2,
                                                                           earnings_effect_year3),
                                             yearly_control_income = control_income,
                                             number_of_periods = 3 + extend_effect,
                                             prices_year = prices_year,
                                             inculde_welfare_benefits_fraction = 0)
  # Do not include welfare benefits since these are separately estimated by the paper

  tax_revenue_increase <- reform_impact$present_value_tax_payment_impact
  government_net_costs <- tax_revenue_increase
  net_income_increase <- reform_impact$present_value_net_earnings_impact
  willingness_to_pay <- net_income_increase

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of reduced benefit payment
  #--------------------------------------------------------------------------------------------------------------------#
  discounted_effects_on_benefits <- c(c(benefit_effect_year1, benefit_effect_year2, benefit_effect_year3),
                                      rep(benefit_effect_year3, extend_effect)) * discountVector(3 + extend_effect)
  cumulated_discounted_effect_on_benefits <- sum(discounted_effects_on_benefits)

  government_net_costs <- government_net_costs + cumulated_discounted_effect_on_benefits

  #--------------------------------------------------------------------------------------------------------------------#
  # Program cost
  #--------------------------------------------------------------------------------------------------------------------#

  discounted_program_cost <- discountMonthlyCashFlow(amount = monthly_cost, months = program_duration)
  government_net_costs <- government_net_costs + discounted_program_cost

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        tax_revenue_increase =-tax_revenue_increase,
                        net_income_increase = net_income_increase,
                        program_cost = discounted_program_cost,
                        benefit_receipt = cumulated_discounted_effect_on_benefits,
                        prices_year = prices_year)
  return(return_values)
}