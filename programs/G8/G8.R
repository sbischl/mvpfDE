#----------------------------------------------------------------------------------------------------------------------#
# G8
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Marcus & Zambre (2019)

G8 <- function (bootstrap_replication = 0,
                use_constant_ols_return_to_schooling = global_use_constant_ols_return_to_schooling) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # G8 reduces shortens high school from 9 to 8 years. As a result, students may start studying one year earlier and can
  # join the labor force earlier:
  # Marcus & Zambre (2019) estimate that students
  earlier_labor_force_participation <- 0.709

  # The shortening of high school prolongs the time frame in which relevant incomes can be earned by
  # earlier_labor_force_participation. It is unclear what the income should be in this additional period.
  # With G8 all incomes are earned one year earlier which is equivalent to receiving the lifetime income one year earlier.
  # This would generate huge gains because of lower discounting.

  # Assume that the value of working one year earlier is given by the income at age 30 without discounting:
  income_during_additional_period_no_college <- getAverageIncome(age = 30, education = "abitur")
  income_during_additional_period_college <- getAverageIncome(age = 30, education = "university_degree")

  # College Share with G8, see Footnote 36 Marcus & Zambre (2019)
  share_college_g8 <- 0.68

  # Effect of G8 on university enrollment
  enrollmentrate_change_pp <- estimates$enrollment_rate_change_pp
  # Effect of G8 on university drop out
  drop_out_pp <- estimates$drop_out_pp

  # The Paper looks at G8 enactment which was spread over almost ten years.
  # Since there are no cost estimates in the paper, it does not really matter which prices are used.
  prices_year <- 2011

  #--------------------------------------------------------------------------------------------------------------------#
  # Program Implementation Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # The number of hours taught at high school remains roughly unchanged. Cost of schooling remains unchanged

  # The fact that less students enroll at university reduces costs:
  # Cost Difference College / Vocational Degree:
  cost_difference <- costOfCollege(duration_of_study = 5, year = 2011, prices_year = prices_year) -
    costOfSchool(duration_of_schooling = 3, year = 2011, prices_year = prices_year, school_type = "berufsschule_dual")

  education_cost <- cost_difference * enrollmentrate_change_pp
  government_net_costs <- education_cost

  #--------------------------------------------------------------------------------------------------------------------#
  # Effects of earlier Employment
  #--------------------------------------------------------------------------------------------------------------------#
  one_additional_year_net_income <-
    share_college_g8 * (income_during_additional_period_college - getTaxPayment(income_during_additional_period_college,
                                                                                prices_year = prices_year)) +
      (1 - share_college_g8) * (income_during_additional_period_no_college - getTaxPayment(income_during_additional_period_college,
                                                                                           prices_year = prices_year))

  one_additional_year_tax_payment <- share_college_g8 * getTaxPayment(income_during_additional_period_college,
                                                                      prices_year = prices_year) +
    (1 - share_college_g8) * getTaxPayment(income_during_additional_period_college,
                                           prices_year = prices_year)


  earlier_labor_market_participation_tax_revenue <- one_additional_year_tax_payment * earlier_labor_force_participation
  earlier_labor_market_participation_net_income <- one_additional_year_net_income * earlier_labor_force_participation
  willingness_to_pay <- earlier_labor_market_participation_net_income
  government_net_costs <- government_net_costs - earlier_labor_market_participation_tax_revenue



  #--------------------------------------------------------------------------------------------------------------------#
  # Effects of lower enrollment rate and higher drop out
  #--------------------------------------------------------------------------------------------------------------------#

  impact_magnitude_matrix <- getEducationEffectOnEarnings(education_decision = "university_degree",
                                                          alternative = "abitur")

  if (use_constant_ols_return_to_schooling) {
    lifetime_impacts <- project_lifetime_impact(impact_age = 21,
                                                impact_magnitude_matrix = impact_magnitude_matrix,
                                                relative_control_income = 1,
                                                start_projection_year = 2014,
                                                prices_year = prices_year,
                                                discount_to = 2011,
                                                inculde_welfare_benefits_fraction = 0)
  }
  else {
    impact_longer_schooling <- project_lifetime_impact(impact_age = 21,
                                                       impact_magnitude = -1,
                                                       relative_control_income = 1,
                                                       end_projection_age = 22,
                                                       start_projection_year = 2011,
                                                       prices_year = prices_year,
                                                       inculde_welfare_benefits_fraction = 0)

    impact_more_education <- project_lifetime_impact(impact_age = 23,
                                                     impact_magnitude = 2* yearly_return_to_schooling,
                                                     relative_control_income = 1,
                                                     start_projection_year = 2011,
                                                     prices_year = prices_year,
                                                     discount_to = 2011,
                                                     inculde_welfare_benefits_fraction = 0)

    lifetime_impacts <- impact_longer_schooling + impact_more_education
  }

  # Students value higher net-income
  net_income_increase <- lifetime_impacts$present_value_net_earnings_impact * (enrollmentrate_change_pp - drop_out_pp)
  willingness_to_pay <- willingness_to_pay + net_income_increase
  # Government costs are reduced by the increase in tax revenue
  tax_revenue_increase <- lifetime_impacts$present_value_tax_payment_impact * (enrollmentrate_change_pp - drop_out_pp)
  government_net_costs <- government_net_costs - tax_revenue_increase

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        education_cost = education_cost,
                        net_income_increase = net_income_increase,
                        earlier_labor_market_participation_net_income = earlier_labor_market_participation_net_income,
                        tax_revenue_increase = -tax_revenue_increase,
                        earlier_labor_market_participation_tax_revenue = -earlier_labor_market_participation_tax_revenue,
                        program_cost = 0,
                        prices_year = prices_year)
  return(return_values)
}
