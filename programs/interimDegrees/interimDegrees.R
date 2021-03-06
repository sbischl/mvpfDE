#----------------------------------------------------------------------------------------------------------------------#
# Interim Degrees (German High Track)
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Obergruber & Zierow (2020)

interimDegrees <- function (bootstrap_replication = 0, use_constant_ols_return_to_schooling = global_use_constant_ols_return_to_schooling) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  #program_cost <- 0

  # Effect of Interim Degree on completing high track?
  # This might be a overestimation because students may get Abitur later
  high_track_grad_effect <- estimates$high_track_grad_effect

  # The paper also report a effect on years of schooling completed.
  years_of_school_effect <- estimates$years_of_school_effect

  prices_year <- 2010 #Study looks at a survey from 2010. Does not really matter as calculations are agnostic to prices here.
  program_year <- 1978 # The reform's introduction was staggered between 1965 and 2003. Take the year in which
  # the largest federal state NRW introduced the reform, see Obergruber & Zierow (2020) Table 1
  impact_age <- 16 #Age when in 10th grade

  #--------------------------------------------------------------------------------------------------------------------#
  # Program Implementation Cost
  #--------------------------------------------------------------------------------------------------------------------#
  # no direct cost.

  #--------------------------------------------------------------------------------------------------------------------#
  # Project and discount earnings / tax payments when receiving higher track degree
  #--------------------------------------------------------------------------------------------------------------------#

  # As with the mentoring program assume that students upgrade from vocational education to abitur (without university)
  impact_magnitude_matrix <- getEducationEffectOnEarnings(education_decision = "abitur",
                                                          alternative = "vocational_educ")


  if (!use_constant_ols_return_to_schooling) {
    impact_more_education <- high_track_grad_effect *
      project_lifetime_impact(impact_age = 18,
                              impact_magnitude_matrix = impact_magnitude_matrix,
                              relative_control_income = getRelativeControlGroupEarnings("vocational_educ"),
                              start_projection_year = program_year + 18 - impact_age,
                              prices_year = prices_year,
                              discount_to = program_year,
                              inculde_welfare_benefits_fraction = 1)

  }
  else {
    # Use the effect of more years of schooling.
    impact_more_education <- project_lifetime_impact(impact_age = 18,
                                                     impact_magnitude = years_of_school_effect * yearly_return_to_schooling,
                                                     relative_control_income = getRelativeControlGroupEarnings("vocational_educ"),
                                                     start_projection_year = program_year + 18 - impact_age,
                                                     prices_year = prices_year,
                                                     discount_to = program_year,
                                                     inculde_welfare_benefits_fraction = 1)

  }
  # In any case substract the 0.7xx years of additional schooling from income.
  impact_longer_schooling <- project_lifetime_impact(impact_age = 18,
                                                     impact_magnitude = -1,
                                                     relative_control_income = getRelativeControlGroupEarnings("vocational_educ"),
                                                     end_projection_age = 18,
                                                     start_projection_year = program_year + 18 - impact_age,
                                                     prices_year = prices_year,
                                                     inculde_welfare_benefits_fraction = 0)


  # Add impact_longer_schooling and impact_more_education.
  lifetime_impacts <- years_of_school_effect * impact_longer_schooling + impact_more_education

  # Students value higher net-income
  net_income_increase <- lifetime_impacts$present_value_net_earnings_impact
  willingness_to_pay <- net_income_increase
  # Government costs are reduced by the increase in tax revenue
  tax_revenue_increase <- lifetime_impacts$present_value_tax_payment_impact
  government_net_costs <- -tax_revenue_increase

  #--------------------------------------------------------------------------------------------------------------------#
  # Cost of Schooling
  #--------------------------------------------------------------------------------------------------------------------#

  education_cost <- years_of_school_effect * costOfSchool(year = program_year, duration_of_schooling = 1, school_type = "gymnasium")
  government_net_costs <- government_net_costs + education_cost

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        net_income_increase = net_income_increase,
                        tax_revenue_increase = -tax_revenue_increase,
                        education_cost = education_cost,
                        program_cost = education_cost, # add this to make this includable in category averages.
                        prices_year = prices_year)
  return(return_values)
}