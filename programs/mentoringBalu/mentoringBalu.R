#----------------------------------------------------------------------------------------------------------------------#
# Mentoring Program Balu und Du
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Falk et. al. (2020)

mentoringBalu <- function (bootstrap_replication = 0, use_constant_ols_return_to_schooling = FALSE) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Implementation cost (per student), see p. 22:
  implemenation_cost <- 1000

  # Effect on attendence of high track in 10th grade:
  high_track_attendence_10th <- estimates$high_track_attendence_10th

  prices_year <- 2013 #The panel comprises children born between 2002 and 2004. They were about 10 years old when the study was conducted

  #--------------------------------------------------------------------------------------------------------------------#
  # Program Implementation Cost
  #--------------------------------------------------------------------------------------------------------------------#
  government_net_costs <- implemenation_cost

  #--------------------------------------------------------------------------------------------------------------------#
  # Project and discount earnings / tax payments when attending higher track instead of one of the lower tracks
  #--------------------------------------------------------------------------------------------------------------------#

  impact_magnitude_matrix = getEducationEffectOnEarnings(education_decision = "abitur",
                                                         alternative = "vocational_educ")


  if (!use_constant_ols_return_to_schooling) {

    # The
    lifetime_impacts <- project_lifetime_impact(impact_age = 18,
                                                impact_magnitude_matrix = impact_magnitude_matrix,
                                                relative_control_income = 1,
                                                start_projection_year = 2021, # 2021 since students are 10 in 2013
                                                prices_year = prices_year,
                                                discount_to = 2013,
                                                inculde_welfare_benefits_fraction = 1)

  }
  else {
    # The high track entails 2 more years of schooling.
    impact_longer_schooling <- project_lifetime_impact(impact_age = 19,
                                                       impact_magnitude = -1,
                                                       relative_control_income = 1,
                                                       end_projection_age = 21,
                                                       start_projection_year = 2022, # 2022 since students are 10 in 2013
                                                       prices_year = prices_year,
                                                       discount_to = 2013,
                                                       inculde_welfare_benefits_fraction = 1)

    impact_more_education <- project_lifetime_impact(impact_age = 21,
                                                     impact_magnitude = 2 * yearly_returns_to_schooling,
                                                     relative_control_income = 1,
                                                     start_projection_year = 2024, # 2024 since students are 10 in 2013
                                                     prices_year = prices_year,
                                                     discount_to = 2013,
                                                     inculde_welfare_benefits_fraction = 1)

    # Add impact_longer_schooling and impact_more_education.
    lifetime_impacts <- impact_longer_schooling + impact_more_education
  }


  # Students value higher net-income
  willingness_to_pay <- lifetime_impacts$present_value_net_earnings_impact * high_track_attendence_10th
  # Government costs are reduced by the increase in tax revenue
  government_net_costs <- government_net_costs - lifetime_impacts$present_value_tax_payment_impact * high_track_attendence_10th


  #--------------------------------------------------------------------------------------------------------------------#
  # Cost of Schooling
  #--------------------------------------------------------------------------------------------------------------------#

  cost_of_schooling_low_track <- costOfSchool(year = 2013, duration_of_schooling = 6, school_type = "realschule")
  cost_of_schooling_high_track <- costOfSchool(year = 2013, duration_of_schooling = 8, school_type = "gymnasium")


  cost_difference <- cost_of_schooling_high_track - cost_of_schooling_low_track
  government_net_costs <- government_net_costs + cost_difference * high_track_attendence_10th

    return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs)
  return(return_values)
}
