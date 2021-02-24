#----------------------------------------------------------------------------------------------------------------------#
# Tuition Fees:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Riphan (2012)

schoolFees <- function (bootstrap_replication = 0, use_constant_ols_return_to_schooling = global_use_constant_ols_return_to_schooling) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  prices_year <- 1962 # Use 1962 prices because the cpi does not reach further back.
  school_fee <- 240 # in DM
  dm_eur_conversion_rate <- 0.51129
  school_fee_eur <- school_fee * dm_eur_conversion_rate
  duration_of_school_alternative <- 6
  duration_of_school_advanced <- 9

  advanced_track_attendence_effect <- estimates$advanced_track_attendance_effect # This is simulated and comes from
  # Table 2, Estimation C

  # Introduction was staged, see Riphan (2012) Table 1. Use year of Baden-Württemberg as relatively large and
  # is also the median year
  program_year <- 1957

  #--------------------------------------------------------------------------------------------------------------------#
  # Direct effects of abolishing fees
  #--------------------------------------------------------------------------------------------------------------------#

  # Discounted value of the fee. This cost is borne by the public. Students or Parents (does not matter) value the
  # cost they no longer have to pay. To be perfectly accurate here we would have to distinguish between tracks
  # and account for the fact that students who attend the advanced school (Gymnasium) would have payed the fee longer.
  # However, we dont know what share would have gone to Gymnasium without the treatment, and there was a general
  # trend towards more people attending Gymnasiumm. Still at the time it was probably a rather small share. About 10 percent
  # Hard to find a value. Should be okay to ignore this for now.
  discounted_school_fee <- discount(from = 1957, to = prices_year) *
    sum(rep(school_fee_eur, duration_of_school_alternative) *
          discountVector(duration_of_school_alternative))

  # Government loses the fees.
  government_net_costs <- discounted_school_fee
  # Students no longer have to pay the fees.
  willingness_to_pay <- discounted_school_fee

  #--------------------------------------------------------------------------------------------------------------------#
  # Project and discount earnings / tax payments when attending higher track instead of one of the lower tracks
  #--------------------------------------------------------------------------------------------------------------------#

  # Use the earnings projection and assume that students get upgraded from vocational education to abitur...
  # There are two potential problems. a) Some of the students get university degrees. -> underestimation of the effect
  # b) We are using 2010 data to esimate returns to education in the 1960s.

  impact_magnitude_matrix <- getEducationEffectOnEarnings(education_decision = "abitur",
                                                          alternative = "vocational_educ")


  if (!use_constant_ols_return_to_schooling) {
    lifetime_impacts <- project_lifetime_impact(impact_age = 18,
                                                impact_magnitude_matrix = impact_magnitude_matrix,
                                                relative_control_income = getRelativeControlGroupEarnings("vocational_educ"),
                                                start_projection_year = program_year + 18 - 10, # Start projection in the year the are 18
                                                prices_year = prices_year,
                                                discount_to = program_year,
                                                inculde_welfare_benefits_fraction = 1)

  }
  else {
    # If we are using years of schooling, assume two three more years of schooling. Gymnasium entails 9 years. Realschule 6.
    impact_longer_schooling <- project_lifetime_impact(impact_age = 18,
                                                       impact_magnitude = -1,
                                                       relative_control_income = getRelativeControlGroupEarnings("vocational_educ"),
                                                       end_projection_age = 21,
                                                       start_projection_year = program_year + 18 - 10, # Start projection in the year the are 18
                                                       prices_year = prices_year,
                                                       discount_to = program_year,
                                                       inculde_welfare_benefits_fraction = 1)

    impact_more_education <- project_lifetime_impact(impact_age = 21,
                                                     impact_magnitude = 3 * yearly_return_to_schooling,
                                                     relative_control_income = getRelativeControlGroupEarnings("vocational_educ"),
                                                     start_projection_year = program_year + 18 - 10 + 3, # Start projection in the year the are 21
                                                     prices_year = prices_year,
                                                     discount_to = program_year,
                                                     inculde_welfare_benefits_fraction = 1)

    # Add impact_longer_schooling and impact_more_education.
    lifetime_impacts <- impact_longer_schooling + impact_more_education
  }


  # Students value higher net-income
  net_income_increase <- lifetime_impacts$present_value_net_earnings_impact * advanced_track_attendence_effect
  willingness_to_pay <- willingness_to_pay + net_income_increase

  # Government costs are reduced by the increase in tax revenue
  tax_revenue_increase <- lifetime_impacts$present_value_tax_payment_impact * advanced_track_attendence_effect
  government_net_costs <- government_net_costs - tax_revenue_increase

  #--------------------------------------------------------------------------------------------------------------------#
  # Cost of Schooling
  #--------------------------------------------------------------------------------------------------------------------#

  # Take into account that the students who go to Gymnasium now induce additional costs at the state level.
  # Same caveat from above applies. Since this reform was 60 years ago the data, our cost data might be inaccurate.
  # The earliest datapoint in our school cost data from destatis is from 1995. The cost will get deflated correctly, but
  # there have probably been changes in costs of schoolin in real terms between 1995 and 1957.
  cost_of_schooling_low_track <- costOfSchool(year = program_year,
                                              duration_of_schooling = 6,
                                              school_type = "realschule",
                                              prices_year = prices_year)
  cost_of_schooling_high_track <- costOfSchool(year = program_year,
                                               duration_of_schooling = 9,
                                               school_type = "gymnasium",
                                               prices_year = prices_year)

  cost_difference <- cost_of_schooling_high_track - cost_of_schooling_low_track
  education_cost <- cost_difference * advanced_track_attendence_effect
  government_net_costs <- government_net_costs + education_cost

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = discounted_school_fee,
                        education_cost = education_cost,
                        tax_revenue_increase = -tax_revenue_increase,
                        net_income_increase = net_income_increase,
                        prices_year = prices_year)

  return(return_values)
}
