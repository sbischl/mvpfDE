#----------------------------------------------------------------------------------------------------------------------#
# Tuition Fees:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Bruckmeier & Wigger (2014)
# Gorgen & Schienle (2019)

tuitionFees <- function (bootstrap_replication = 0, use_constant_ols_return_to_schooling = global_use_constant_ols_return_to_schooling) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#


  prices_year <- 2008
  tuition_fee <- 1000 #500 EUR per term. Some states allowed universities to set lower tuition fees. However, only very
  # few university did not set the maximum tuition fee of 500 EUR. (Bruckmeier & Wigger ,2014)


  enrollment_effect <- estimates$enrollment_rate_pp_bruckmeier_more_controls
  # Possible estimates of the effect of tuition fees on university enrollment (in percentage points) are:
  # enrollment_rate_pp_bruckmeier : about -3% from Bruckmeier & Wigger (2014)
  # enrollment_rate_pp_bruckmeier_more_controls : insignificant, about -1% from Bruckmeier & Wigger (2014)
  # enrollment_rate_pp_gorgen : about -4% from Gorgen & Schienle (2019)
  #
  # Bruckmeier & Wigger (2014) use DiD. Gorgen & Schienle (2019) also follow a DiD approach, but they select
  # the covariates using lasso and argue that using all available covariates (similar to Bruckmeier & Wigger (2014)) leads
  # to unreliable estimation because of high dimensionality of the data relative to the sample size.

  # College enrollment share, without tuition fees. On average somewhere between 0.4 and 0.5.
  # See Bruckmeier & Wigger (2014) Table 4
  enrollment_share <- 0.45

  # Share of students who are not excempted from tuition fees:
  # 31% were excempted in Bavaria (see Footnote 36 of Bruckmeier & Wigger (2014))
  not_excempted_share <- 0.7

  program_year <- 2008
  #--------------------------------------------------------------------------------------------------------------------#
  # Redistributive effect of charging tuition fees
  #--------------------------------------------------------------------------------------------------------------------#

  average_discounted_tuition_fee_per_college_student <- sum(rep(not_excempted_share * tuition_fee,  duration_of_study) * discountVector(5))
  # Government receives tuition fees from students who still study:
  government_net_costs <- - (enrollment_share + enrollment_effect) * average_discounted_tuition_fee_per_college_student
  # Students who still study have to pay the tuition fee:
  willingness_to_pay <- - (enrollment_share + enrollment_effect) * average_discounted_tuition_fee_per_college_student

  #--------------------------------------------------------------------------------------------------------------------#
  # University Attendence Costs
  #--------------------------------------------------------------------------------------------------------------------#

  # Cost Difference College / Vocational Degree:
  cost_difference <- costOfCollege(duration_of_study = duration_of_study, year = 2008, prices_year = prices_year) -
    costOfSchool(duration_of_schooling = duration_of_berufsschule, year = 2008, prices_year = prices_year, school_type = "berufsschule_dual")

  # Government saves more money because less students enroll
  education_cost <- enrollment_effect * cost_difference
  government_net_costs <- government_net_costs + education_cost

  #--------------------------------------------------------------------------------------------------------------------#
  # Project and discount earnings / tax payments when enrolling in college compared to a vocational degree.
  #--------------------------------------------------------------------------------------------------------------------#

  impact_magnitude_matrix <- getEducationEffectOnEarnings(education_decision = "university_degree",
                                                          alternative = "abitur")

  if (!use_constant_ols_return_to_schooling) {
    lifetime_impacts <- project_lifetime_impact(impact_age = age_university_enrollment,
                                                impact_magnitude_matrix = impact_magnitude_matrix,
                                                relative_control_income = getRelativeControlGroupEarnings("abitur"),
                                                start_projection_year = 2010,
                                                prices_year = prices_year,
                                                discount_to = program_year,
                                                inculde_welfare_benefits_fraction = 0)

  }
  else {
    # Alternative specification assuming simply 2 more years of schooling:
    impact_longer_schooling <- project_lifetime_impact(impact_age = age_university_enrollment + duration_of_berufsschule,
                                                       impact_magnitude = -1,
                                                       relative_control_income = getRelativeControlGroupEarnings("abitur"),
                                                       end_projection_age = age_university_enrollment + duration_of_berufsschule + additional_years_of_schooling_university - 1,
                                                       start_projection_year = 2010 + duration_of_berufsschule,
                                                       prices_year = prices_year,
                                                       discount_to = program_year,
                                                       inculde_welfare_benefits_fraction = 0)

    impact_more_education <- project_lifetime_impact(impact_age = age_university_enrollment + duration_of_berufsschule + 2,
                                                     impact_magnitude = 2 * yearly_return_to_schooling,
                                                     relative_control_income = getRelativeControlGroupEarnings("abitur"),
                                                     start_projection_year = 2010 + duration_of_berufsschule + additional_years_of_schooling_university,
                                                     prices_year = prices_year,
                                                     discount_to = program_year,
                                                     inculde_welfare_benefits_fraction = 0)
    # Add impact_longer_schooling and impact_more_education.
    lifetime_impacts <- impact_longer_schooling + impact_more_education
  }

  # Students value higher net-income
  net_income_increase <- lifetime_impacts$present_value_net_earnings_impact * enrollment_effect
  willingness_to_pay <- willingness_to_pay + net_income_increase
  # Government costs are reduced by the increase in tax revenue
  tax_revenue_increase <- lifetime_impacts$present_value_tax_payment_impact * enrollment_effect
  government_net_costs <- government_net_costs  - tax_revenue_increase

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = -average_discounted_tuition_fee_per_college_student,
                        education_cost = education_cost,
                        tax_revenue_increase = -tax_revenue_increase,
                        net_income_increase = net_income_increase,
                        prices_year = prices_year)

  return(return_values)
}
