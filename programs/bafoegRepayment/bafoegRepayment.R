#----------------------------------------------------------------------------------------------------------------------#
# Bafög Reform which changed Bafög from a loan (100% repayment) to partial subsidy (50% repayment)
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Baumgartner & Steiner (2006)

bafoegRepayment <- function (bootstrap_replication = 0,
                                       use_constant_ols_return_to_schooling = global_use_constant_ols_return_to_schooling) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  dm_eur_conversion_rate <- 0.51129

  # Effect on enrollment, see Table 5:
  enrollment_effect <- estimates$enrollment_effect_extended_sample

  # The enrollment effect gives the effect on students who are eligible for bafög and have the necessary degree from
  # secondary education to enroll at a university or technical college. See p. 7:
  # "We also restrict the sample to people who have completed upper secondary schooling,
  # since only these people are entitled to enroll into higher education at universities or technical colleges."

  # The enrollment rate we would have seen, had there been no reform. Again, this is the enrollment rate of high school
  # graduates who have the necessary degree to enroll and are eligible for bafög.
  enrollment_control_group <- 0.84 - 0.076

  average_bafoeg_1990 <- 596 * dm_eur_conversion_rate # Destatis 1991 Fachserie 11 Reihe 7 Bildung und Kultur, Table 1 : 1.3 Studenten - Früheres
  # Bundesgebiet

  program_year <- 1990
  prices_year <- 1990

  average_age <- 19.810

  #--------------------------------------------------------------------------------------------------------------------#
  # Program Implementation Cost
  #--------------------------------------------------------------------------------------------------------------------#

  discounted_payment_bafoeg <- sum(rep(average_bafoeg_1990 * 12, 5) * discountVector(5))
  # Assume that bafoeg recipients pay back the entire loan 5 years after they finished their degree.
  repayment_before_reform <- (average_bafoeg_1990 * 12 * 5) * (1 + discount_rate)^(-(duration_of_study + 5))
  repayment_after_reform <- 0.5 * (average_bafoeg_1990 * 12 * 5) * (1 + discount_rate)^(-(duration_of_study + 5))
  bafoeg_cost_before_reform <- discounted_payment_bafoeg - repayment_before_reform
  bafoeg_cost_after_reform <- discounted_payment_bafoeg - repayment_after_reform

  # The cost of the reform is twofold:
  # 1) Existing bafoeg recipients only have to pay back half
  program_cost <- (bafoeg_cost_after_reform - bafoeg_cost_before_reform) * enrollment_control_group
  # 2) More students enroll. -> More Bafög recipients
  program_cost_marginal <- enrollment_effect * bafoeg_cost_after_reform


  #--------------------------------------------------------------------------------------------------------------------#
  # Project and discount earnings / tax payments when enrolling in college compared to a vocational degree.
  #--------------------------------------------------------------------------------------------------------------------#

  impact_magnitude_matrix <- getEducationEffectOnEarnings(education_decision = "university_degree",
                                                          alternative = "abitur")

  if (!use_constant_ols_return_to_schooling) {
    lifetime_impacts <- project_lifetime_impact(impact_age = 20,
                                                impact_magnitude_matrix = impact_magnitude_matrix,
                                                relative_control_income = 1,
                                                start_projection_year = program_year,
                                                prices_year = prices_year,
                                                inculde_welfare_benefits_fraction = 0)

  }
  else {
    # Alternative specification assuming simply 2 more years of schooling:
    impact_longer_schooling <- project_lifetime_impact(impact_age = 20,
                                                       impact_magnitude = -1,
                                                       relative_control_income = 1,
                                                       end_projection_age = 21,
                                                       start_projection_year = program_year,
                                                       prices_year = prices_year,
                                                       inculde_welfare_benefits_fraction = 0)

    impact_more_education <- project_lifetime_impact(impact_age = 22,
                                                     impact_magnitude = 2* yearly_return_to_schooling,
                                                     relative_control_income = 1,
                                                     start_projection_year = program_year + 2,
                                                     prices_year = prices_year,
                                                     discount_to = program_year,
                                                     inculde_welfare_benefits_fraction = 0)
    # Add impact_longer_schooling and impact_more_education.
    lifetime_impacts <- impact_longer_schooling + impact_more_education
  }

  net_income_increase <- lifetime_impacts$present_value_net_earnings_impact * enrollment_effect
  tax_revenue_effect <- lifetime_impacts$present_value_tax_payment_impact * enrollment_effect

  #--------------------------------------------------------------------------------------------------------------------#
  # University Attendence Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # Cost Difference College / Vocational Degree:
  cost_difference <- costOfCollege(duration_of_study = duration_of_study, year = program_year, prices_year = prices_year) -
    costOfSchool(duration_of_schooling = duration_of_berufsschule, year = program_year, prices_year = prices_year, school_type = "berufsschule_dual")

  education_cost <-  enrollment_effect * cost_difference

  #--------------------------------------------------------------------------------------------------------------------#
  # Private costs of studying
  #--------------------------------------------------------------------------------------------------------------------#

  # This is currently missing. There are no tuition fees. Unless students study at private institutions. Yet, there are
  # still costs that would not be incurred if they did not study. However, these are probably small compared to

  #--------------------------------------------------------------------------------------------------------------------#
  # Reform Valuation
  #--------------------------------------------------------------------------------------------------------------------#

  # Bafög recipients who would study anyways receive windfall gains because they only have to half of the loan back.
  reform_valuation_inframarginal_students <- (repayment_before_reform - repayment_after_reform) * enrollment_control_group

  # The willingness to pay and the government net cost are given by:
  government_net_costs <- program_cost + program_cost_marginal + education_cost - tax_revenue_effect
  willingness_to_pay <- reform_valuation_inframarginal_students + net_income_increase

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        net_income_increase = net_income_increase,
                        bafoeg_valuation = reform_valuation_inframarginal_students,
                        bafoeg_cost = program_cost_marginal,
                        program_cost = program_cost,
                        tax_revenue_increase = -tax_revenue_effect,
                        education_cost = education_cost,
                        prices_year = prices_year)

  return(return_values)
}