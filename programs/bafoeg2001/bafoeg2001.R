#----------------------------------------------------------------------------------------------------------------------#
# Bafög Reform 2001
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Baumgartner & Steiner (2006)


bafoeg2001 <- function (bootstrap_replication = 0,
                                       use_constant_ols_return_to_schooling = global_use_constant_ols_return_to_schooling) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#
  # Effect on enrollment, see Table 5:
  enrollment_effect <- estimates$enrollment_effect

  # The enrollment effect gives the effect on students who are eligible are eligible for tertiary education.
  # Unlike the 1990 reform, this bafoeg reform left the repayment and loan amount unchanged. However, the income threshold
  # above which students are no longer eligible for bafög was lowered. -> Bafög was expanded to children of richer parents.

  # Share of eligible students calculated from Baumgartner & Steiner (2006) Table 1:
  eligible_students_share_before <- (0.456 - 0.329) / 0.382
  eligible_students_share_after <- 0.329 / 0.618

  # Share of students who studied in the affected population before the Bafög reform:
  enrollment_share_before <- 0.64 # This comes from the text. On p. 15, Baumgartner & Steiner (2006) it says:
  # "If it were significant, this point estimate would imply that the average transition rate of those
  # affected by the reform increased by 1.5 percentage points. This would mean that, due to the
  # BAfoeG reform, the average transition rate into tertiary education of those eligible for
  # BAfoeG increased from about 64 percent (see Table A1) to about 65.5 percent."
  # But in Table A1, I cannot see anything that would gives us this information.

  average_bafoeg_2001 <- 365# Destatis 2003 Fachserie 11 Reihe 7 Bildung und Kultur, Tabelle 1.2.3 Studierende

  program_year <- 2001
  prices_year <- 2001

  average_age <- 19.421

  #--------------------------------------------------------------------------------------------------------------------#
  # Program Implementation Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # The cost of paying Bafög is given by:

  # 1) Present value of the payments:
  discounted_payment_bafoeg <- sum(rep(average_bafoeg_2001 * 12, 5) * discountVector(5))

  # 2) Minus the present value of the repayment
  # Assume that bafoeg recipients pay back the entire loan 5 years after they finished their degree.
  repayment <- 0.5 * (average_bafoeg_2001 * 12 * 5) * (1 + discount_rate)^(-(duration_of_study + 5))
  bafoeg_cost <- discounted_payment_bafoeg - repayment

  # The cost of the reform is given by the additional bafög recipients (or equivalently by the probability that someone
  # starts to receive Bafög because of the reform:
  program_cost <- (eligible_students_share_after - eligible_students_share_before) * (enrollment_share_before + enrollment_effect) * bafoeg_cost

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

  # (eligible_students_share_after - eligible_students_share_before) * enrollment_effect is the share of the population who
  # enrolls because of the bafög reform
  net_income_increase <- lifetime_impacts$present_value_net_earnings_impact *
    (eligible_students_share_after - eligible_students_share_before) * enrollment_effect
  tax_revenue_effect <- lifetime_impacts$present_value_tax_payment_impact *
    (eligible_students_share_after - eligible_students_share_before) * enrollment_effect

  #--------------------------------------------------------------------------------------------------------------------#
  # University Attendence Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # Cost Difference College / Vocational Degree:
  cost_difference <- costOfCollege(duration_of_study = duration_of_study, year = program_year, prices_year = prices_year) -
    costOfSchool(duration_of_schooling = duration_of_berufsschule, year = program_year, prices_year = prices_year, school_type = "berufsschule_dual")

  education_cost <-  cost_difference * (eligible_students_share_after - eligible_students_share_before) * enrollment_effect


  #--------------------------------------------------------------------------------------------------------------------#
  # Private costs of studying
  #--------------------------------------------------------------------------------------------------------------------#

  # This is currently missing. There are no tuition fees. Unless students study at private institutions. Yet, there are
  # still costs that would not be incurred if they did not study. However, these are probably small compared to

  #--------------------------------------------------------------------------------------------------------------------#
  # Reform Valuation
  #--------------------------------------------------------------------------------------------------------------------#

  # New Bafög recipients who would study anyways receive windfall gains because they can keep half of the grant. Students who
  # start studying because of the reform might be indifferent (Envelope Theorem) -> valuation 0.
  reform_valuation_inframarginal_students <- bafoeg_cost * (eligible_students_share_after - eligible_students_share_before) * enrollment_share_before

  # The willingness to pay and the government net cost are given by:
  government_net_costs <- program_cost + education_cost - tax_revenue_effect
  willingness_to_pay <- reform_valuation_inframarginal_students + net_income_increase

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        reform_valuation = reform_valuation_inframarginal_students,
                        net_income_increase = net_income_increase,
                        program_cost = program_cost,
                        tax_revenue_increase = -tax_revenue_effect,
                        education_cost = education_cost,
                        prices_year = prices_year)

  return(return_values)
}