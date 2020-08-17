#----------------------------------------------------------------------------------------------------------------------#
# Best Up College Information Workshop
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Peter et. al. (2018)

BestUpInformationWorkshop <- function (bootstrap_replication = 0,
                                       use_constant_ols_return_to_schooling = FALSE) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Implementation cost (per student), see Table A4 :
  set_up_cost <- 4.54 #Preparing the workshop
  implementation_cost <- 1.34 #Deliviring the presentation
  total_cost <- 5.87

  # Effect on enrollment, see Table 5:
  enrollment_effect_1year_all <- estimates$enrollment_effect_1year_all

  # Effect on Bafoeg application rate, see Table 8
  bafoeg_application <- estimates$bafoeg_application
  average_bafoeg_2014 <- 448 #Destatis 2014 Fachserie 11 Reihe 7 Bildung und Kultur

  program_year <- 2013 # The workshop took place in 2013, one year prior to graduation.
  prices_year <- 2013

  duration_of_study <- 5
  duration_of_berufsschule <- 3

  #--------------------------------------------------------------------------------------------------------------------#
  # Program Implementation Cost
  #--------------------------------------------------------------------------------------------------------------------#

  government_net_costs <- total_cost

  #--------------------------------------------------------------------------------------------------------------------#
  # Project and discount earnings / tax payments when enrolling in college compared to a vocational degree.
  #--------------------------------------------------------------------------------------------------------------------#

  impact_magnitude_matrix = getEducationEffectOnEarnings(education_decision = "university_degree",
                                                         alternative = "abitur")

  if (!use_constant_ols_return_to_schooling) {
    lifetime_impacts <- project_lifetime_impact(impact_age = 20,
                                                impact_magnitude_matrix = impact_magnitude_matrix,
                                                relative_control_income = 1,
                                                start_projection_year = 2014,
                                                prices_year = prices_year,
                                                inculde_welfare_benefits_fraction = 0)

  }
  else {
    # Alternative specification assuming simply 2 more years of schooling:
    impact_longer_schooling <- project_lifetime_impact(impact_age = 20,
                                                       impact_magnitude = -1,
                                                       relative_control_income = 1,
                                                       end_projection_age = 21,
                                                       start_projection_year = 2014,
                                                       prices_year = prices_year,
                                                       inculde_welfare_benefits_fraction = 0)

    impact_more_education <- project_lifetime_impact(impact_age = 22,
                                                     impact_magnitude = 2* yearly_returns_to_schooling,
                                                     relative_control_income = 1,
                                                     start_projection_year = 2016,
                                                     prices_year = prices_year,
                                                     discount_to = 2014,
                                                     inculde_welfare_benefits_fraction = 0)
    # Add impact_longer_schooling and impact_more_education.
    lifetime_impacts <- impact_longer_schooling + impact_more_education
  }

  # Peter et. al. (2018) measure the effect of their information workshop on the probability of college enrollment using
  # a linear probability model. Hence, the reported estimate represents the effect on the probability of enrollment in
  # percentage points. On average, the probability that a student changes her behavior because of the treatment
  # (i.e. enrolls but would have enrolled in absence of the information workshop) is given be the estimated effect
  # on probability of enrollment.

  # Students value higher net-income
  willingness_to_pay <- lifetime_impacts$present_value_net_earnings_impact * enrollment_effect_1year_all
  # Government costs are reduced by the increase in tax revenue
  government_net_costs <- government_net_costs - lifetime_impacts$present_value_tax_payment_impact * enrollment_effect_1year_all

  #--------------------------------------------------------------------------------------------------------------------#
  # University Attendence Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # Cost Difference College / Vocational Degree:
  cost_difference <- costOfCollege(duration_of_study = duration_of_study, year = 2014, prices_year = prices_year) -
    costOfSchool(duration_of_schooling = duration_of_berufsschule, year = 2014, prices_year = prices_year, school_type = "berufsschule_dual")


  government_net_costs <- government_net_costs + enrollment_effect_1year_all * cost_difference

  # Bafoeg
  discounted_cost_bafoeg <- sum(rep(average_bafoeg_2014 * 12, 5) * discountVector(5))
  government_net_costs <- government_net_costs + discounted_cost_bafoeg * bafoeg_application

  # Bafög is a transfer to students. Students value it euro for euro.
  # They would not receive it if they did an apprenticeship
  willingness_to_pay <- willingness_to_pay + discounted_cost_bafoeg * bafoeg_application

  # Assume that 50 percent of Bafög has to be payed back 5 years after finishing the degree.
  repayment_bafoeg <- 0.5*(average_bafoeg_2014 * 12 * 5) * (1 + discount_rate)^(-9)
  willingness_to_pay <- willingness_to_pay - repayment_bafoeg * bafoeg_application
  government_net_costs <- government_net_costs - repayment_bafoeg * bafoeg_application

  #--------------------------------------------------------------------------------------------------------------------#
  # Private costs of studying
  #--------------------------------------------------------------------------------------------------------------------#

  # This is currently missing. There are no tuition fees. Unless students study at private institutions. Yet, there are
  # still costs that would not be incurred if they did not study. However, these are probably small compared to 

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        cost_difference_between_college_and_vocational_degree = cost_difference)
  return(return_values)
}