#----------------------------------------------------------------------------------------------------------------------#
# Rock Your Life
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Resnjanskij et al. (2021)
# Appendix H is particularly interesting as it already contains a cost-benefit analysis which uses the effect on math
# grades to project the effect on earnings.

rockYourLife <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Program Cost:
  # "Thus, direct program costs are roughly 750 EUR per mentee." - Resnjanskij et al. (2021), p. A78
  program_cost <- 750

  # Effect of mentoring on math grade:
  math_grade_effect <- estimates$math_grade_effect

  # Effect of higher grade on earnings:
  grade_earnings_effect <- estimates$grade_earnings_effect

  prices_year <- 2017 #The cost data is from 2017. And Resnjanskij et al. (2021) also note that they used 2017 prices
  # for their calculation, see p. A78

  average_age <- 13.98 # Table A4

  # Low SES share
  low_ses_share <- 0.47 # Table A4

  #--------------------------------------------------------------------------------------------------------------------#
  # Program Implementation Cost
  #--------------------------------------------------------------------------------------------------------------------#
  government_net_costs <- program_cost

  #--------------------------------------------------------------------------------------------------------------------#
  # Project and discount earnings / tax payments when attending higher track instead of one of the lower tracks
  #--------------------------------------------------------------------------------------------------------------------#
  # We replicate the approach of Resnjanskij et al. (2021) using our own data. To project the effect on lifetime earnings of the
  # mentoring program we take the effect of the mentoring on grades (math_grade_effect) and multiply with the effect of
  # the grade on log wages. That way we get a earnings increase in percent which we can use to project earnings. ~2.5%

  lifetime_impacts <- project_lifetime_impact(impact_age = 18,
                                              impact_magnitude = math_grade_effect * grade_earnings_effect,
                                              relative_control_income = getRelativeControlGroupEarnings("vocational_educ"),
                                              start_projection_year = 2017 + 18 - 14, # Most participants entered in 2017, they were about 14
                                              prices_year = prices_year,
                                              discount_to = 2017, # Discount to the year the cost of the mentor program accrues
                                              inculde_welfare_benefits_fraction = 1)
  # Students value higher net-income. Since there is a zero effect for non low-SES students have to multiply by low SES share
  net_income_increase <- lifetime_impacts$present_value_net_earnings_impact * low_ses_share
  willingness_to_pay <- net_income_increase
  # Government costs are reduced by the increase in tax revenue. Since there is a zero effect for non low-SES students have to multiply by low SES share
  tax_revenue_increase <- lifetime_impacts$present_value_tax_payment_impact * low_ses_share
  government_net_costs <- government_net_costs - tax_revenue_increase

  #--------------------------------------------------------------------------------------------------------------------#
  # Cost of Schooling
  #--------------------------------------------------------------------------------------------------------------------#
  # For the Balu und Du mentoring program, we had information about track choice. That's not available here. We cannot say
  # what additional educational costs there might be.

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = program_cost,
                        net_income_increase = net_income_increase,
                        tax_revenue_increase = -tax_revenue_increase,
                        prices_year = prices_year)
  return(return_values)
}
