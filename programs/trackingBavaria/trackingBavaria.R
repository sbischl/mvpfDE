#----------------------------------------------------------------------------------------------------------------------#
# Tracking Reform Bavaria
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Piopiunik (2014)

trackingBavaria <- function (bootstrap_replication = 0, use_constant_ols_return_to_schooling = global_use_constant_ols_return_to_schooling) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Effect of introducing tracking on pisa score
  test_score_tracking_effect <- estimates$test_score_tracking_effect

  # Piopiunik (2014) refers to literature that estimates the number of pisa points you earn per year of schooling.
  # On page 23, there are references to two values:
  # "To get an idea of the effect size, note that the
  # competency increase in one school year is on average about
  # 40 points in PISA 2003 (cf. Prenzel et al., 2005) and 25–30
  # points in PISA 2006 (see Prenzel et al., 2008, p. 59)"
  pisa_points_per_year <- 30

  # We can use this to translate the reform into years of schooling lost / gained and then calculate the effect on earnigns.
  # Obviously that's a rather rough approximation and results in about .43 school years lost due tracking.
  relative_earnings_effect <- test_score_tracking_effect / pisa_points_per_year * yearly_return_to_schooling

  # Alternatively specify whats the value of increasing test scores by a standard deviation:
  sd_pisa <- 90.5 # Piopiunik (2014) Table 2, 2003, Bavaria, non-Gym
  # return_to_one_sd_test_score <- ???
  # relative_earnings_effect <- test_score_tracking_effect / sd_pisa * return_to_one_sd_test_score


  # From Piopiunik (2014):
    # The new school law of April 2000 did not require an
    # immediate implementation of the reform, but rather
    # allowed middle schools to add a fifth and sixth grade
    # level within several years. This provided schools time to
    # hire additional teachers and to build additional buildings, if
    # necessary. The state-wide implementation of six-year
    # middle schools was complete by school year 2003/2004
    # -> Make 2003 the reform year.
    prices_year <- 2003

  # Impact age = 18, makes sense given the data we have which starts at the age of 18. It shoud
  impact_age <- 18

  #--------------------------------------------------------------------------------------------------------------------#
  # Project and discount earnings & tax payments
  #--------------------------------------------------------------------------------------------------------------------#

  lifetime_impacts <- project_lifetime_impact(impact_age = 18,
                                              impact_magnitude = relative_earnings_effect,
                                              relative_control_income = getRelativeControlGroupEarnings("vocational_educ"),
                                              start_projection_year = 2003 + impact_age - 10,
                                              discount_to = 2003,
                                              prices_year = prices_year,
                                              inculde_welfare_benefits_fraction = 1)

  # Affected students value higher net-income
  net_income_increase <- lifetime_impacts$present_value_net_earnings_impact
  willingness_to_pay <- net_income_increase
  # Government costs are reduced by the increase in tax revenue
  tax_revenue_increase <- lifetime_impacts$present_value_tax_payment_impact
  government_net_costs <- - tax_revenue_increase

  #--------------------------------------------------------------------------------------------------------------------#
  # Cost of Reform
  #--------------------------------------------------------------------------------------------------------------------#
  # This is unkown really. Overall number of school years did not chagne. Looking at the time series of spending on schools,
  # we see that spending per student went down for middle track and increased for the basic track. Since the number of students
  # in both tracks is roughly identical, and the increase in basic is larger than the decrease in middle, spending
  # increased after the reform. But is this due to the reform? Hard to say ... could also be transitional cost or something
  # else entirely. Assume no cost for now. (See the Figure 4 of Piopiunik (2014))

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        net_income_increase = net_income_increase,
                        tax_revenue_increase = -tax_revenue_increase,
                        prices_year = prices_year)
  return(return_values)
}
