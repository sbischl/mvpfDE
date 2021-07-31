#----------------------------------------------------------------------------------------------------------------------#
# Decentralization of Employment Services:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Mergele & Weber (2020)

decentralizedEmploymentServices <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  prices_year <- 2012

  # Mergele & Weber (2020) report the effect on log wages and log unemployment in Table 5 & 6
  log_earnings_effect <- estimates$log_wage_effect
  log_unemployment_duration_effect <- estimates$log_unemployment_duration_effect

  # Since the effects are on log outcomes, we need the levels from Table A3
  unemployment_duration_days <- 424.920
  earnings_control_group <- (32.699 * 365) / 12

  # Mergele & Weber (2020), Table A4
  average_age <- 44.532

  # Mergele & Weber (2020) provide no direct cost information. However, they state that "The financing of job centers
  # remained unaffected by the reform". Also the number of employees employed at decentralized agencies remained constant.
  # -> low or no direct cost
  program_cost <- NA

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # Only employed receive wages. Hence, the effect on wages is only relevant for those who are employed. Mergele & Weber (2020)
  # don't show how the exit to employment as a function of time. -> Assume a exponential distribution and use the average
  # unemployment distribution to approximate the job finding process.

  # The rate of the exponential distrubtion is equal to 1 / expected value. On average individuals find a job after
  # unemployment_duration_days / 365 years
  rate = 1 / (unemployment_duration_days / 365)

  # The authors oberve labor market outcomes up to 5 years after decentralization. The share of individuals working again
  # in years 1 to 5 post reform is given by:
  share_working <- sapply(1:5, pexp, rate = rate)

  # This is, when assuming that everyone returns to work. In this case, the about three percent increase in wages would make this a very
  # profitable reform (MVPF -> infinity). This would be in stark contrast to the conculusion of Mergele & Weber (2020) who argue that
  # the decentralization had a mostly negative impact on labor market outcomes and caused probably inefficient cost shifting
  # to higher levels of government. The authors put little weight on the increased wages. This makes sense since prior to the
  # considered unemployment spell, individuals affected by the reform only spent about one third of the year in employment.
  # The is no reason why the reform should increase employment. If anything the reported effects would suggest that the share
  # of employed on average at any given date is even lower now.
  # The average share of employed pre-reform can be calculated by averaging over the observed 5 years prior to the reform (see Table A3):
  share_working_pre_reform <- (121.547 + 122.508 + 124.669 + 122.531 + 133.493) / (5 * 365)

  # Then multiply the share of working (which acts as a upper bound on the share of working),
  # by the share returned to work under full employment:
  share_working <- share_working_pre_reform * share_working

  # The reported effects are over a time span of 5 years
  reform_impact <- project_medium_run_impact(impact_magnitude = log_earnings_effect,
                                             yearly_control_income = earnings_control_group * 12,
                                             number_of_periods = 5,
                                             prices_year = prices_year,
                                             inculde_welfare_benefits_fraction = 1,
                                             share_affected = share_working)

  net_income_increase <- reform_impact$present_value_net_earnings_impact
  tax_revenue_increase <- reform_impact$present_value_tax_payment_impact

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Extended Unemployment
  #--------------------------------------------------------------------------------------------------------------------#

  additional_days_unemployed <- log_unemployment_duration_effect * unemployment_duration_days

  # Cost of a year of unemployment is the transfer payed (which is equal to MINUS the tax payment with a gross income of zero)
  yearly_cost_unemployed <- getTaxPayment(gross_income = 0,
                                          prices_year = prices_year,
                                          inculde_welfare_benefits_fraction = 1,
                                          assume_flat_tax = FALSE)

  unemployment_benefits_cost <- -yearly_cost_unemployed * (additional_days_unemployed / 365)

  #--------------------------------------------------------------------------------------------------------------------#
  # MVPF Calculation
  #--------------------------------------------------------------------------------------------------------------------#

  government_net_costs <- -tax_revenue_increase + unemployment_benefits_cost
  willingness_to_pay <- net_income_increase

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        benefit_receipt = unemployment_benefits_cost,
                        net_income_increase =  net_income_increase,
                        tax_revenue_increase = -tax_revenue_increase,
                        prices_year = prices_year)
  return(return_values)
}
