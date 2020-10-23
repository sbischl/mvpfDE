#----------------------------------------------------------------------------------------------------------------------#
# Unemployment Benefit Extension Age 49:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Caliendo et al. (2013)
# Schmieder & von Wachter (2016)

# Caliendo et al. (2013) make use of a discontinuity in the German unemployment benefit system:
# At the age of 45 the entitlement to UI benefits increases from 12 to 18 months

unemploymentBenefits2002 <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#
  max_benefit_duration <- 18
  max_benefit_duration_before_cutoff <- 12

  prices_year <-2002 # Autors use a dataset containing observations between 2001 and 2003

  # Caliendo et al. (2013) report effects for men and womens separately:
  share_women <- 2776 / (2241 + 2776) # Table 2

  # Caliendo et al. (2013) do not estimate the marginal effect of extending the benefit duration on unemployment duration
  # directly. They only simulate (whatever that means) the impact of the reform on unemployment duration (p. 624 Section 5.5)
  # They also report different estimates for homogenous & heterogenous treatment effects.
  # The homogenous treatment effect simulation implies:
  non_employment_duration_women <- 11.3
  non_employment_duration_men <- 13.5
  non_employment_duration <- share_women * non_employment_duration_women + (1 - share_women) * non_employment_duration_men

  average_monthly_wage_women <- 52.63 * 365 / 12 # Caliendo et al. (2013) Table 3 - This is pre unemployment / post not available
  average_monthly_wage_men <- 82 * 365 / 12 # Caliendo et al. (2013) Table 3 - This is pre unemployment / post not available
  average_monthly_wage <- share_women * average_monthly_wage_women + (1 - share_women) * average_monthly_wage_men

  consumption_drop <- 0.08 # (delta consumption / consumption), i.e. the consumption drop when someone becomes unemployed
  relative_risk_aversion <- global_relative_risk_aversion # Coefficient of relative risk aversion
  # The two assumptions above are from Hendren & Sprung-Keyser (2020).
  # Schmieder & von Wachter (2016) survey studies that estimate the consumption drop. Estimates range from about 5%
  # to 25%. All but one study look at the US. There is no study for Germany.
  # Since the unemployment benefits are more generous in GER compared to US, a estimate towards the lower end of the distribution
  # seems reasonable.

  tax_rate <- global_flat_tax

  # Replacement rate is the share of net income unemployed receive while receiving benefits
  replacement_rate = 0.63 # Schmieder et al. (2012), p. 711

  # Marginal effect of extending the benefit duration on unemployment duration d(unemployment_duration) / d(max_benefit_duration)
  d_unemployment_duration_over_d_benefit_duration_women <-
    estimates$unemployment_duration_effect_women / (max_benefit_duration - max_benefit_duration_before_cutoff)
  d_unemployment_duration_over_d_benefit_duration_men <-
    estimates$unemployment_duration_effect_men / (max_benefit_duration - max_benefit_duration_before_cutoff)
  d_unemployment_duration_over_d_benefit_duration <- share_women * d_unemployment_duration_over_d_benefit_duration_women
  + (1- share_women) * d_unemployment_duration_over_d_benefit_duration_men

  #--------------------------------------------------------------------------------------------------------------------#
  # MVPF Calculation:
  #--------------------------------------------------------------------------------------------------------------------#

  # Hendren & Sprung-Keyser (2020) calculate the marginal value of public funds of a unemployment benefits reform as:
  # WTP = 1 + relative_risk_aversion * (delta consumption / consumption)
  # Cost = 1 + Fiscal Externality

  # Schmieder & von Wachter (2016) derive a formula for the fiscal externality:
  # FE = d(unemployment_duration) / d(max_benefit_duration) * (1 / exhaustion_rate) * (chi + tax_rate / replacement_rate)
  # where chi = 1 - (1 + max_benefit_duration * job_finding_hazard_rate) * exp(-job_finding_hazard_rate * max_benefit_duration)
  # see p. 560 & 562

  # The formula derived by Schmieder & von Wachter (2016) assumes a constant hazard rate of job finding.
  job_finding_hazard_rate <- 1 / non_employment_duration

  # Exhaustion rate is the share of unemployed who reach the end of the benefit period without finding new employment
  # Schmieder & von Wachter (2016) assume a exponential distribution. -> The exhaustion rate follows from the hazard rate
  # & the benefit duration:
  exhaustion_rate <- exp(- job_finding_hazard_rate * max_benefit_duration)

  # Now we can calculate chi and the fiscal externality:
  chi = 1 - (1 + max_benefit_duration*job_finding_hazard_rate) * exp(-max_benefit_duration*job_finding_hazard_rate)
  fiscal_externality <- d_unemployment_duration_over_d_benefit_duration * (1 / exhaustion_rate) * (chi + tax_rate / replacement_rate)

  # Then calculate cost & WTP
  government_net_costs <- 1 + fiscal_externality
  valuation_reduced_risk <- relative_risk_aversion * consumption_drop
  willingness_to_pay <- 1 + valuation_reduced_risk

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        fiscal_externality = fiscal_externality,
                        valuation_reduced_risk = relative_risk_aversion * consumption_drop,
                        program_cost = 1)

  return(return_values)
}
