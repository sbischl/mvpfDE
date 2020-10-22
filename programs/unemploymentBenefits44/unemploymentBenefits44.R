#----------------------------------------------------------------------------------------------------------------------#
# Unemployment Benefit Extension Age 44:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Schmieder et al. (2012)
# Schmieder & von Wachter (2016)

# Schmieder et al. (2012) make use of a discontinuity in the German unemployment benefit system:
# At the age of 44 the entitlement to UI benefits increases from 18 to 22 months

unemploymentBenefits44 <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#
  max_benefit_duration <- 22
  max_benefit_duration_before_cutoff <- 18

  prices_year <- (1987 + 1999) / 2 # Autors use a dataset containing observations between July 1987 and March 1999

  non_employment_duration <-  17.5 # Schmieder et al. (2012) Online Appendix Table W1

  average_monthly_wage <- (63.4 * 365) / 12 # Schmieder et al. (2012) Online Appendix Table W1

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
  d_unemployment_duration_over_d_benefit_duration <- estimates$unemployment_duration_effect / (max_benefit_duration - max_benefit_duration_before_cutoff)

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

  # When assuming a tax_rate of 0.03, the fiscal externality should replicate the entry "Schmieder et al. 2012a" | Column:
  # "Behavioral cost per $1 increase in transfer − tax = 3%" of Table 2 of Schmieder & von Wachter (2016)
  # Schmieder & von Wachter (2016) find a value of 0.13, the calculations above yield 0.146. Probably Schmieder & von Wachter (2016)
  # assumed a slightly different hazard rate or replacement rate.

  # Then calculate cost & WTP
  government_net_costs <- 1 + fiscal_externality
  valuation_reduced_risk <- relative_risk_aversion * consumption_drop
  willingness_to_pay <- 1 + valuation_reduced_risk

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        fiscal_externality = fiscal_externality,
                        valuation_reduced_risk = relative_risk_aversion * consumption_drop,
                        prices_year = prices_year,
                        program_cost = 1)

  return(return_values)
}
