#----------------------------------------------------------------------------------------------------------------------#
# Unemployment Benefit Hartz Reform 2006:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Petrunyk & Pfeifer (2018)
# Schmieder et al. (2012)
# Schmieder & von Wachter (2016)

# Petrunyk & Pfeifer (2018) apply a DiD estimate the measure the effect of the unemployment benefit cuts that were
# enacted as part of the Hartz reforms. DiD is possible because the reform only affected Germans who were older than 45

unemploymentBenefits2006 <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#
  prices_year <-2006

  non_employment_duration <- 20.5 # Petrunyk & Pfeifer (2018) do not have data on the average unemployment duration.
  # The mvpf is not that sensitive with respect to the unemployment duration. Schmieder et al. (2012) Online Appendix Table W1
  # reports a non employment duration of 20.5 for 49 year olds (the sample average age of Petrunyk & Pfeifer (2018) is about 50)

  # The Benefit duration has to be calculated from Petrunyk & Pfeifer (2018) Table 1 & 9
  share_6month_reduction <- 4238 / (4238 + 4238 + 11174 + 17775)
  share_8month_reduction <- 4972 / (4238 + 4238 + 11174 + 17775)
  share_10month_reduction <- 11174 / (4238 + 4238 + 11174 + 17775)
  share_14month_reduction <- 17775 / (4238 + 4238 + 11174 + 17775)

  max_benefit_duration <- 12 * share_6month_reduction + 12 * share_10month_reduction + 18 * share_14month_reduction +
    18 * share_8month_reduction
  max_benefit_duration_before <- 18 * share_6month_reduction  + 22 * share_10month_reduction + 32 * share_14month_reduction + 26 * share_8month_reduction

  consumption_drop <- 0.08 # (delta consumption / consumption), i.e. the consumption drop when someone becomes unemployed
  relative_risk_aversion <- global_relative_risk_aversion # Coefficient of relative risk aversion
  # The two assumptions above are from Hendren & Sprung-Keyser (2020).
  # Schmieder & von Wachter (2016) survey studies that estimate the consumption drop. Estimates range from about 5%
  # to 25%. All but one study look at the US. There is no study for Germany.
  # Since the unemployment benefits are more generous in GER compared to US, a estimate towards the lower end of the distribution
  # seems reasonable.

  tax_rate <- global_flat_tax

  # Replacement rate is the share of net income unemployed receive while receiving benefits
  replacement_rate = 0.60 # Petrunyk & Pfeifer (2018), p. 11

  # Marginal effect of extending the benefit duration on unemployment duration d(unemployment_duration) / d(max_benefit_duration)
  # Petrunyk & Pfeifer (2018) report the effect of a one month benefit extension on days worked per year.
  # -> To get the change in days worked per unemployment spell multiply by the non_employment_duration in years.
  # Since we need the effect in months and not in days divide by 30
  d_unemployment_duration_over_d_benefit_duration <- estimates$days_worked_effect_continuous_treatment * (non_employment_duration / 12)  / 30

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
  government_net_costs <- -1 - fiscal_externality
  valuation_reduced_risk <- -relative_risk_aversion * consumption_drop
  willingness_to_pay <- -1 + valuation_reduced_risk

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        fiscal_externality = -fiscal_externality,
                        valuation_reduced_risk = -relative_risk_aversion * consumption_drop,
                        program_cost = -1,
                        prices_year = prices_year)

  return(return_values)
}
