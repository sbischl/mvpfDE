#----------------------------------------------------------------------------------------------------------------------#
# Negative Income Tax Experiment:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Spermann & Strotmann (2006)

# Spermann & Strotmann (2006) evaluate the effects of what they call a negative income tax experiment.
# This is misleading because the experiment they are describing does not resemble what is typically
# considered a negative income tax as proposed by Milton Friedman. In the sense of Friedman, a negative income tax
# is a basic income that is gradually phased-out with increasing gross earnings.
# Instead, Spermann & Strotmann (2006) consider a experiment where long-term unemployed receive a
# subsidy on top of their gross wage. The subsidy increases with the wage and can reach a maximum of up to 643 Deutschmark.

negativeIncomeTax <- function (bootstrap_replication = 0, extend_effect = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  prices_year <- 2001 # "We report empirical evidence from the first field experiments to be conducted in Germany
  # with program and control groups between 1999 and 2002" (Spermann & Strotmann, 2006)

  # D Mark EUR conversion
  dm_eur_conversion_rate <- 0.51129

  # Spermann & Strotmann (2006) measure the effect on net earnings (before the negative income tax) & on the probability of being employed
  # The earnings effect is measured on net income. But it looks like net income as Spermann & Strotmann (2006) define it
  # does not include any transfers. Otherwise their tobit model would be misspecified as observations below or equal to the minimum
  # tranfer would be censored (and not below or equal 0). Since the treatment group here are long term unemployed with a low income, they won't pay
  # any / very low income taxes -> assume net earnings effect = gross earnings effect
  earnings_effect <- estimates$earnings_effect * dm_eur_conversion_rate
  employment_effect <- estimates$employment_effect

  # Spermann & Strotmann (2006) do not report average earnings for the treatment | control group.
  # This is a problem because:
  # 1) we cannot calculate the non-linear income tax
  # 2) we do not know the amount of the subsidy as the subsidy depends on the gross wage
  # We need to assume something. This is not a critical assumptions. MVPF does not react alot to changes here:
  earnings_treatment_group <- 2500 * dm_eur_conversion_rate
  subsidy_amount <- 609 * dm_eur_conversion_rate # Spermann & Strotmann (2006) Appendix A4

  earnings_control_group <- earnings_treatment_group - earnings_effect
  # Average age of training participants:
  average_age <- 39.612 # Spermann & Strotmann (2006) Table 1

  # Share of unemployed who find employment in the treatment group.
  share_enters_emloyment <- 0.159 # Spermann & Strotmann (2006) Table 1

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # Multiply by 12 because the projection needs the yearly income
  reform_impact <- project_medium_run_impact(absolute_impact_magnitude = earnings_effect * 12,
                                             yearly_control_income = earnings_control_group * 12,
                                             number_of_periods = 1,
                                             prices_year = prices_year)


  # The effect on willingness to pay and government net cost is given by the increase in tax revenue and net earnings (per year)
  net_income_increase <- reform_impact$present_value_net_earnings_impact
  tax_revenue_increase <- reform_impact$present_value_tax_payment_impact

  #--------------------------------------------------------------------------------------------------------------------#
  # MVPF Calculation
  #--------------------------------------------------------------------------------------------------------------------#

  # Individuals who have started working had there been no subsidy value the subsidy euro for euro. Individuals who are
  # incentivized by the subsidy are indifferent (Envelope Theorem)
  # -> We need the share of subsidy receivers who would have worked had the subsidy not existed:
  share_inframarginal_employed <- (share_enters_emloyment - employment_effect) / share_enters_emloyment
  # Subsidy valuation per year
  subsidy_valuation <- share_inframarginal_employed * subsidy_amount * 12
  willingness_to_pay <- subsidy_valuation

  # Costs consist of paying the subsidy for 12 months minus the increase in tax revenue
  program_cost <- subsidy_amount * 12
  government_net_costs <- program_cost - tax_revenue_increase

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = program_cost,
                        reform_valuation = subsidy_valuation,
                        tax_revenue_increase = -tax_revenue_increase,
                        prices_year = prices_year)
  return(return_values)
}
