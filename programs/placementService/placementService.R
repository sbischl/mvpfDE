#----------------------------------------------------------------------------------------------------------------------#
# Intensive Placement Servies East Germany (Inhouse by Employment agency vs private provision):
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Krug & Stephan (2013)

placementService <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  prices_year <- 2009 # The RCT was conducted in 2009.

  # Krug & Stephan (2013) provide a cost-benefit analysis in the appendix. This cost-benefit analysis only concerns
  # the employment agency. But for the MVPF we also need the effect on tax revenue. Krug & Stephan (2013) do not
  # show the effect on earnings in their cost benefit analysis. However, it is possible to infer the effect on earnings
  # because Table A1 reports the effect on unemployment insurance contributions.

  # Krug & Stephan (2013) have data on two seperate local agencies (One in the east and one in the west)
  share_east <- (414 + 412) / (254 + 280 + 414 + 412) # Table 1

  # This is a quick check regarding the definition of earnings (see longer comment below)
  unemployment_insurance_contribution_effect <- 17
  unemployment_insurance_rate <- 0.028
  implied_earnings_effect <- unemployment_insurance_contribution_effect / unemployment_insurance_rate

  # The implied effect on earnings is roughly equivalent to the effect reported in Table 4. Use the effect from Table 4!
  earnings_effect <- share_east * estimates$earnings_effect_east + (1 - share_east) * estimates$earnings_effect_west

  # Table A1 reports the effect on the cumulated unemployment benefits received over 1.5 years.
  # The effect on unemloyment insurance contributions is ignored because this is not considered a tax.
  unemployment_benefits_effect <- share_east * 1211 + (1 - share_east) * 937

  # No earnings information is displayed in the descriptive statistics. We know that the control group is unemployed.
  # As an approximation use the earnings information of Altmann et al. (2018) who also conducted a field experiment on
  # unemployed.
  earnings_control_group <- (52.38 * 365) / 12 * deflate(from = 2011, to = prices_year)

  # Average age of unemployed in sample. Only brackets are reported, Table 1:
  average_age <- share_east * ((25 + 40) / 2 * 0.18 + (40 + 49) / 2 * 0.15 + (50 + 59) / 2 * 0.48 + (60 + retirement_age) / 2 * 0.19) +
    (1 - share_east) *((25 + 40) / 2 * 0.18 + (40 + 49) / 2 * 0.20 + (50 + 59) / 2 * 0.41 + (60 + retirement_age) / 2 * 0.20)

  # Krug & Stephan (2013) Table A1 "Entries 4/2009 to 2/2010" - Costs of intensive services & Fixed budget expenditures
  program_cost <- share_east * (636 + 149) + (1 - share_east) * (929 + 123)

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # The reported effects are over a time span of 1.5 years
  reform_impact <- 1.5 * project_medium_run_impact(absolute_impact_magnitude = earnings_effect / 1.5,
                                                   yearly_control_income = earnings_control_group * 12,
                                                   number_of_periods = 1,
                                                   prices_year = prices_year,
                                                   inculde_welfare_benefits_fraction = 0) # exclude welfare benefits
  # because the paper provides a estimate of the effect on unemployment benefits

  net_income_increase <- reform_impact$present_value_net_earnings_impact
  tax_revenue_increase <- reform_impact$present_value_tax_payment_impact

  #--------------------------------------------------------------------------------------------------------------------#
  # MVPF Calculation
  #--------------------------------------------------------------------------------------------------------------------#

  government_net_costs <- -tax_revenue_increase + program_cost - unemployment_benefits_effect
  # It is not entirely clear if the effect on earnings only considers the earnings of working individuals or if unemployed as
  # coded as zero earnings. The way the authors calculate the authors calculate the unemployment insurance contribution
  # would imply the latter: "We compute additional income for unemployment insurance by multiplying earnings during the
  # observation period by the current contribution rate" (p. 33). If earnings were only for employed, the authors would
  # have to multiply with the share of employed.
  # It is odd that the effect on earnings is a lot smaller than the reduction in unemployment benefits received. This would
  # imply that individuals in the treatment group are worse off even though they have higher earnings and are more likely
  # to be employed. Such a result is not compatible with the German tax system since marginal tax rates typically do not
  # exceed 1 and definitely not to the extent to explain such a large difference. Assume that the willingness to pay as at
  # least equal to the benefit reduction MINUS the earnings change
  willingness_to_pay <- max(net_income_increase, unemployment_benefits_effect - earnings_effect)

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = program_cost,
                        net_income_increase =  max(net_income_increase, unemployment_benefits_effect - earnings_effect),
                        tax_revenue_increase = -tax_revenue_increase,
                        benefit_receipt = -unemployment_benefits_effect,
                        prices_year = prices_year)

  return(return_values)
}
