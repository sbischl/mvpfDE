#----------------------------------------------------------------------------------------------------------------------#
# Maternity Leave Reform 1979 (Extension of payed leave from 2 to 6 months)
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Dustmann & Schönberg (2011)

maternityLeave79 <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Dustmann & Schönberg (2011) use 1992 DM
  prices_year <- 1992

  # D Mark EUR conversion
  dm_eur_conversion_rate <- 0.51129

  # The maternity benefit that is payed after week 8 after birth till the end of the maternity leave phase
  maternity_benefit <- 750 * dm_eur_conversion_rate

  share_of_inframarginal_mothers <- 0.6 #Mothers who would remain at home without the reform.
  # From Dustmann & Schönberg (2011) Figure 3 Panel A, we can see that this is should be roughly equal to 0.6

  # Share of mothers who receive maternity benefits during the phase that was affected by the reform.
  share_of_benefit_receivers <- 0.9 # This can also be roughly inferred from Figure 3 Panel A

  # Effect on available income after 40 months:
  available_income_effect_40months <- estimates$available_income_effect * dm_eur_conversion_rate

  # Effect on children's wages at the age of 28:
  children_wage_effect <- estimates$children_wage_effect_at28_full_sample

  #--------------------------------------------------------------------------------------------------------------------#
  # Impacts on Mother:
  #--------------------------------------------------------------------------------------------------------------------#

  # Valuation of the maternity benefit which is now payed 6 instead of 2 months:
  # Mothers who would have stayed at home even if the maternity benefit remained at the pre-reform level value the
  # transfer euro for euro.
  valuation_maternity_benefit <- maternity_benefit * 4 * share_of_inframarginal_mothers
  willingness_to_pay <- valuation_maternity_benefit
  # The valuation for women who change their behavior, i.e. who stay longer at home because of the reform is unclear.
  # In case of a marginal reform the envelope theorem would imply that the valuation is zero.

  # The subsidy has to be payed for by the government:
  program_cost <- maternity_benefit * 4
  government_net_costs <- program_cost
  # In addition the mothers who stay at home do not receive a labor income. Dustmann & Schönberg (2011) do not report
  # the effect on labor income. Instead, they report the effect on available income defined as "income is defined as
  # her monthly earnings if the mother is working, as the monthly maternity benefit if
  # she is not working but eligible for paid leave, and zero otherwise" (p. 206).
  # Although it is never stated explicitely, this available income should be net income.

  # By subtracting the increase in benefits times the take-up rate, we can try to infer the effect on net_income that was taxed.
  net_income_effect <- available_income_effect_40months - share_of_benefit_receivers * 4 * maternity_benefit

  gross_income_effect <- getGrossIncome(net_income = net_income_effect,
                                        flat_tax = global_flat_tax,
                                        assume_flat_tax = TRUE)

  # Since we do not have any information about the income of mothers, the linear tax rate has to be used:
  tax_revenue_effect_mother <- global_flat_tax * gross_income_effect
  government_net_costs <- government_net_costs - tax_revenue_effect_mother

  #--------------------------------------------------------------------------------------------------------------------#
  # Impacts on Children:
  #--------------------------------------------------------------------------------------------------------------------#

  lifetime_impacts <- project_lifetime_impact (impact_age = 18,
                           impact_magnitude = children_wage_effect,
                           relative_control_income = 1,
                           end_projection_age = retirement_age,
                           start_projection_year = 1979 + 18,
                           prices_year = prices_year,
                           discount_to = 1979)

  tax_revenue_effect_children <- lifetime_impacts$present_value_tax_payment_impact
  net_income_children_effect <- lifetime_impacts$present_value_net_earnings_impact


  willingness_to_pay <- willingness_to_pay + net_income_children_effect
  government_net_costs <- government_net_costs - tax_revenue_effect_children

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = program_cost,
                        tax_revenue_increase = - tax_revenue_effect_mother,
                        tax_revenue_increase_children = - tax_revenue_effect_children,
                        net_income_increase = net_income_children_effect,
                        benefit_receipt = valuation_maternity_benefit)

  return(return_values)
}