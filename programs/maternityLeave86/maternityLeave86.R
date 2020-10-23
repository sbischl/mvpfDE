#----------------------------------------------------------------------------------------------------------------------#
# Maternity Leave Reform 1986 (Extension of payed leave from 6 to 10 months)
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Dustmann & Schönberg (2011)

maternityLeave86 <- function (bootstrap_replication = 0, use_constant_ols_return_to_schooling = global_use_constant_ols_return_to_schooling) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Dustmann & Schönberg (2011) use 1992 DM
  prices_year <- 1992

  # D Mark EUR conversion
  dm_eur_conversion_rate <- 0.51129

  # The maternity benefit that is payed after week 8 after birth till the end of the maternity leave phase. It was lowered
  # from 750 DM to 600, but is payed for a longer period.
  maternity_benefit <- 600 * dm_eur_conversion_rate
  maternity_benefit_before_reform <- 750 * dm_eur_conversion_rate

  share_of_inframarginal_mothers <- 0.55 #Mothers who would remain at home without the reform.
  # From Dustmann & Schönberg (2011) Figure 3 Panel B, we can see that this is should be roughly equal to 0.55

  # Share of mothers who receive maternity benefits during the phase that was affected by the reform.
  share_of_benefit_receivers <- 0.8 # This can also be roughly inferred from Figure 3 Panel B

  # Effect on available income after 40 months:
  available_income_effect_40months <- estimates$available_income_effect * dm_eur_conversion_rate

  # Effect on children's high track graduation rate:
  high_track_graduation_effect <- estimates$high_track_graduation_effect

  #--------------------------------------------------------------------------------------------------------------------#
  # Impacts on Mother:
  #--------------------------------------------------------------------------------------------------------------------#

  # Valuation of the maternity benefit which is now payed 10 instead of 6 months AND the transfer is now lower (600 instead
  # of 750 DM from month 3 onwards)
  # Mothers who would have stayed at home even if the maternity benefit remained at the pre-reform level value the
  # transfer euro for euro.
  valuation_maternity_benefit <- (maternity_benefit * 10 -  maternity_benefit_before_reform * 4) * share_of_inframarginal_mothers
  willingness_to_pay <- valuation_maternity_benefit
  # The valuation for women who change their behavior, i.e. who stay longer at home because of the reform is unclear.
  # In case of a marginal reform the envelope theorem would imply that the valuation is zero.

  # The subsidy has to be payed for by the government:
  program_cost <- share_of_benefit_receivers * (maternity_benefit * 10 -  maternity_benefit_before_reform * 4)
  government_net_costs <- program_cost

  # In addition the mothers who stay at home do not receive a labor income. Dustmann & Schönberg (2011) do not report
  # the effect on labor income. Instead, they report the effect on available income defined as "income is defined as
  # her monthly earnings if the mother is working, as the monthly maternity benefit if
  # she is not working but eligible for paid leave, and zero otherwise" (p. 206).
  # Although it is never stated explicitely, this available income should be net income.

  # By subtracting the increase in benefit difference before and after the reform
  # times the take-up rate, we can try to infer the effect on net_income that was taxed.
  net_income_effect <- available_income_effect_40months - share_of_benefit_receivers * (maternity_benefit * 10 -  maternity_benefit_before_reform * 4)

  gross_income_effect <- getGrossIncome(net_income = net_income_effect,
                                        flat_tax = global_flat_tax,
                                        assume_flat_tax = TRUE)

  # Since we do not have any information about the income of mothers, the linear tax rate has to be used:
  tax_revenue_effect_mother <- global_flat_tax * gross_income_effect
  government_net_costs <- government_net_costs - tax_revenue_effect_mother

  #--------------------------------------------------------------------------------------------------------------------#
  # Impacts on Children:
  #--------------------------------------------------------------------------------------------------------------------#

  # Dustmann & Schönberg report the Effect on High Track (=Gymnasium) Graduation:

  # Project and discount earnings / tax payments when enrolling in college compared to a vocational degree.
  impact_magnitude_matrix <- getEducationEffectOnEarnings(education_decision = "abitur",
                                                          alternative = "vocational_educ")

  if (!use_constant_ols_return_to_schooling) {

    lifetime_impacts <- project_lifetime_impact(impact_age = 18,
                                                impact_magnitude_matrix = impact_magnitude_matrix,
                                                relative_control_income = 1,
                                                start_projection_year = 1986 + 18,
                                                prices_year = prices_year,
                                                discount_to = 1986)
  }
  else {
    # The high track entails 2 more years of schooling.
    impact_longer_schooling <- project_lifetime_impact(impact_age = 19,
                                                       impact_magnitude = -1,
                                                       relative_control_income = 1,
                                                       end_projection_age = 21,
                                                       start_projection_year = 1986 + 19,
                                                       prices_year = prices_year,
                                                       discount_to = 1986)

    impact_more_education <- project_lifetime_impact(impact_age = 21,
                                                     impact_magnitude = 2 * yearly_return_to_schooling,
                                                     relative_control_income = 1,
                                                     start_projection_year = 1986 + 19 + 2,
                                                     prices_year = prices_year,
                                                     discount_to = 1986)

    # Add impact_longer_schooling and impact_more_education.
    lifetime_impacts <- impact_longer_schooling + impact_more_education
  }

  tax_revenue_effect_children <- lifetime_impacts$present_value_tax_payment_impact * high_track_graduation_effect
  net_income_children_effect <- lifetime_impacts$present_value_net_earnings_impact * high_track_graduation_effect

  willingness_to_pay <- willingness_to_pay + net_income_children_effect
  government_net_costs <- government_net_costs - tax_revenue_effect_children

  # Cost of Schooling
  cost_of_schooling_low_track <- discount(from = 1986 + 11, to = 1986) * costOfSchool(year = 1986 + 11, duration_of_schooling = 6, school_type = "realschule", prices_year = prices_year)
  cost_of_schooling_high_track <- discount(from = 1986 + 11, to = 1986) * costOfSchool(year = 1986 + 11, duration_of_schooling = 8, school_type = "gymnasium", prices_year = prices_year)

  cost_difference <- cost_of_schooling_high_track - cost_of_schooling_low_track
  education_cost <- cost_difference * high_track_graduation_effect
  government_net_costs <- government_net_costs + education_cost


  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = program_cost,
                        tax_revenue_increase = - tax_revenue_effect_mother,
                        education_cost = education_cost,
                        tax_revenue_increase_children = - tax_revenue_effect_children,
                        net_income_increase = net_income_children_effect,
                        benefit_receipt = valuation_maternity_benefit,
                        prices_year= prices_year)

  return(return_values)
}