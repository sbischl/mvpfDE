#----------------------------------------------------------------------------------------------------------------------#
# Maternity Leave Reform 1992 (Up to 18 months of additional unpayed leave)
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Dustmann & Schönberg (2011)

maternityLeave92 <- function (bootstrap_replication = 0, use_constant_ols_return_to_schooling = global_use_constant_ols_return_to_schooling) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Dustmann & Schönberg (2011) use 1992 DM
  prices_year <- 1992

  # D Mark EUR conversion
  dm_eur_conversion_rate <- 0.51129

  # This reform differs from the other two reforms because the maternity leave benefit remains unchanged. The only
  # effect of this reform is that it allows mothers to take a additional maternal leave of up to 18 months (after the
  # 18 months of payed leave) which is unpayed. -> This reform has no direct cost because the benefit payment remains
  # unchanged an also the share of mothers who return to work in the first 18 months is unchanged. See Dustmann & Schönberg (2011)
  # Figure 3 Panel C

  # Effect on available income after 40 months:
  available_income_effect_40months <- estimates$available_income_effect * dm_eur_conversion_rate

  # Effect on children's high track graduation attendence:
  high_track_graduation_effect <- estimates$high_track_attendence_effect

  #--------------------------------------------------------------------------------------------------------------------#
  # Impacts on Mother:
  #--------------------------------------------------------------------------------------------------------------------#

  # The valuation of being able to stay home and being able to return to the previous job at a later date is assumed to
  # be zero. Maybe look for some Literature on this; but this is probably very hard to measure


  # The mothers who stay at home do not receive a labor income. Dustmann & Schönberg (2011) do not report
  # the effect on labor income. Instead, they report the effect on available income defined as "income is defined as
  # her monthly earnings if the mother is working, as the monthly maternity benefit if
  # she is not working but eligible for paid leave, and zero otherwise" (p. 206).
  # Although it is never stated explicitely, this available income should be net income.

  # Since benefit payment is unchanged, the change in available income is equal to the change in net earnigs.
  net_income_effect <- available_income_effect_40months

  gross_income_effect <- getGrossIncome(net_income = net_income_effect,
                                        flat_tax = global_flat_tax,
                                        assume_flat_tax = TRUE)

  # Since we do not have any information about the income of mothers, the linear tax rate has to be used:
  tax_revenue_effect_mother <- global_flat_tax * gross_income_effect
  government_net_costs <- - tax_revenue_effect_mother

  #--------------------------------------------------------------------------------------------------------------------#
  # Impacts on Children:
  #--------------------------------------------------------------------------------------------------------------------#

  # Dustmann & Schönberg report the Effect on Low / Medium / High Track (=Gymnasium) Attendence. The IAB data does not
  # allow to distinguish between Realschule / Hauptschule. -> Only use effect on High Track

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

  willingness_to_pay <- net_income_children_effect
  government_net_costs <- government_net_costs - tax_revenue_effect_children

  # Cost of Schooling
  cost_of_schooling_low_track <- discount(from = 1986 + 11, to = 1986) * costOfSchool(year = 1986 + 11, duration_of_schooling = 6, school_type = "realschule", prices_year = prices_year)
  cost_of_schooling_high_track <- discount(from = 1986 + 11, to = 1986) * costOfSchool(year = 1986 + 11, duration_of_schooling = 8, school_type = "gymnasium", prices_year = prices_year)

  cost_difference <- cost_of_schooling_high_track - cost_of_schooling_low_track
  education_cost <- cost_difference * high_track_graduation_effect
  government_net_costs <- government_net_costs + education_cost

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        tax_revenue_increase = - tax_revenue_effect_mother,
                        education_cost = education_cost,
                        tax_revenue_increase_children = - tax_revenue_effect_children,
                        net_income_increase = net_income_children_effect,
                        prices_year = prices_year)

  return(return_values)
}