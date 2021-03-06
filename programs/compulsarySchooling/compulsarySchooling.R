#----------------------------------------------------------------------------------------------------------------------#
# Compulsary Schooling
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Piopiunik (2014)
# Piopiunik (2011)
# Kamhöfer & Schmitz (2016)
# Pischke & von Wachter (2008)

compulsarySchooling <- function (bootstrap_replication = 0, use_constant_ols_return_to_schooling = global_use_constant_ols_return_to_schooling) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Effect of compulsary schooling on probability of receiving at least middle school degree (for sons whose mother was affected
  # by the compulsary schooling reform)
  # For daughters and fathers the effect is always not significant. These are reported and could theoretically be included.
  at_least_middle_school <- estimates$at_least_middle_school

  # Additional years of schooling of at least middle school:
  additonal_years_of_schooling <- 2.4 # see  Piopiunik (2014), p. 896

  # reform effect in years of schooling
  years_of_schooling_effect <- additonal_years_of_schooling * at_least_middle_school

  prices_year <- 1967 #Piopiunik (2011) lists the years in which the reform was enacted for each state.
  # Last (Bavaria) was 23 years after the first (Hamburg).
  # Take 1967 as this was the year when larger states such as NRW, BW, Hessen adopted the reform.

  # The age at which the effect of the reform kicks in. Not entirely clear since the reform was targeted at the parents
  # but we see an effect for the children. -> Take the childrens age. -> Assume that the age at which they would have
  # finished school before the reform (i.e. 9th grade). In 9th grade students are about 15 - 16.
  impact_age <- 16

  # Generation time:
  generation_time <- 1981 - 1953  # see  Piopiunik (2014), Table 1
  # equal to age of parents at birth and also the number of years that pass till children are in 9th grade.

  # Direct Effect on Parents (i.e. the population affected by the schooling reform)
  lwage_effect <- estimates$lwage_effect_parents_ks # _pw: Pischke & von Wachter (2008) or _ks: Kamhöfer & Schmitz (2016)

  #--------------------------------------------------------------------------------------------------------------------#
  # Project and discount earnings & tax payments of children
  #--------------------------------------------------------------------------------------------------------------------#

  # Need to take into account that the effect only applies to son's of mothers.
  mothers_children <- 2832
  share_daughters_mothers <- 0.49
  fathers_children <- 2599
  share_daughers_fathers <- 0.48

  # Share of children who are are sons of mothers affected by the school reform:
  share_affected <- (1 - share_daughters_mothers) * (mothers_children / (mothers_children + fathers_children)) # should be about 0.25

  # Ideally we would project the earnings usinig the differences in average incomes as with the other reforms. However,
  # the IAB data does not distinguish between realschule / hauptschule. However, from p 896 of  Piopiunik (2014) we know
  # the share that got abitur and realschul degrees of the "at least middle school degree" outcome.
  # "at least middle school degree"
  # Realschule = 0.518
  # Abitur = 0.159 + 0.324
  # "NOT at least middle school degree"
  # -----------------
  # Drop out: 0.046
  # Hauptschule: 0.954
  # Strategy: Map Realschule & Hauptschule to "Berufsausbildung" in IAB Data
  # And Drop out to "No Berufsausbildung".
  # Probably leads to underestimation because Abitur in IAB Data means no university degree.
  # However, this same bias is in the years of school calculation of  Piopiunik (2014) also, so should be okay.
  # And the Balu und Du Calculation made a similar assumption

  # We assume here that the share who gets "Abitur" would have otherwise received vocational educ
  # And the share that got no degree before now gets vocational education
  impact_magnitude_matrix <- getEducationEffectOnEarnings(education_decision = "abitur", alternative = "vocational_educ") * (0.159 + 0.324) +
    0.046 * getEducationEffectOnEarnings(education_decision = "vocational_educ", alternative = "no_vocational_educ")
  # The multiplication messed up the age column. Restore it.
  impact_magnitude_matrix$age <- 18:(18 + nrow(impact_magnitude_matrix) - 1)


  if (!use_constant_ols_return_to_schooling) {
    lifetime_impacts <- project_lifetime_impact(impact_age = 18, # cannot go lower than 18
                                                impact_magnitude_matrix = impact_magnitude_matrix,
                                                relative_control_income = getRelativeControlGroupEarnings("vocational_educ"),
                                                start_projection_year = 1967 + generation_time,
                                                prices_year = prices_year,
                                                discount_to = 1967,
                                                inculde_welfare_benefits_fraction = 0)

  } else {
    impact_longer_schooling <- project_lifetime_impact(impact_age = 18, # cannot go lower than 18
                                                       impact_magnitude = -1,
                                                       relative_control_income = getRelativeControlGroupEarnings("vocational_educ"),
                                                       end_projection_age = 18,
                                                       start_projection_year = 1967 + generation_time,
                                                       prices_year = prices_year,
                                                       discount_to = 1967,
                                                       inculde_welfare_benefits_fraction = 0)

    impact_more_education <- project_lifetime_impact(impact_age = 19,
                                                     impact_magnitude = years_of_schooling_effect * yearly_return_to_schooling,
                                                     relative_control_income = getRelativeControlGroupEarnings("vocational_educ"),
                                                     start_projection_year = 1967 + generation_time + 1,
                                                     prices_year = prices_year,
                                                     discount_to = 1967,
                                                     inculde_welfare_benefits_fraction = 0)

    # Add impact_longer_schooling and impact_more_education.
    lifetime_impacts <- years_of_schooling_effect * impact_longer_schooling + impact_more_education
  }

  # Affected students value higher net-income
  net_income_increase <- lifetime_impacts$present_value_net_earnings_impact * share_affected
  willingness_to_pay <- net_income_increase
  # Government costs are reduced by the increase in tax revenue
  tax_revenue_increase <- lifetime_impacts$present_value_tax_payment_impact * share_affected
  government_net_costs <- - tax_revenue_increase

  #--------------------------------------------------------------------------------------------------------------------#
  # Project and discount earnings & tax payments of parent generation (the one which received more schooling)
  #--------------------------------------------------------------------------------------------------------------------#
  # Both Kamhöfer & Schmitz (2016) and Pischke & von Wachter (2008) find basically zero effect.
  lifetime_impacts_parents <- project_lifetime_impact(impact_age = 18, # cannot go lower than 18
                                                      impact_magnitude = lwage_effect,
                                                      relative_control_income = getRelativeControlGroupEarnings("vocational_educ"),
                                                      start_projection_year = 1967,
                                                      prices_year = prices_year,
                                                      inculde_welfare_benefits_fraction = 1)

  tax_revenue_increase_parents <- lifetime_impacts_parents$present_value_tax_payment_impact
  government_net_costs <- government_net_costs - tax_revenue_increase_parents
  net_income_increase_parents <- lifetime_impacts_parents$present_value_net_earnings_impact
  willingness_to_pay <- willingness_to_pay + net_income_increase_parents

  #--------------------------------------------------------------------------------------------------------------------#
  # Cost of Schooling
  #--------------------------------------------------------------------------------------------------------------------#

  # Increasing compulsary schooling costs
  cost_of_additional_year_parents <- costOfSchool(year = 1967,
                                          duration_of_schooling = 1,
                                          school_type = "hauptschule",
                                          prices_year = prices_year)

  # The children who get more education also induce costs
  cost_of_additonal_year_children <- share_affected *
    costOfSchool(year = 1967 + generation_time, duration_of_schooling = 1, school_type = "realschule", prices_year = prices_year) *
    discount(from = 1967 + generation_time, to = 1967) * years_of_schooling_effect

  government_net_costs <- government_net_costs + cost_of_additonal_year_children + cost_of_additional_year_parents

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = cost_of_additional_year_parents,
                        net_income_increase = net_income_increase,
                        net_income_increase_parents = net_income_increase_parents,
                        tax_revenue_increase = -tax_revenue_increase,
                        tax_revenue_increase_parents = - tax_revenue_increase_parents,
                        education_cost = cost_of_additonal_year_children,
                        prices_year = prices_year)
  return(return_values)
}
