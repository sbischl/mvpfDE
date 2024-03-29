#----------------------------------------------------------------------------------------------------------------------#
# Class Room Training (Active Labor Market Policy):
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Biewen et al. (2014)

trainingMeasures <- function (bootstrap_replication = 0, extend_effect = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # The paper reports effects for three different strata (depending on the number of months that have passed since the
  # training participant became unemployed). For each stata the effect for women and men is reported.
  # There is no specification that averages over all individuals.
  # But there is information about the size of each group:

  # Number of class room training participants in each strata, see Biewen et al. (2014) Table 2
  number_women_stratum_1 <- 693
  number_men_stratum_1 <- 912

  number_women_stratum_2 <- 409
  number_men_stratum_2 <- 547

  number_women_stratum_3 <- 662
  number_men_stratum_3 <- 497

  total_number_class_room_training <- number_women_stratum_1 + number_men_stratum_1 +
    number_women_stratum_2 + number_men_stratum_2 +
    number_women_stratum_3 + number_men_stratum_3

  share_men <- (number_men_stratum_3 + number_men_stratum_2 + number_men_stratum_1) / total_number_class_room_training

  # Load all the earnigs effect for all the subpopulations
  earnings_effect_1year_women_strat3 <- estimates$earnings_effect_1year_women_strat3
  earnings_effect_2year_women_strat3 <- estimates$earnings_effect_2year_women_strat3
  earnings_effect_1year_men_strat3 <- estimates$earnings_effect_1year_men_strat3
  earnings_effect_2year_men_strat3 <- estimates$earnings_effect_2year_men_strat3
  earnings_effect_1year_women_strat2 <- estimates$earnings_effect_1year_women_strat2
  earnings_effect_2year_women_strat2 <- estimates$earnings_effect_2year_women_strat2
  earnings_effect_1year_men_strat2 <- estimates$earnings_effect_1year_men_strat2
  earnings_effect_2year_men_strat2 <- estimates$earnings_effect_2year_men_strat2
  earnings_effect_1year_women_strat1 <- estimates$earnings_effect_1year_women_strat1
  earnings_effect_2year_women_strat1 <- estimates$earnings_effect_2year_women_strat1
  earnings_effect_1year_men_strat1 <- estimates$earnings_effect_1year_men_strat1
  earnings_effect_2year_men_strat1 <- estimates$earnings_effect_2year_men_strat1

  # Use daily log earnings from last job. Online Appendix Table A2 & A3
  control_income <- share_men * (exp(4.02) * 365 / 12)  + (1 - share_men) * (exp(3.81) * 365 / 12)

  # Average age of training participants:
  average_age <- 37 # About 37, see Online Appendix of Biewen et al. (2014) Table A2, A3

  # Training cost:
  enrollment_length <- 1.2 # Biewen et al. (2014), Table 1 Column 2000
  monthly_cost <- 580 # Biewen et al. (2014) Table 1 Column 2000

  prices_year <- 2000 # Dataset covers 2000 - 2002. Figures, e.g. Figure 5, use 2000 Euros

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  # Construct an average impact magnitude by taking the average over all earnings effects weighted with
  # the population share of each subpopulation
  impact_magnitude_year_1 <- earnings_effect_1year_men_strat1 * (number_men_stratum_1 / total_number_class_room_training) +
    earnings_effect_1year_women_strat1 * (number_women_stratum_1 / total_number_class_room_training) +
    earnings_effect_1year_men_strat2 * (number_men_stratum_2 / total_number_class_room_training) +
    earnings_effect_1year_women_strat2 * (number_women_stratum_2 / total_number_class_room_training) +
    earnings_effect_1year_men_strat3 * (number_men_stratum_3 / total_number_class_room_training) +
    earnings_effect_1year_women_strat3 * (number_women_stratum_3 / total_number_class_room_training)


  impact_magnitude_year_2 <- earnings_effect_2year_men_strat1 * (number_men_stratum_1 / total_number_class_room_training) +
    earnings_effect_2year_women_strat1 * (number_women_stratum_1 / total_number_class_room_training) +
    earnings_effect_2year_men_strat2 * (number_men_stratum_2 / total_number_class_room_training) +
    earnings_effect_2year_women_strat2 * (number_women_stratum_2 / total_number_class_room_training) +
    earnings_effect_2year_men_strat3 * (number_men_stratum_3 / total_number_class_room_training) +
    earnings_effect_2year_women_strat3 * (number_women_stratum_3 / total_number_class_room_training)

  reform_impact <- project_medium_run_impact(absolute_impact_magnitude = c(impact_magnitude_year_1 * 12, impact_magnitude_year_2 * 12),
                                             yearly_control_income = control_income * 12,
                                             number_of_periods = 2 + extend_effect,
                                             prices_year = prices_year)

  # The effect on willingness to pay and government net cost is given by the increase in tax revenue and net earnings
  net_income_increase <- reform_impact$present_value_net_earnings_impact
  tax_revenue_increase <- reform_impact$present_value_tax_payment_impact

  government_net_costs <- -tax_revenue_increase
  willingness_to_pay <- net_income_increase

  #--------------------------------------------------------------------------------------------------------------------#
  # Training cost
  #--------------------------------------------------------------------------------------------------------------------#

  government_net_costs <- government_net_costs + enrollment_length * monthly_cost

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = enrollment_length * monthly_cost,
                        net_income_increase = net_income_increase,
                        tax_revenue_increase = -tax_revenue_increase,
                        prices_year = prices_year)
  return(return_values)
}