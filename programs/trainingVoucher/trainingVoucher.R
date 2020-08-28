#----------------------------------------------------------------------------------------------------------------------#
# Training Vouchers (Active Labor Market Policy):
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Huber et al. (2018)
# Doerr et al (2016) & their working paper version Doerr et al. (2014) which contains more tables. In particular they
# show the OLS estimates for earnings 1-4 years after training. The published version only contains graphs.

trainingVoucher <- function (bootstrap_replication = 0, extend_effect = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Neither Doerr et al. (2016) nor Huber et al. (2018) contains any information on the cost of the training programs.
  # We have to borrow this information from Biewen et al. who looked at similar Training Measures.
  monthly_cost <- 631 # Biewen et al. (2014) Table 1 Column "2003"

  # Program Duration:
  duration_long_term_training <- 5 # "Long-term training courses typically last from several months to one
  # year (in our sample, the average was five months)", Doerr et al (2016), p. 9
  participants_long_term_training <- 22199 # Doerr et al. (2014) Table 1

  duration_degree_course <- 24 + 12/2  # "Degree courses have a typical duration of two to three years
  # (similar to the former retraining programs)", Doerr et al (2016), p. 9
  participants_degree_course <- 9017 # Doerr et al (2016) Table 1

  # (Monthly) effect on earnings of voucher receipt, Doerr et al. (2014) Table 7:
  earnings_effect_year1 <- estimates$earnings_effect_year1
  earnings_effect_year2 <- estimates$earnings_effect_year2
  earnings_effect_year3 <- estimates$earnings_effect_year3
  earnings_effect_year4 <- estimates$earnings_effect_year4

  control_income <- 91258 / 4 # Huber et al. (2018) Online Appendix Table A1
  average_age <- 39.03 # Huber et al. (2018) Online Appendix Table A1
  redeption_rate <- 0.78 # "Individuals who redeemed their vouchers (78% of all the treated individuals who received a voucher)",
  # Doerr et al. (2016), p. 39

  prices_year <- 2003 # Both studies look at training voucher that were awarded in 2003 and 2004

  #--------------------------------------------------------------------------------------------------------------------#
  # Effect of Earnings Change on WTP and Government Cost
  #--------------------------------------------------------------------------------------------------------------------#

  reform_impact <- project_medium_run_impact(absolute_impact_magnitude = c(earnings_effect_year1 * 12,
                                                                           earnings_effect_year2 * 12,
                                                                           earnings_effect_year3 * 12,
                                                                           earnings_effect_year4 * 12),
                                             yearly_control_income = control_income,
                                             number_of_periods = 4 + extend_effect,
                                             prices_year = prices_year)

  government_net_costs <- - reform_impact$present_value_tax_payment_impact
  willingness_to_pay <- reform_impact$present_value_net_earnings_impact

  #--------------------------------------------------------------------------------------------------------------------#
  # Training cost
  #--------------------------------------------------------------------------------------------------------------------#

  discounted_cost_long_term_training <- discountMonthlyCashFlow(amount = monthly_cost, months = duration_long_term_training)
  discounted_cost_degree_course <- discountMonthlyCashFlow(amount = monthly_cost, months = duration_degree_course)
  average_discounted_cost <- discounted_cost_long_term_training * (participants_long_term_training / (participants_degree_course + participants_long_term_training))
  + discounted_cost_degree_course * (participants_degree_course / (participants_degree_course + participants_long_term_training))

  government_net_costs <- government_net_costs + average_discounted_cost

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs)
  return(return_values)
}