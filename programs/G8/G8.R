G8 <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)
  # Assumptions:
  # The natural number would be one since students spend one year less in school. Marcus & Zambre (2019) estimate that
  # that G8 graduates start to study 10 months earlier compared to G9 students.
  earlier_labor_force_participation <- 10 / 12 #in years
  first_year_income <- 25000

  willingness_to_pay <- 1
  government_net_costs <- 1
  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs)
  return(return_values)
}
