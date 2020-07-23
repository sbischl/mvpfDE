testProgram <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimatates <- getEstimates(program_name, bootstrap_replication)

  willingness_to_pay = estimatates$test_13
  government_net_costs = estimatates$test_12

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        additional_return_value = estimatates$test_8)
  return(return_values)
}
