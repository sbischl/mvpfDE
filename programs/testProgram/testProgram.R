#----------------------------------------------------------------------------------------------------------------------#
# Test Program
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Nothing Really

testProgram <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  willingness_to_pay <- estimates$test_12
  government_net_costs <- estimates$test_13

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        additional_return_value = estimates$test_8)
  return(return_values)
}
