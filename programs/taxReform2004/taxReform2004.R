#----------------------------------------------------------------------------------------------------------------------#
# Top Tax Reform 2001
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Doerrenberg et al. (2017) and Working Paper Version from 2015

taxReform2004 <- function (bootstrap_replication = 0, year_difference = 3) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  # Tax rates before and after the reform:
  marginal_top_tax_rate_before <- 0.485
  marginal_top_tax_rate_after <- 0.45

  if (overwrite_eti) {
    elasticity_of_taxable_income <- global_eti
  }
  else {
    # We have 3 estimates for the elasticity of taxable income with respect to the net-of-tax rate depending on the
    # elapsed time between tax rate change and the income reaction.
    elasticity_of_taxable_income <- c(estimates$eti_1year, estimates$eti_2year, estimates$eti_3year)[year_difference]
  }

  # The way the MVPF is contructed for tax reforms, the willingness to pay is always one
  willingness_to_pay <- 1

  # Calculate the fiscal externality given the tax rate before the reform and after the reform
  fiscal_externality_before <-
    marginal_top_tax_rate_before / (1 - marginal_top_tax_rate_before) * estimates$pareto_coefficient_2004_wid * elasticity_of_taxable_income

  fiscal_externality_after <-
    marginal_top_tax_rate_after / (1 - marginal_top_tax_rate_after) * estimates$pareto_coefficient_2004_wid * elasticity_of_taxable_income

  # Follow Hendren and Sprung-Keyser (2020) by taking the average of the fiscal externality before and after the reform
  average_fiscal_externality <- (fiscal_externality_before + fiscal_externality_after) / 2

  # Substract the fiscal externality from the initial government cost of 1:
  government_net_costs <- 1 - average_fiscal_externality

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        fiscal_externality = -average_fiscal_externality,
                        program_cost = 1,
                        prices_year = 2004)
  return(return_values)
}
