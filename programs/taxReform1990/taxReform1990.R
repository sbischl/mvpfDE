#----------------------------------------------------------------------------------------------------------------------#
# Top Tax Reform 1990
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Atkinson & Piketty (2010)
# Gottfried & Schellhorn (2004)

taxReform1990 <- function (bootstrap_replication = 0, useETIFromDoerrenberg2015 = T, useParetoCoefficienFromWID = T) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  # Tax rates before and after the reform:
  marginal_top_tax_rate_before <- 0.56
  marginal_top_tax_rate_after <- 0.53

  # The way the MVPF is contructed for tax reforms, the willingness to pay is always one
  willingness_to_pay <- 1

  # Elasticity of taxable income:
  if (overwrite_eti) {
    elasticity_of_taxable_income <- global_eti
  }
  else if (useETIFromDoerrenberg2015) {
    elasticity_of_taxable_income <- estimates$elasticity_of_taxable_income_doerrenberg_2015
  }
  else {
    elasticity_of_taxable_income <- estimates$elasticity_of_taxable_income
  }

  if (useParetoCoefficienFromWID) {
    pareto_coefficient <- estimates$pareto_coefficient_1989
  }
  else {
    pareto_coefficient <- estimates$pareto_coefficient_1990_wid
  }

  # Calculate the fiscal externality given the tax rate before the reform and after the reform
  fiscal_externality_before <-
    marginal_top_tax_rate_before / (1 - marginal_top_tax_rate_before) * pareto_coefficient * elasticity_of_taxable_income

  fiscal_externality_after <-
    marginal_top_tax_rate_after / (1 - marginal_top_tax_rate_after) * pareto_coefficient * elasticity_of_taxable_income

  # Calculate lowest ETI which makes this reform self financing.
  break_even_eti <- 2 / pareto_coefficient /
    (marginal_top_tax_rate_before / (1 - marginal_top_tax_rate_before) + marginal_top_tax_rate_after / (1 - marginal_top_tax_rate_after))

  # Follow Hendren and Sprung-Keyser (2020) by taking the average of the fiscal externality before and after the reform
  average_fiscal_externality <- (fiscal_externality_before + fiscal_externality_after) / 2

  # Substract the fiscal externality from the initial government cost of 1:
  government_net_costs <- 1 - average_fiscal_externality


  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        fiscal_externality = -average_fiscal_externality,
                        program_cost = 1,
                        prices_year = 1990)
  return(return_values)
}
