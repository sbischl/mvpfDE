#----------------------------------------------------------------------------------------------------------------------#
# Home Care Subsidy (Up to 18 months of additional unpayed leave)
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Collischon et al. (2020)

homeCareSubsidy <- function (bootstrap_replication = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Effect on child care takeup:
  subsidized_childcare_takeup <- estimates$subsidized_childcare_takeup

  # Effect on mothers's cumulated labor income over 36 motnhs:
  labor_income_effect_36months <- estimates$labor_income_effect_36months

  # Share of families who receive the subsidy:
  share_subsidy_receipt <- 0.535 # Collischon et al. (2020), Table 7 Panel A

  # Subsidy Amount:
  subsidy_amount <- 100

  # Fiscal cost of child care:
  fiscal_childcare_cost <- 620 #Collischon et al. (2020) p. 22

  # Subsidy period in months
  subsidy_period <- 22

  prices_year <- 2013

  # According to Collischon et al. (2020) Table 1 Mothers in the sample were 30.11 years old at birth. They start to receive
  # the subsidy 15 months after birth
  average_age <- 30.11 + (15 / 12)

  #--------------------------------------------------------------------------------------------------------------------#
  # Government Net Cost:
  #--------------------------------------------------------------------------------------------------------------------#

  # Becoming eligible for the subsidy reduces labor income. No information about the gross income of mothers in the sample
  # who are working is reported. The average net income of working mothers in Germany is 1200.
  # Still it is difficult to calculate the marginal tax rate, since a large share of the mothers is probably married
  # and the tax rate would also depend on the income of the husband. -> Use linear tax rate assumption

  tax_revenue_effect <- labor_income_effect_36months * global_flat_tax
  government_net_costs <- -tax_revenue_effect

  # The govermnet does not have to pay for child care:
  child_care_cost_reduction <- - subsidized_childcare_takeup * discountMonthlyCashFlow(fiscal_childcare_cost, subsidy_period)
  government_net_costs <- government_net_costs - child_care_cost_reduction

  # The goverment has to pay the subsidy to eligible mothers who take up the subsidy:
  program_cost <- discountMonthlyCashFlow(subsidy_amount, subsidy_period) * share_subsidy_receipt
  government_net_costs <- government_net_costs + program_cost

  #--------------------------------------------------------------------------------------------------------------------#
  # Willingness to Pay:
  #--------------------------------------------------------------------------------------------------------------------#

  # Mothers who would have stayed home without the subsidy value the subsidy euro for euro. Others value it at zero.
  # (-> Envelope Theorem)
  share_of_mothers_unchanged_behavior <- (share_subsidy_receipt + subsidized_childcare_takeup) / share_subsidy_receipt
  subsidy_valuation <- share_subsidy_receipt * share_of_mothers_unchanged_behavior * discountMonthlyCashFlow(subsidy_amount, subsidy_period)

  return_values <- list(willingness_to_pay =  subsidy_valuation,
                        government_net_costs = government_net_costs,
                        child_care_cost_reduction = -child_care_cost_reduction,
                        tax_revenue_increase = tax_revenue_effect,
                        program_cost = program_cost,
                        prices_year = prices_year)

  return(return_values)
}