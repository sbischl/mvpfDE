#----------------------------------------------------------------------------------------------------------------------#
# CO2 Abatement:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Abrell et al. (2019)

# This paper considers the cost associated with avoiding one ton of CO2 in the electricity sector.
# To analyze this policy, we have to assume that the funds raised by the German "Erneuerbare Energien Gesetz"
# are public funds.

eegWind <- function (bootstrap_replication = 0, carbon_leakage_rate = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  prices_year <- floor((2015+2010) / 2) # Abrell et al. (2019) use data from multiple sources. It looks like their estimation
  # takes the years 2010-2015 into account (see Figure 2)

  # Abrell et al. (2019) estimate the producer cost and consumer cost associated with reducing carbon emissions by one
  # ton in the production of electricity. In their model, they assume that consumers pay for the subsidy as the subsidy
  # is financed by a tax on electricity. To translate these estimates into a MVPF it has to be assumed that the government
  # pays for the subsidy. Which makes sense given that the revenue from EEG could at least in theory be used elsewhere.
  # Electricity producers pay for what  Abrell et al. (2019) call a price effect. Subsidized green energy pushes
  # conventinal producers with high margial costs out of the market and thus lowers the electricity price.

  # Producer Cost (lower bound) Abrell et al. (2019) Table 7
  producer_cost <- 303.0
  # Subsidy Cost labelled as consumer cost in Abrell et al. (2019) Table 7. This one is acutally negative which would
  # imply that the government could find a subsidy scheme that would leave electricity prices unchanged for consumers
  # while raising money at same time because the subsidy has a strongly negative effect on electricity prices.
  consumer_cost <- -20.9

  # Abrell et al. (2019) do no report standard erros for their final estimation of the abatement costs. In
  # Table 5 they calculate standard erros for the total margianal CO2 offset. This is the denominator of both producer_cost
  # & consumer_cost. See equation (21) and (22). When assuming that the numerator is constant we can caluclate a confidence
  # interval. This is obviously not ideal, but it won't get any better. Reverse engineering the entire calculation is
  # too complicated and maybe also be impossible.
  denominator_point_estimate <- -175.2
  denominator <- estimates$denominator

  # cost = numerator / denominator -> the numerator implied by the point estimate is:
  numerator_consumer_cost <- denominator_point_estimate * consumer_cost
  numerator_producer_cost <- denominator_point_estimate * producer_cost

  producer_cost <- numerator_producer_cost / denominator
  consumer_cost <- numerator_consumer_cost / denominator
  # The consumer cost (in the paper) is actually equal to the subsidy. See explanation above
  subsidy_cost <- consumer_cost

  #--------------------------------------------------------------------------------------------------------------------#
  # MVPF Calculation
  #--------------------------------------------------------------------------------------------------------------------#

  # The MVPF Calculation is really simple in this case.
  # Individuals value the reduction in CO2 emissions & the effect on producer costs.
  # In addition, co2 emissions can leak to other countries because of the European Emissions Trading System. If firms
  # are rational, and sell their certificates the leakage rate should be 1. Yet, a leakage rate of 0 is assumed. It does
  # not really matter since these policies provide very little value independently of the leakage problem.
  willingness_to_pay <- -producer_cost + co2_externality * (1 - carbon_leakage_rate)
  # The government has to pays for the subsidy that induces emission reductions
  government_net_costs <- subsidy_cost

  return_values <- list(willingness_to_pay = willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = subsidy_cost,
                        income_loss = -producer_cost,
                        co2_emission_reducation = co2_externality,
                        prices_year = prices_year)
  return(return_values)
}
