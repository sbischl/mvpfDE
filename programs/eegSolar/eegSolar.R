#----------------------------------------------------------------------------------------------------------------------#
# CO2 Abatement:
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Abrell et al. (2019)

# This paper considers the cost associated with avoiding one ton of CO2 in the electricity sector.
# To analyze this policy, we have to assume that the funds raised by the German "Erneuerbare Energien Gesetz"
# are public funds.

eegSolar <- function (bootstrap_replication = 0, carbon_leakage_rate = 0) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  prices_year <- floor((2015+2010) / 2) # Abrell et al. (2019) use data from multiple sources. It looks like their estimation
  # takes the years 2010-2015 into account (see Figure 2)

  # Abrell et al. (2019) differentiates between consumer and producer costs. This is inconvenient to calculate the MVPF
  # because the consumer costs are combination of price changes (due to the subsidy) and the cost of paying the tax.
  # But we need to isolate these two effects. The price changes are part of the willingness to pay, the tax is
  # the net cost. There is no table that displays these two effects separately. But I think I have found away to
  # calculate these:

  # Producer Cost (lower bound) Abrell et al. (2019) Table 7
  producer_cost <- 181.5
  # Consumer cost Abrell et al. (2019) Table 7.
  consumer_cost <- 780.2

  # From Table 7 we know that Solar is subsidized by 286.3€ per megawatt hour.
  subsidy_per_MWh <- 286.3

  # From Table 5 we know the CO2 offset per MWh for different scenarios. Since the paper calculates producer and consumer
  # cost only for the "Domestic and hydro/PSP offsets only" scenario, we can only assume this scenario.
  # The Table is in kg, so we need to divide by 1000 to get tons
  co2_offset_per_MWh <- - estimates$co2_offset_per_MWh / 1000

  # -> we need 1/co2_offset_per_MWh to reduce carbon emissions by one ton.
  # and therefore the subsidy per ton of emission reduction is given by:
  subsidy_per_ton_co2_reduction <- subsidy_per_MWh / co2_offset_per_MWh

  # Since the price effect and subsidy_per_ton_co2_reduction sum to the consumer cost, we can calculate the
  # price effect as:
  price_effect <- consumer_cost - subsidy_per_ton_co2_reduction
  # The paper claims that this effect has to be negative because ineffient producers are pushed out
  # of the market. As a sanity check, we check if this negative .... it is negative

  # To summarize: 1) We have the price effect which lowers prices for consumers
  # 2) we have the producer cost which is incurred by producers due to lower prices (and maybe also other channels)
  # 3) we have the cost of the subsidy

  subsidy_cost <- subsidy_per_ton_co2_reduction

  # The CO2 abatament cost is:
  subsidy_cost + price_effect + producer_cost
  # which is pretty close to the values reported in Table 7.
  # -> Calculation should be valid.

  #--------------------------------------------------------------------------------------------------------------------#
  # MVPF Calculation
  #--------------------------------------------------------------------------------------------------------------------#

  # The MVPF Calculation is really simple in this case.
  # Individuals value the reduction in CO2 emissions & the effect on producer costs
  # In addition, co2 emissions can leak to other countries because of the European Emissions Trading System. If firms
  # are rational, and sell their certificates the leakage rate should be 1. Yet, a leakage rate of 0 is assumed.
  co2_reducation_valuation <- co2_externality * (1 - carbon_leakage_rate)
  willingness_to_pay <- -price_effect - producer_cost + co2_reducation_valuation
  # The government has to pays for the subsidy that induces emission reductions
  government_net_costs <- subsidy_cost

  return_values <- list(willingness_to_pay = willingness_to_pay,
                        government_net_costs = government_net_costs,
                        program_cost = subsidy_cost,
                        income_loss = -producer_cost,
                        price_effect = -price_effect,
                        co2_emission_reducation = co2_reducation_valuation,
                        prices_year = prices_year)
  return(return_values)
}
