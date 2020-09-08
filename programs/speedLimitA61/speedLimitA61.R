#----------------------------------------------------------------------------------------------------------------------#
# Class Room Training (Active Labor Market Policy):
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Thiedig (2018)

speedLimitA61 <- function (bootstrap_replication = 0, internalize_carbon_emissions = 1) {
  program_name <- toString(match.call()[1])
  # This paper does not report any confidence intervalls. The effect of the speed limit on the number and severity of
  # accidents is based on comparing means between a highway that's partly in germany (no speed limit) and in
  # the netherlands (speed limit of 130 kmph).
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Value of avoiding accidents depending on severity of the accident:
  # 1. Resource Cost: "resource cost reflect lost production and economic cost of an accident", Thiedig (2018), p. 54
  resource_cost_fatal <- 1161892 # Table 7.3 Thiedig (2018), p 54
  resource_cost_severe <- 116151 # Table 7.3 Thiedig (2018), p 54
  resource_cost_light <- 4829 # Table 7.3 Thiedig (2018), p 54

  # 2. Risk Value Component:
  # In addition to the resource costs of accidents, there is a private willingness to pay for having a higher probability of
  # being alive and uninjured.
  risk_value_fatal <- 1319104 # Table 7.3 Thiedig (2018), p 54
  risk_value_severe <- 171484 # Table 7.3 Thiedig (2018), p 54
  risk_value_light <- 13191 # Table 7.3 Thiedig (2018), p 54


  # Cost incurred by spending more time on the road:
  # Thiedig (2018) differentiates between business travelers and leisure travelers:
  # The paper assumes that business travelers value 1 hour at 27.48€ and leisure travelers at 9.71€ and then arrives
  # at the following numbers:
  cost_increased_travel_time_business <- 2906001.81 # Table 7.2 Thiedig (2018), p 53
  cost_increased_travel_time_leisure <- 10159703.13 # Table 7.2 Thiedig (2018), p 53
  cost_increased_travel_time <- cost_increased_travel_time_business + cost_increased_travel_time_leisure

  # Effect on Fuel Consumption as estimated by the paper (this is in metric tons), Table 7.5 Thiedig (2018), p 57:
  gasoline_tons_consumption_decrease <- 6368.724
  diesel_tons_consumption_decrease <- 1498.785

  # To calculate the effect on private cost and tax revenue we need the effect on fuel consumption in liters:
  # Luckily, the paper provides the assumed density of gasoline and diesel:
  gasoline_density <- 0.742 # kg / l, see Thiedig (2018) Footnote 71
  diesel_density <- 0.832 # kg / l, see Thiedig (2018) Footnote 71

  # The fuel consumption in liter is given by:
  gasoline_liter_consumption_decrease <- gasoline_tons_consumption_decrease * 1000 / gasoline_density
  diesel_liter_consumption_decrease <- diesel_tons_consumption_decrease * 1000 / diesel_density

  # CO2 Emission Externality:
  co2_social_cost <- co2_externality # Pulling global assumption

  # Reduction of CO2 emissions, see Thiedig (2018) Table 7.5:
  co2_reducation <- 19965.951 + 4764.636

  # Associated cost:
  co2_cost_reducation <- co2_reducation * co2_social_cost # Calculated using the co2 externality assumption. This allows to
  # change this assumption and unify this assumption across reforms if needed. The other types of emissions are unlikely
  # to be relevant for other reforms. -> Use result from Thiedig (2018) directly
  co_cost_reduction <- 37507.06 # Carbon monoxide
  hc_cost_reduction <- 6826.76# Hydrocarbons
  nox_cost_reduction <- 1739104.67 # Nitrogen oxides
  pm_cost_reduction <- 306419.13 # Particulate matter


  # Gas / Diesel Prices:
  # Thiedig uses 2012 prices. ADAC has data on historical prices: see
  # https://www.adac.de/verkehr/tanken-kraftstoff-antrieb/deutschland/kraftstoffpreisentwicklung/
  # Traffic Data is from 2015-2017. Could also use 2015 gas prices and deflate to 2012
  gasoline_price <- 1.598
  diesel_price <- 1.478

  # Energy Tax
  energy_tax_gasoline <- 0.6545
  energy_tax_diesel <- 0.4704

  # Distance driven on relevant highway, see Thiedig (2018) Table 6.2 p. 25
  vehicle_km_per_year <- 1151492875

  # Changes in accidents per million vehicle kilometer, see Thiedig (2018) Table 6.11 p. 45:
  change_fatal_accidents <- 0.00000521873
  change_severe_accidents <- 0.00827941907
  change_light_accidents <- 0.03655438992

  #--------------------------------------------------------------------------------------------------------------------#
  # Calculate MVPF
  #--------------------------------------------------------------------------------------------------------------------#

  # Trips take longer:
  willingness_to_pay <- - cost_increased_travel_time

  # Traffic is more secure (This matches the result from Thiedig 2018):
  # Some of the resource cost of accidents goes to the government. Since no income information is avaiable, the flat_tax
  # rate specified in assumptions is used.
  tax_rate <- global_flat_tax
  private_safer_traffic_valuation <- (vehicle_km_per_year / 10^6) *
    (change_fatal_accidents * ((1- tax_rate) * resource_cost_fatal + risk_value_fatal) +
      change_severe_accidents * ((1- tax_rate) * resource_cost_severe + risk_value_severe) +
      change_light_accidents  * ((1- tax_rate) * resource_cost_light + risk_value_light))

  public_safer_traffic_valuation <- (vehicle_km_per_year / 10^6) *
    (change_fatal_accidents * tax_rate * resource_cost_fatal +
      change_severe_accidents * tax_rate * resource_cost_severe +
      change_light_accidents  * tax_rate * resource_cost_light)

  willingness_to_pay <- willingness_to_pay + private_safer_traffic_valuation
  government_net_costs <- - public_safer_traffic_valuation

  # Effects of lower fuel consumption on private and public budgets:
  private_fuel_cost_saving = gasoline_liter_consumption_decrease * gasoline_price +
    diesel_tons_consumption_decrease * diesel_price
  willingness_to_pay <- willingness_to_pay + private_fuel_cost_saving


  # Goverment looses tax revenue
  value_added_tax_share_of_prices <- (1 - 1 /(1 + value_added_tax))
  value_added_tax_loss <- value_added_tax_share_of_prices * gasoline_liter_consumption_decrease * gasoline_price +
    value_added_tax_share_of_prices * diesel_liter_consumption_decrease * diesel_price
  energy_tax_loss <- gasoline_liter_consumption_decrease * energy_tax_gasoline +
    diesel_liter_consumption_decrease * energy_tax_diesel

  government_net_costs <- government_net_costs + value_added_tax_loss + energy_tax_loss

  # Local Emission Reduction
  local_emission_reduction <- pm_cost_reduction + nox_cost_reduction + co_cost_reduction + hc_cost_reduction
  willingness_to_pay <- willingness_to_pay + local_emission_reduction
  # CO2 Emission Reduction
  willingness_to_pay <- willingness_to_pay + co2_cost_reducation * internalize_carbon_emissions

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        cost_increased_travel_time = -cost_increased_travel_time,
                        private_safer_traffic_valuation = private_safer_traffic_valuation,
                        public_safer_traffic_valuation = -public_safer_traffic_valuation,
                        private_fuel_cost_saving = private_fuel_cost_saving,
                        value_added_tax_loss = value_added_tax_loss,
                        energy_tax_loss = energy_tax_loss,
                        local_emission_reduction = local_emission_reduction,
                        co2_emission_reducation = co2_cost_reducation * internalize_carbon_emissions)
  return(return_values)
}