#----------------------------------------------------------------------------------------------------------------------#
# Mandatory Bicycle Law (Active Labor Market Policy):
#----------------------------------------------------------------------------------------------------------------------#

# Relevant Literature:
# Sieg (2014)

bicycleHelmet <- function (bootstrap_replication = 0, internalize_carbon_emissions = 1) {
  program_name <- toString(match.call()[1])
  estimates <- getEstimates(program_name, bootstrap_replication)

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions:
  #--------------------------------------------------------------------------------------------------------------------#

  # Assumptions that can be modified and are not directly from Sieg (2014)
  risk_value_fatal <- global_risk_value_fatal
  risk_value_severe <-  global_risk_value_severe
  risk_value_light <- global_risk_value_light

  resource_cost_fatal <- global_resource_cost_fatal
  resource_cost_severe <- global_resource_cost_severe
  resource_cost_light <- global_resource_cost_light

  total_value_of_statistical_life <- risk_value_fatal + resource_cost_fatal
  value_of_average_injury <- 0.027 * (risk_value_fatal + resource_cost_fatal)
  internalized_injury_cost <- 0.6

  co2_emission_per_kilometer <- 150 # This appears to be a sensible value. See for instance
  # https://www.umweltbundesamt.de/bild/vergleich-der-durchschnittlichen-emissionen-0

  tax_rate <- global_flat_tax

  # All these assumptions are from Sieg (2014). For a description what these variables actually mean, see Sieg (2014) Table 2
  q_head <- 0.364
  q_h <- 0.13
  q_m <- 0.5
  F_g <- 406
  F_s <- 13854
  F_l <- 60516
  rr <- estimates$rr
  r <- 0.045
  h_f <- 6.676443 * 10^-7 * total_value_of_statistical_life
  h_p <- 1.586171 * 10^-6 * total_value_of_statistical_life
  W <- 3.29694 * 10^10
  v_f <- 12.30
  v_c <- 24.9
  v_b <- 17
  v_p <- 4.9

  ms_c <- 0.31
  ms_b <- 0.26
  ms_p <- 0.30

  r_cg <-0.23
  r_bg <-0.02
  r_pg <-1.76
  r_fg <- 1.22

  r_cu <- 0.26
  r_bu <-0.14
  r_pu <- 0.92
  r_fu <- 2.35

  B_f <- 73000000
  q_i <- 0.124
  q_g <- 0.094
  C_H_net <- 27.62
  C_H_tax <- 5.255
  l_H <- 5


#--------------------------------------------------------------------------------------------------------------------#
  # MVPF Calculation (same Notation as Sieg (2014)):
  #--------------------------------------------------------------------------------------------------------------------#

  # Equations 4-6 from Sieg (2014)
  f <- q_h * q_head * rr
  F_gh <- F_g / (1-f)
  F_sh <- (F_s - f*(F_g + F_s)) / ((1-f)^2)
  F_lh <- (F_l*((1-f)^2) +f*(f*(F_g + F_s) - F_s)) / ((1-f)^3)

  # N_h is a intermediate step to calculate N_f
  N_h_private <- (1-rr) * q_head *
    ((risk_value_fatal - risk_value_severe)*F_gh +
      (risk_value_severe- risk_value_light)*F_sh + risk_value_light*F_lh) +
    (1-rr) * q_head *
      ((1 - tax_rate) * (resource_cost_fatal - resource_cost_severe) * F_gh +
        (1 - tax_rate) * (resource_cost_severe- resource_cost_light) * F_sh + (1 - tax_rate) * resource_cost_light*F_lh)

  N_h_public <- (1-rr) * q_head *
    (tax_rate * (resource_cost_fatal - resource_cost_severe)*F_gh +
      tax_rate * (resource_cost_severe - resource_cost_light)*F_sh + tax_rate * resource_cost_light*F_lh)

  # N_f is the valuation of fewer injuries because cycling is more secure
  N_f_private <- N_h_private* (1-q_h) * (1-r)
  N_f_public <- N_h_public * (1-q_h) * (1-r)

  W_cs <- ms_c /(ms_c + ms_b + ms_p)* (((1-q_h) * r * W) / v_f) * ((ms_c + ms_b + ms_p) /(ms_c / v_c + ms_b / v_b + ms_p /v_p))
  W_bs <- ms_b /(ms_c + ms_b + ms_p)* (((1-q_h) * r * W) / v_f) * ((ms_c + ms_b + ms_p) /(ms_c / v_c + ms_b / v_b + ms_p /v_p))
  W_ps <- ms_p /(ms_c + ms_b + ms_p)* (((1-q_h) * r * W) / v_f) * ((ms_c + ms_b + ms_p) /(ms_c / v_c + ms_b / v_b + ms_p /v_p))
  W_s <- (1-q_h) * r * W


  # Health cost because people ride less bicycle
  share_risk_value <- risk_value_fatal / total_value_of_statistical_life
  K_h_private <- h_f * W_s * share_risk_value - h_p * W_ps * share_risk_value +
    (1-tax_rate) * h_f * W_s * (1 - share_risk_value) - (1-tax_rate) * h_p * W_ps * (1-share_risk_value)
  K_h_public <- tax_rate * h_f * W_s * (1 - share_risk_value) - tax_rate * h_p * W_ps * (1-share_risk_value)

  rc_c_private <- share_risk_value * (r_cg * 10^-11 * total_value_of_statistical_life + r_cu * 10^-8 * value_of_average_injury) +
    (1- share_risk_value) * (1 - tax_rate) * (r_cg * 10^-11 * total_value_of_statistical_life + r_cu * 10^-8 * value_of_average_injury)
  rc_c_public <- (1- share_risk_value) * tax_rate * (r_cg * 10^-11 * total_value_of_statistical_life + r_cu * 10^-8 * value_of_average_injury)

  rc_b_private <- share_risk_value * (r_bg * 10^-11 * total_value_of_statistical_life + r_bu * 10^-8 * value_of_average_injury) +
    (1- share_risk_value) * (1 - tax_rate) * (r_bg * 10^-11 * total_value_of_statistical_life + r_bu * 10^-8 * value_of_average_injury)
  rc_b_public <- (1- share_risk_value) * tax_rate * (r_bg * 10^-11 * total_value_of_statistical_life + r_bu * 10^-8 * value_of_average_injury)

  rc_p_private <- share_risk_value *(r_pg * 10^-11 * total_value_of_statistical_life + r_pu * 10^-8 * value_of_average_injury) +
    (1 - share_risk_value) * (1 - tax_rate) * (r_pg * 10^-11 * total_value_of_statistical_life + r_pu * 10^-8 * value_of_average_injury)
  rc_p_public <- (1 - share_risk_value) * tax_rate * (r_pg * 10^-11 * total_value_of_statistical_life + r_pu * 10^-8 * value_of_average_injury)

  rc_f_private <- share_risk_value * (r_fg * 10^-11 * total_value_of_statistical_life + r_fu * 10^-8 * value_of_average_injury) +
    (1 - share_risk_value) * (1 -tax_rate) * (r_fg * 10^-11 * total_value_of_statistical_life + r_fu * 10^-8 * value_of_average_injury)
  rc_f_public <-  (1 - share_risk_value) * tax_rate * (r_fg * 10^-11 * total_value_of_statistical_life + r_fu * 10^-8 * value_of_average_injury)


  N_n_private <- rc_f_private * W_s - rc_p_private * W_ps - rc_c_private * W_cs - rc_b_private * W_bs
  N_n_public <- rc_f_public * W_s - rc_p_public * W_ps - rc_c_public * W_cs - rc_b_public * W_bs

  # Environmental Damage Externality.
  # Assuming a CO2 price of 100€/t, 150g CO2 Emission per Kilometer driven, c_c is 1.5 cent.
  # Sieg (2014) assumes 3.14 cent
  c_c <- co2_emission_per_kilometer / 10^6 * co2_externality
  K_e_private <- internalize_carbon_emissions * c_c * W_cs

  # This is lower bound and equal to the monetary security benefit from wearing a helmet.
  ul <- internalized_injury_cost * q_m * ((N_h_public + N_h_private) / W)

  K_g_private <- ul * (1-q_h) * (1-r) * W

  K_m_private <- B_f * (1- q_i - q_g) * C_H_net / l_H
  K_m_public <- B_f * (1- q_i - q_g) * C_H_tax / l_H

  willingness_to_pay <- N_f_private + N_n_private - (K_g_private + K_m_private + K_e_private + K_h_private)
  government_net_costs <- - K_m_public + K_h_public - (N_f_public + N_n_public)

  return_values <- list(willingness_to_pay =  willingness_to_pay,
                        government_net_costs = government_net_costs,
                        private_safer_traffic_valuation = N_f_private + N_n_private,
                        utility_loss_helmet = -K_g_private,
                        helmet_cost = - K_m_private,
                        co2_emission_reducation = - K_e_private,
                        private_health_cost = -K_h_private,
                        value_added_tax_loss = - K_m_public,
                        public_health_cost = K_h_public,
                        public_safer_traffic_valuation = - (N_f_public + N_n_public))

  return(return_values)
}