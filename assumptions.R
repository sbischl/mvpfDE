# Economic Assumptions:
discount_rate <- 0.03
retirement_age <- 65
wage_growth_rate <- 0.005

# Tax System Assumptions:
global_assume_flat_tax <- FALSE # If this is set to TRUE, only flat_tax is relevant. Otherwise flat_tax is only
# used where no absolutely no income information is available.
global_flat_tax <- 0.3
global_inculde_welfare_benefits_fraction <- 1 #The share of welfare_benefits 'Hartz IV' invidivudals receive.
global_income_fraction_of_pension_contribution <- 1 #The fraction of pension contributions that is considered income
global_income_fraction_of_unemployment_insurance_contribution <- 1 #The fraction of unemployment insurance contributions that is considered income
global_income_fraction_of_health_insurance_contribution <- 0 #The fraction of pension contributions that is considered income
global_income_fraction_of_long_term_care_contribution <- 0 #The fraction of unemployment insurance contributions that is considered income
global_welfare_benefit_monthly <- 700
value_added_tax <- 0.19

# Value of statistical Life and Injuries:
# Resource Cost: "resource cost reflect lost production and economic cost of an accident", Thiedig (2018), p. 54
global_resource_cost_fatal <- 1161892 # Table 7.3 Thiedig (2018), p 54
global_resource_cost_severe <- 116151 # Table 7.3 Thiedig (2018), p 54
global_resource_cost_light <- 4829 # Table 7.3 Thiedig (2018), p 54

# Risk Value Component: private willingness to pay for having a higher probability of being alive and uninjured.
global_risk_value_fatal <- 1319104 # Table 7.3 Thiedig (2018), p 54
global_risk_value_severe <- 171484 # Table 7.3 Thiedig (2018), p 54
global_risk_value_light <- 13191 # Table 7.3 Thiedig (2018), p 54

# Education
global_use_constant_ols_return_to_schooling <- FALSE
yearly_return_to_schooling <- 0.05
duration_of_study <- 5
duration_of_berufsschule <- 3

# Environment Policies:
co2_externality <- 86.5 # Externality in € caused by one additional ton of CO2 emissions. 86.5 is the value from Thiedig (2018)

# Bootstrap Settings
bootstrap_seed <- 24135693
bootstrap_replications <- 100
correlation_between_estimates <- 1