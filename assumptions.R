# Economic Assumptions:
discount_rate <- 0.03
retirement_age <- 65
wage_growth_rate <- 0.005

# Tax System Assumptions:
global_assume_flat_tax <- FALSE # If this is set to TRUE, only flat_tax is relevant. Otherwise flat_tax is irrelevant
global_flat_tax <- 0.3
global_inculde_welfare_benefits_fraction <- 1 #The share of welfare_benefits 'Hartz IV' invidivudals receive.
global_income_fraction_of_pension_contribution <- 1 #The fraction of pension contributions that is considered income
global_welfare_benefit_monthly <- 700
value_added_tax <- 0.19

# Education
yearly_return_to_schooling <- 0.074
duration_of_study <- 5
duration_of_berufsschule <- 3

# Environment Policies:
co2_externality <- 86.5 # Externality in € caused by one additional ton of CO2 emissions. 86.5 is the value from Thiedig (2018)

# Bootstrap Settings
bootstrap_seed <- 24135693
bootstrap_replications <- 250
correlation_between_estimates <- 1