#----------------------------------------------------------------------------------------------------------------------#
# Changeable Assumptions
#----------------------------------------------------------------------------------------------------------------------#

# Economic Assumptions:
discount_rate <- 0.03
retirement_age <- 64 # This is more or less set in stone by the IAB data. We cannot go higher only lower.
wage_growth_rate <- 0.005

# Tax System Assumptions:
global_assume_flat_tax <- FALSE # If this is set to TRUE, only flat_tax is relevant. Otherwise flat_tax is only
# used where no income information is available.
global_flat_tax <- 0.2810786
global_income_tax_only <- FALSE # If this is set to TRUE, the income tax is the only tax
global_flat_tax_income_tax_only <- 0.1827759 # Irrelevant unless global_income_tax_only is set to TRUE

global_inculde_welfare_benefits_fraction <- 1 #The share of welfare_benefits 'Hartz IV' invidivudals receive.
global_income_fraction_of_pension_contribution <- 1 #The fraction of pension contributions that is considered income
global_income_fraction_of_unemployment_insurance_contribution <- 1 #The fraction of unemployment insurance contributions that is considered income
global_income_fraction_of_health_insurance_contribution <- 0 #The fraction of pension contributions that is considered income
global_income_fraction_of_long_term_care_contribution <- 0 #The fraction of unemployment insurance contributions that is considered income
global_welfare_benefit_monthly <- 700
value_added_tax <- 0.19
cost_of_raising_public_funds <- 0.3 # Relevant for the more traditional cost benefit ratio

# Preferences
global_relative_risk_aversion <- 2

# Value of statistical Life and Injuries:
use_single_statistical_life_value <- TRUE # If set to true only one statistical life value has to be specified. All of the
# other values for injuries and the differentiation between resource cost and risk value are derived from the value set
# in value_of_statistical_life
value_of_statistical_life <-1161892 + 1319104  # Should be the sum of resource cost and risk value (see below)
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
yearly_return_to_schooling <- 0.07
duration_of_study <- 5
duration_of_berufsschule <- 3
additional_years_of_schooling_university <- duration_of_study - duration_of_berufsschule
age_university_enrollment <- 20 #This is basically given by the IAB data. For > 20, the cost in terms of foregone
# income of going to university is underestiamted. For < 20 there is either no change or the esimation breaks because of 0 by 0 division.

# Environment Policies:
co2_externality <- 100 # Externality in € caused by one additional ton of CO2 emissions. 86.5 is the value used by Thiedig (2018)


#----------------------------------------------------------------------------------------------------------------------#
# Settings
#----------------------------------------------------------------------------------------------------------------------#

# Bootstrap Settings
bootstrap_seed <- 24135693
bootstrap_replications <- 25
correlation_between_estimates <- 1

# Prices
results_prices <- 2010 # The year to which results should be deflated
exclude_variables_from_price_adjustment <- c("prices_year") # All variables that are returned by a program (willingness_to_pay, program_cost ...)
# are deflated to results_prices automatically except those in exclude_variables_from_price_adjustment
disable_deflating <- FALSE
# Exclude programs from deflaiting where all effects are stated per euro. E.g for tax reform we are always looking at
# the effects of a one euro tax cut. These effects do not depend on prices and it would be weird if every tax reform
# had a different WTP depending on the year it was implemented.
excluded_from_deflating <- c("taxReform2005",
                             "taxReform2004",
                             "taxReform2001",
                             "taxReform1990",
                             "unemploymentBenefits2006",
                             "unemploymentBenefits42",
                             "unemploymentBenefits44",
                             "unemploymentBenefits49")

# Plots and Tables:
order_of_categories <- c("Top Tax Reform",
                         "Education",
                         "Job Training",
                         "Start up Subsidy",
                         "Subsidized Employment",
                         "Other Labor Market Policies",
                         "Unemployment Insurance",
                         "Parental Leave Reform",
                         "Climate Policy",
                         "Health Program") #This determines the order in which the reforms categories are displayed in the graphs
# and tables.
# Exclude Programs from category average:
excluded_from_category_average <- c("bicycleHelmet", "coronavirusLockdownR1")
excluded_from_all_plots <- c("bicycleHelmet")


#----------------------------------------------------------------------------------------------------------------------#
# Apply Assumptgions: (No relevant settings / assumptions beyond this point)
#----------------------------------------------------------------------------------------------------------------------#
applyAssumptions <- function() {
  # Each of the assumptions relative to the sum of resource cost and risk value
  if (use_single_statistical_life_value) {
    # This scales all the injury valuation as they are in Thiedig (2018)
    sum <- global_resource_cost_fatal + global_risk_value_fatal
    share_global_resource_cost_fatal <<- global_resource_cost_fatal / sum
    share_global_resource_cost_severe <<- global_resource_cost_severe / sum
    share_global_resource_cost_light <<- global_resource_cost_light / sum

    share_global_risk_value_fatal <<- global_risk_value_fatal / sum
    share_global_risk_value_severe <<- global_risk_value_severe / sum
    share_global_risk_value_light <<- global_risk_value_light / sum

    global_resource_cost_fatal <<- share_global_resource_cost_fatal * value_of_statistical_life
    global_resource_cost_severe <<- share_global_resource_cost_severe * value_of_statistical_life
    global_resource_cost_light <<-  share_global_resource_cost_light * value_of_statistical_life

    global_risk_value_fatal <<- share_global_risk_value_fatal * value_of_statistical_life
    global_risk_value_severe <<- share_global_risk_value_severe * value_of_statistical_life
    global_risk_value_light <<-  share_global_risk_value_light * value_of_statistical_life
  }

  if (global_income_tax_only) {
    global_flat_tax <<- global_flat_tax_income_tax_only
  }
}
applyAssumptions()

