#----------------------------------------------------------------------------------------------------------------------#
# Unified Welfare Analysis for Germany using MVPFs (Marginal Value of Public Funds)
#----------------------------------------------------------------------------------------------------------------------#

# Clear the environment:
rm(list = ls())

# Working Directory:
setwd("C:/Users/Simon/IdeaProjects/UnifiedWelfareGer")

# Check if packages are installed
required_packages <- c("ggplot2",
                       "mvtnorm",
                       "dplyr",
                       "readxl",
                       "showtext",
                       "ggrepel",
                       "tidyr",
                       "foreach",
                       "doParallel",
                       "knitr",
                       "kableExtra",
                       "patchwork",
                       "xtable")

not_installed_packages <- required_packages[!required_packages %in% installed.packages()[, 1]]

# Ask for user confirmation before installing any packages
if (length(not_installed_packages) > 0) {
  cat("The following required packages are not installed: \n")
  cat(paste(not_installed_packages, collapse = "\n"), "\n")

  while (TRUE) {
    confirmation <- readline("Do you want to install these packages now? (y/n)")
    if (confirmation == "y") {
      install.packages(not_installed_packages)
      break
    }
    else if (confirmation == "n") {
      stop("Need to have required packages installed. Exiting...")
    }
  }
}

# Load all libraries specified above in 'required_packages'
invisible(lapply(required_packages, FUN = library, character.only = TRUE))

# Try to load the alternative font that is used in the pdf exports.
# If this font is not installed / cannot be found, the standard one will be used
tryCatch({
  plot_font <<- "Arial"
  font_add(plot_font, "Arial.ttf")
}, error = function(e) {
  warning("Font is not installed. Plots are going to use the standard font instead.")
  plot_font <<- "sans"
})
showtext_auto()

# Initilialize parallel environment
registerDoParallel(detectCores(all.tests = FALSE, logical = TRUE) - 1)

# Set up table export
options(knitr.kable.NA = '-')

# Measure runtime
start_time <- Sys.time()

# Load functions
source("functions.R")

# Load Assumptions:
source("assumptions.R")

# List all files in the estimates folder to prepare bootstrap
estimate_files <- list.files("./estimates", pattern = "^[^~$].*.xlsx")

# Boostrap all estimates and store them in a list, which contains a dataframe of bootstrapped estimates per program
bootstrapped_estimates <- list()
for (i in 1:length(estimate_files)) {
  bootstrapped_estimates[[sub(".xlsx", "", estimate_files[i])]] <- drawBootstrap(paste0("./estimates/", estimate_files[i]), bootstrap_replications)
}

# Get all programs that are complete in the sense that a correctly named .R file, function and estimates file exists
programs <- getCompletePrograms()

# Run each program with default settings to get the point estimates
mvpf_results <- getPointEstimates(programs)

# Calculate Bootstrap and add bootstrapped CIs to the results
mvpf_results <- addBootstrappedConfidenceIntervalls(mvpf_results)

# Display runtime of the estimation procedure
cat("Estimation completed in ", difftime(Sys.time(), start_time, units='mins'), " minutes \n")

# Print all results to console

for (i in 1:length(programs)) {

  message("Printing results for ", programs[i], ":")
  # The first element is the program name. Therefore, remove the first element
  nonNA_results <- mvpf_results[i, !is.na(mvpf_results[i, ])][-1]
  # Select all results that are no confidence intervall bounds
  point_estimates <- nonNA_results[!grepl(pattern = "_95ci_(upper|lower)", names(nonNA_results))]

  for (j in 1:length(point_estimates)) {
    cat(names(point_estimates[j]), " = ", point_estimates[[j]], "\t 95% CI [", nonNA_results[[paste0(names(point_estimates[j]),"_95ci_lower")]],
        ",", nonNA_results[[paste0(names(point_estimates[j]),"_95ci_upper")]], "]\n", sep = "")
  }
  cat("\n")
}

#----------------------------------------------------------------------------------------------------------------------#
# Present Results
#----------------------------------------------------------------------------------------------------------------------#

# Plot Tax and Transfer System
plotTaxRates()

# Augment results with additional information for plotting
plot_data <- getPlotData(mvpf_results)
category_plot_data <- getCategoryPlotData(plot_data, all_bootstrap_replications_results)

# Plot Results
# Figure that plots the MVPF of all policies against the year they were implemented (with text lables)
plotResults(plot_data = plot_data,
            save ="mvpf_against_year.pdf",
            y_label = "Marginal Value of Public Funds",
            confidence_intervalls = FALSE,
            text_labels =  TRUE)

# Figure that plots the Net Costs of all policies against the year they were implemented (with text lables)
plotResults(plot_data = plot_data,
            y_axis = "government_net_costs_per_program_cost",
            y_label = "Net Costs per Euro Progammatic Expenditure",
            x_axis = "year", x_label = "Year",
            save = "cost_against_year.pdf",
            lower_cutoff = 0,
            upper_cutoff = 3,
            confidence_intervalls = FALSE,
            text_labels = TRUE)

# Figure that plots the WTP of all policies against the year they were implemented (with text lables)
plotResults(plot_data = plot_data,
            y_axis = "willingness_to_pay_per_program_cost",
            y_label = "Willingness to Pay per Euro Progammatic Expenditure",
            x_axis = "year", x_label = "Year",
            save = "wtp_against_year.pdf",
            lower_cutoff = 0,
            upper_cutoff = 4,
            confidence_intervalls = FALSE,
            text_labels = TRUE)

# Figure that plots the MVPF against the BCR
plotResults(plot_data = plot_data,
            y_label = "Marginal Value of Public Funds",
            x_axis = "cost_benefit_ratio", x_label = "Benefit Cost Ratio",
            save = "mvpf_against_bcr.pdf",
            confidence_intervalls = FALSE,
            text_labels = FALSE)

# Figure that plots the MVPF of all policies with 95% CIs
plotResults(plot_data = plot_data,
            x_axis = "program_name",
            x_label = "Program Name",
            y_label = "Marginal Value of Public Funds",
            save = "mvpf_overview.pdf",
            landscape = TRUE,
            confidence_intervalls = TRUE,
            text_labels = FALSE,
            vertical_x_axis_labels =  TRUE)

# Figure that plots the MVPF against the average earnings of beneficiaries with category-averages
plotResults(plot_data = plot_data,
            category_plot_data = category_plot_data,
            y_axis = "mvpf",
            y_label = "Marginal Value of Public Funds",
            x_axis = "average_earnings_beneficiary",
            x_label = "Average Earnings of Beneficiaries",
            save = "mvpf_categories_earnings.pdf",
            confidence_intervalls = TRUE,
            text_labels = FALSE)

# Figure that plots the MVPF against the average age of beneficiaries with category-averages
plotResults(plot_data = plot_data,
            category_plot_data = category_plot_data,
            y_axis = "mvpf",
            y_label = "Marginal Value of Public Funds",
            x_axis = "average_age_beneficiary",
            x_label = "Age of Beneficiaries",
            save = "mvpf_categories_age.pdf",
            confidence_intervalls = TRUE,
            text_labels = FALSE)

# Robustness Check that re-estimates everything with various discount rates and displays each specifiction in a combined plot
robustnessCheck(programs,
                robustnesscheck_assumptions = function(specification) {
                  if (specification == 1) {
                    discount_rate <<- 0
                  }
                  else if (specification == 2) {
                    discount_rate <<- 0.01
                  }
                  else if (specification == 3) {
                    # baseline
                    discount_rate <<- 0.03
                  }
                  else if (specification == 4) {
                    discount_rate <<- 0.07
                  }
                },
                headlines = c("ρ = 0", "ρ = 0.01", "ρ = 0.03 (Baseline)", "ρ = 0.07"),
                save = "robustness_check_discount_rate.pdf")

# Robustness Check that re-estimates everything with various tax rate asumptions and displays each specifiction in a combined plot
robustnessCheck(programs,
                robustnesscheck_assumptions = function(specification) {
                  if (specification == 1) {
                    global_assume_flat_tax <<- TRUE
                    global_flat_tax <<- 0.1
                  }
                  else if (specification == 2) {
                    global_income_tax_only <<- TRUE
                  }
                  else if (specification == 3) {
                    # nothing baseline
                  }
                  else if (specification == 4) {
                    global_assume_flat_tax <<- TRUE
                    global_flat_tax <<- 0.5
                  }
                },
                headlines = c("τ = 0.1", "Only Income Tax", "Taxes and Transfers (Baseline)", "τ = 0.5"),
                save = "robustness_check_tax_rate.pdf")


# Exports all possible combinations of assumptions specified in getListOfAllMetaAssumptions().
# Takes about 2 hours with 3 parallel threads. Only relevant for the web visualization.
# exportPlotCSV(programs, assumption_list = getListOfAllMetaAssumptions(), bootstrap  = FALSE, meta_assumptions = TRUE)#

# Export Tables:
exportLatexTables(plot_data)

# Copy Files:
FolderCopy()

# Additional Results:
project_lifetime_impact(impact_age = age_university_enrollment,
                        impact_magnitude_matrix =  getEducationEffectOnEarnings(education_decision = "university_degree",
                                                                                alternative = "abitur"),
                        relative_control_income = getRelativeControlGroupEarnings("abitur"),
                        start_projection_year = 2010,
                        prices_year = 2010,
                        inculde_welfare_benefits_fraction = 0)

# Average Tax Rate of someone working full time: 47928 is the average gross income of someone working full time:
# https://www.destatis.de/DE/Themen/Arbeit/Verdienste/Verdienste-Verdienstunterschiede/Tabellen/liste-bruttomonatsverdienste.html
getAverageTaxRate(47928,
                  inculde_welfare_benefits_fraction = 1,
                  income_fraction_of_pension_contribution = 1,
                  income_fraction_of_unemployment_insurance_contribution = 1,
                  income_fraction_of_long_term_care_contribution = 0,
                  income_fraction_of_health_insurance_contribution = 0)

getAverageTaxRate(47928,
                  income_tax_only = TRUE)

# Bafög Repayment Reform:
(mvpf_results %>% filter(program == "bafoegRepayment"))["program_cost"]
(mvpf_results %>% filter(program == "bafoegRepayment"))["tax_revenue_increase"]
(mvpf_results %>% filter(program == "bafoegRepayment"))["education_cost"]
(mvpf_results %>% filter(program == "bafoegRepayment"))["bafoeg_cost"]
(mvpf_results %>% filter(program == "bafoegRepayment"))["net_income_increase"]

(mvpf_results %>% filter(program == "bafoegRepayment"))["willingness_to_pay"]
(mvpf_results %>% filter(program == "bafoegRepayment"))["government_net_costs"]

#MVPF with envelope theorem:
((mvpf_results %>% filter(program == "bafoegRepayment"))["willingness_to_pay"] -
  (mvpf_results %>% filter(program == "bafoegRepayment"))["net_income_increase"]) /
   (mvpf_results %>% filter(program == "bafoegRepayment"))["government_net_costs"]


0.04 * project_lifetime_impact(impact_age = age_university_enrollment,
                        impact_magnitude_matrix = getEducationEffectOnEarnings(education_decision = "university_degree",
                                                                               alternative = "abitur"),
                        relative_control_income = getRelativeControlGroupEarnings("abitur"),
                        start_projection_year = 1990,
                        prices_year = 2010,
                        inculde_welfare_benefits_fraction = 0)

0.04 * (costOfCollege(duration_of_study = 5, year = 1990, prices_year = 2010) -
  costOfSchool(duration_of_schooling = 3, year = 1990, prices_year = 2010, school_type = "berufsschule_dual"))

# Bafög
596 * 0.51129 * deflate(from = 1990, to = 2010)

# Retraining:
(mvpf_results %>% filter(program == "retraining"))["willingness_to_pay"]
(mvpf_results %>% filter(program == "retraining"))["government_net_costs"]
(mvpf_results %>% filter(program == "retraining"))["program_cost"]
(mvpf_results %>% filter(program == "retraining"))["tax_revenue_increase"]
(mvpf_results %>% filter(program == "retraining"))["net_income_increase"]


(mvpf_results %>% filter(program == "retraining"))["willingness_to_pay"] / (mvpf_results %>% filter(program == "retraining"))["government_net_costs"]

# Unemployment Benefits:
(mvpf_results %>% filter(program == "unemploymentBenefits42"))["willingness_to_pay"]
(mvpf_results %>% filter(program == "unemploymentBenefits42"))["fiscal_externality"]

(mvpf_results %>% filter(program == "unemploymentBenefits42"))["willingness_to_pay"] /
  ((mvpf_results %>% filter(program == "unemploymentBenefits42"))["fiscal_externality"] + 1)

# Maternity Leave:
deflate(from = 1992, to = 2010) * 750 * 0.51129
1713.59 * 0.51129 * deflate(from = 1992, to = 2010)
# FE
sum((mvpf_results %>% filter(program == "maternityLeave79"))[c("tax_revenue_increase","tax_revenue_increase_children")])


# 2007 Parental Leave:
deflateReturnValues(parentalLeave2007(), 2010)
deflateReturnValues(homeCareSubsidy(), 2010)

# Climate Policy:
deflateReturnValues(eegSolar(), 2010)
deflateReturnValues(eegWind(), 2010)

# Approximate share of co2 reduction
plot_data %>%
  filter(category == "Climate Policy") %>%
  mutate(emission_share = co2_emission_reducation / willingness_to_pay) %>%
  select(mvpf, program, emission_share) %>%
  pull(emission_share) %>% mean()

# Average MVPF of 2000s tax reforms.
getCategoryPlotData(plot_data %>% filter(program != "taxReform1990", category == "Top Tax Reform"), bootstrap_results = all_bootstrap_replications_results)

# Comparison of Cost Benefit Ratio and MVPF
impute_missing_program_costs(plot_data) %>% filter(mvpf != Inf) %>% select(cost_benefit_ratio, mvpf, program) %>% arrange(-cost_benefit_ratio)