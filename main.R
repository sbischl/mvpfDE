#----------------------------------------------------------------------------------------------------------------------#
# Unified Welfare Analysis for Germany using MVPFs (Marginal Value of Public Funds)
#----------------------------------------------------------------------------------------------------------------------#

# Clear the environment:
rm(list = ls())

# Check if packages are installed
required_packages <- c("ggplot2",
                       "mvtnorm",
                       "dplyr",
                       "readxl",
                       "showtext",
                       "ggrepel",
                       "tidyr",
                       "foreach",
                       "doParallel")

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

#Load libraries
invisible(lapply(required_packages, FUN = library, character.only = TRUE))

# Try to load the alternative font that is used in the pdf exports.
# If this font is not installed / cannot be found, the standard one will be used
tryCatch({
           plot_font <<- "Open Sans"
           font_add(plot_font, "OpenSans-Regular.ttf")
         },
         error = function(e) {
           warning("Font is not installed. Plots are going to use the standard font instead.")
           plot_font <<- "sans"
})
showtext_auto()

# Initilialize parallel environment
registerDoParallel(detectCores(all.tests = FALSE, logical = TRUE) - 1)

# Measure runtime
start_time <- Sys.time()

# Working Directory:
setwd("C:/Users/Simon/IdeaProjects/UnifiedWelfareGer")

# Load functions
source("functions.R")

# Load Assumptions:
source("assumptions.R")

#Prepare Bootstrap
estimate_files <- list.files("./estimates", pattern = "^[^~$].*.xlsx")


for (i in 1:length(estimate_files)) {
  write.csv(drawBootstrap(paste0("./estimates/", estimate_files[i]), bootstrap_replications),
            file = paste0("./bootstrap/", sub(".xlsx", "", estimate_files[i]), "_bootstrap.csv"),
            row.names=FALSE)
}

# Get all programs that are complete in the sense that a correctly named .R file, function and estimates file exists
programs <- getCompletePrograms()

# Run each program with default settings to get the point estimates
mvpf_results <- getPointEstimates(programs)

# Bootstrap
mvpf_results <- addBootstrappedConfidenceIntervalls(mvpf_results)

# Display runtime of the estimation procedure
cat("Estimation completed in ", -(start_time - Sys.time()), " minutes \n")

# Print results:
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


# Present results:

# Tax and Transfer System
plotTaxRates()

# Augment results with additional information for plotting
plot_data <- getPlotData(mvpf_results)

# Plot MVPF
plotResults(plot_data = plot_data, save ="mvpf_against_year.pdf", confidence_intervalls = FALSE)
plotResults(plot_data = plot_data, y_axis = "government_net_costs", y_label = "Government Net Costs", x_axis = "year", x_label = "Year",
            save = "government_net_costs_against_year.pdf", lower_cutoff = 0, upper_cutoff = 4, confidence_intervalls = FALSE, text_labels = TRUE)
plotResults(plot_data = plot_data, y_axis = "willingness_to_pay", y_label = "Willingness to Pay", x_axis = "year", x_label = "Year",
            save = "willingness_to_pay_against_year.pdf", lower_cutoff = 0, upper_cutoff = 4, confidence_intervalls = FALSE, text_labels = TRUE)
plotResults(plot_data = plot_data, x_axis = "program_name", x_label = "Program Name", save = "mvpf_overview.pdf",
            confidence_intervalls = TRUE, text_labels = FALSE, vertical_x_axis_labels =  TRUE)


# Export CSV Files
exportPlotCSV(programs)
#exportPlotCSV(programs, assumption_list = getListOfAllMetaAssumptions(), bootstrap  = FALSE, meta_assumptions = TRUE)