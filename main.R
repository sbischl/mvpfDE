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
                       "tidyr")

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
      stop("Need to install required packages. Exiting.")
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


# Get List of all programs
programs <- list.dirs("./programs", full.names = FALSE, recursive = FALSE)
complete_programs <- c()
for (i in 1:length(programs)) {

  # Check if *.xlsx file in estimates folder exists
  if (!file.exists(paste0("./estimates/", programs[i], ".xlsx"))) {
    warning(paste0("No correctly named .xlsx file for program \"", programs[i], "\" was found. ",
                   "Make sure that for each program there exists a Excel (.xlsx) file in the \"estimates\" folder."))
    next
  }


  # Check if file .R file exists
  current_program_path <- paste0("./programs/", programs[i],  "/", programs[i], ".R")
  if (!file.exists(current_program_path)) {
    warning(paste0("No correctly named .R file for program \"", programs[i], "\" was found. ",
                  "Make sure that for each directory there exists a .R file within this directory with the same name."))
    next
  }

  # Check if the required function exists
  source(current_program_path)
  if (!exists(programs[i])) {
    warning(paste0("The file \"", programs[i], ".R\" does not contain a function named \"", programs[i],
                  "\". Make sure that the function that should be called to calculate the MVPF is correctly named."))
    next
  }
  complete_programs <- c(complete_programs, programs[i])
}
programs <- complete_programs



# Run each program with default settings to get the point estimates
mvpf_results <- data.frame(program = programs)
for (i in 1:length(programs)) {
  return_values <- do.call(programs[i], list())
  # Check if return_values include the necessary "willingness_to_pay" and "government_net_costs"
  if (!all(c("willingness_to_pay", "government_net_costs") %in% names(return_values))) {
    warning(paste0(programs[i], "does not include willingness_to_pay and / or government_net_costs. Cannot calculate MVPF"))
    mvpf_results[names(return_values), ] <- unlist(return_values)
    next
  }
  mvpf_results[i, names(return_values)] <- unlist(return_values)
  mvpf_results[i, "mvpf"] <- calculateMVPF(mvpf_results[i, "willingness_to_pay"], mvpf_results[i, "government_net_costs"])
}


# Bootstrap
for (i in 1:length(programs)) {
  # Preallocate data.frame with number of replications rows
  bootstrapped_mvpf_results <- data.frame(replication = 1:bootstrap_replications)

  # Run 'number of replications' bootstrap replications
  for (j in 1:bootstrap_replications) {
    return_values <- do.call(programs[i], list(bootstrap_replication = j))
    bootstrapped_mvpf_results[j, names(return_values)] <- unlist(return_values)
    bootstrapped_mvpf_results[j, "mvpf"] <-
    calculateMVPF(bootstrapped_mvpf_results[j, "willingness_to_pay"], bootstrapped_mvpf_results[j, "government_net_costs"])
  }

  # Calculate confidence intervall for all return values
  for (k in 1:length(return_values)) {
    mvpf_results[i, paste0(names(return_values)[k], "_95ci_lower")] <-
      quantile(bootstrapped_mvpf_results[, names(return_values)[k]], 0.025)
    mvpf_results[i, paste0(names(return_values)[k], "_95ci_upper")] <-
      quantile(bootstrapped_mvpf_results[, names(return_values)[k]], 0.975)
  }

  # The interpretation of the MVPF changes when the willingsness to pay and the government net costs are both negative.
  # Although the sign of the MVPF is also positive. Example: Consider MVPF = 0.5
  # This means that either
  # (1) Willingness to pay and goverment net costs are positive. In this case one dollar spent by the government is valued
  # with 0.5 dollar by the beneficiaries of the reform (higher value better)
  # (2) Willingness to pay and goverment net costs are negative. In this case the MVPF measures how much WTP is lost
  # per tax revenue increase. (lower value better)
  # If the point estimate is either (1), and one of the bootstrapped estimates is (2) (or vice versa)
  # the bootstrapped estimate is out of the sensible range and is thus not defined.
  # When running the bootstrap, this possibility has to be acconted for by removing the replications where the MVPF is
  # not defined, and adjusting the confidence intervall.
  replication_defined <- rep (TRUE, bootstrap_replications)
  for (j in 1:bootstrap_replications) {
    if (all(mvpf_results[i, c("willingness_to_pay", "government_net_costs")] >= 0) &
      all(bootstrapped_mvpf_results[j, c("willingness_to_pay", "government_net_costs")] < 0)) {
      replication_defined[j] <- FALSE
    }
    else if (all(mvpf_results[i, c("willingness_to_pay", "government_net_costs")] < 0) &
      all(bootstrapped_mvpf_results[j, c("willingness_to_pay", "government_net_costs")] < 0)) {
      replication_defined[j] <- FALSE
    }
  }

  # Adjust the confindence intervall to account for the fact that the non-defined estimates have been removed.
  # Note that the 95% confidence intervall can reach into the non-defined region of the mvpf. In this case,
  # the confidence intervall spans -Inf to +Inf.
  upper_percentile_95ci <- 1 - (0.05 - sum(!replication_defined) / bootstrap_replications) / 2
  lower_percentile_95ci <- (0.05 - sum(!replication_defined) / bootstrap_replications) / 2


  if (lower_percentile_95ci  <= 0 & upper_percentile_95ci >= 1) {
    mvpf_results$mvpf_95ci_lower[i] <- -Inf
    mvpf_results$mvpf_95ci_upper[i] <- Inf
  }
  else {
    mvpf_results$mvpf_95ci_lower[i] <- quantile(bootstrapped_mvpf_results[replication_defined, "mvpf"], lower_percentile_95ci)
    mvpf_results$mvpf_95ci_upper[i] <- quantile(bootstrapped_mvpf_results[replication_defined, "mvpf"], upper_percentile_95ci)
  }
}

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

# Load additional information about each policy from the excel file:
program_information <- as.data.frame(read_xlsx("programs.xlsx"))
# Join the estimates and the additional information:
plot_data <- left_join(mvpf_results, program_information, by = c("program" = "program_identifier"))
plotResults(plot_data = plot_data, save ="MVPF.pdf", confidence_intervalls = TRUE)
plotResults(plot_data = plot_data, y_axis = "government_net_costs", y_label = "Government Net Costs", x_axis = "year", x_label = "Year",
            save = "NetCosts.pdf", lower_cutoff = 0, upper_cutoff = 4, confidence_intervalls = TRUE, text_labels = TRUE)
plotResults(plot_data = plot_data, y_axis = "willingness_to_pay", y_label = "Willingness to Pay", x_axis = "year", x_label = "Year",
            save = "WTP.pdf", lower_cutoff = 0, upper_cutoff = 4, confidence_intervalls = TRUE, text_labels = TRUE)

# Tax and Transfer System
plotTaxRates()
