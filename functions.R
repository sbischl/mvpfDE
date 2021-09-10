#----------------------------------------------------------------------------------------------------------------------#
# Core Functions
#----------------------------------------------------------------------------------------------------------------------#

# Get all programs that are complete in the sense that a correctly named .R file, function and estimates file exists
getCompletePrograms <- function() {
  # Get List of all programs
  programs <- list.dirs("./programs", full.names = FALSE, recursive = FALSE)
  complete_programs <- NULL
  for (i in 1:length(programs)) {

    # Check if *.xlsx file in estimates folder exists
    if (!file.exists(paste0("./estimates/", programs[i], ".xlsx"))) {
      warning(paste0("No correctly named .xlsx file for program \"", programs[i], "\" was found. ",
                     "Make sure that for each program there exists a Excel (.xlsx) file in the \"estimates\" folder."))
      next
    }


    # Check if .R file exists
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
  return(complete_programs)
}

# getEstimates takes the program identifier and the bootstrap replication as inputs and returns a one row dataframe
# of (potentially) bootstrapped estimates. bootstrap_replication = 0 returns the point estimate.
getEstimates <- function(program, bootstrap_replication) {

  if (bootstrap_replication == 0) {
    estimates <- read_xlsx(paste0("./estimates/", program, ".xlsx"))
    point_estimates <- data.frame(t(estimates$point_estimate))
    colnames(point_estimates) <- estimates$estimate
    return(point_estimates)
  }
  return(as.data.frame(bootstrapped_estimates[[program]][bootstrap_replication, , drop = FALSE]))
}

# Calculates the MVPF taking WTP and Net Cost as inputs
calculateMVPF <- function(willingness_to_pay , government_net_costs) {
  # the inputs can either be a scalar or a vector. In case of a vector multiple MVPF are returned.
  # This is useful for boostrapping the MVPF
  mvpf <- rep(NA, length(willingness_to_pay))
  for (j in 1:length(willingness_to_pay)) {
    if (willingness_to_pay[j] > 0 & government_net_costs[j] <= 0) {
      mvpf[j] <- Inf
    }
    else if (willingness_to_pay[j] < 0 & government_net_costs[j] == 0) {
      mvpf[j] <- -Inf
    }
    else {
      mvpf[j] <- willingness_to_pay[j] / government_net_costs[j]
    }
  }
  return(mvpf)
}

# helper function to quickly calculate the mvpf of a program, takes program identifier as input
getMVFP <- function(program) {
  result <- do.call(program, list())
  calculateMVPF(result$willingness_to_pay, result$government_net_costs)
}

# Calculate the point estimates of all programs.
getPointEstimates <- function(programs, disable_deflating = F) {
  mvpf_results <- data.frame(program = programs)
  for (i in 1:length(programs)) {
    message(paste("Running", programs[i], "once to get the point estimate."))
    # Call program "i" and store the return values
    return_values <- do.call(programs[i], list())

    # Deflate unless deflating is explicitely disabled.
    if (!disable_deflating && !programs[i] %in% excluded_from_deflating) {
      return_values <- deflateReturnValues(return_values, results_prices)
    }

    # Check if return_values include the necessary "willingness_to_pay" and "government_net_costs"
    if (!all(c("willingness_to_pay", "government_net_costs") %in% names(return_values))) {
      warning(paste0(programs[i], "does not include willingness_to_pay and / or government_net_costs. Cannot calculate MVPF"))
      mvpf_results[names(return_values), ] <- unlist(return_values)
      next
    }
    # Store the return values in the results data.frame
    mvpf_results[i, names(return_values)] <- unlist(return_values)
    # Calculate the MVPF and also store it in the results data.frame
    mvpf_results[i, "mvpf"] <- calculateMVPF(mvpf_results[i, "willingness_to_pay"], mvpf_results[i, "government_net_costs"])
  }
  return(mvpf_results)
}

# Run Programs without printing messages
quietelyRunPrograms <- function(programs, bootstrap = FALSE) {
  results <- suppressMessages(getPointEstimates(programs))
  if (bootstrap) {
    results <- suppressMessages(addBootstrappedConfidenceIntervalls(results))
  }
  return(results)
}

# Takes in results e.g. from getPointEstimates and transforms the dataframe in a list. This makes it easier to access
# specific elements
listifyResults <- function(results, saveToRMarkDownFolder = FALSE) {
  list <- setNames(split(results, seq(nrow(results))), results$program)
  if (saveToRMarkDownFolder) {
    saveRDS(list, "./rmarkdown/results.RData")
  }
  return(list)
}

# Deflates using the cpi
deflate <- function(from, to) {
  if (!exists("cpi")) {
    # Load consumer price index into the global environment
    cpi <<- read.csv("./cpi/cpi.csv")
  }

  if (from == to) {
    return(1)
  }

  index_from <- cpi[cpi$year == from, "index"]
  index_to <- cpi[cpi$year == to, "index"]

  if(length(index_from) == 0 | length(index_to) == 0) {
    warning("You are trying to deflate to or from a year for which there is no data available")
  }

  return(index_to / index_from)
}

# This function deflates the return values of some program e.g. classRoomTraining to 'year' prices
deflateReturnValues <- function(return_values, year) {

  if (disable_deflating) {
    return(return_values)
  }

  if (!"prices_year" %in% names(return_values)) {
    # The return values do not contain prices_year -> we don't know from which year to deflate. -> Assume that results
    # are already correctly deflated
    return(return_values)
  }

  # Vector to deselect variables that should not be deflated:
  variables_to_deflate <- !names(return_values) %in% exclude_variables_from_price_adjustment

  # Deflate variables
  return_values[variables_to_deflate] <- lapply(return_values[variables_to_deflate], function(var) {
    return(deflate(from = return_values[["prices_year"]], to = year) * var)
  })

  return(return_values)
}

# Use this function if a cash flow is split evenly across multiple periods.
# This function then returns the present value of the cash flow
splitAndDiscount <- function(amount, periods, discount_rate) {
  cash_flows <- rep(amount / periods, periods)
  present_value_cash_flows <- cash_flows / sapply(1:periods, function(x) {
    (1 + discount_rate)^x})
  return(sum(present_value_cash_flows))
}

# Returns the factor that some cash flow in year "from" has to be multiplied by to get the discounted value in year "to"
discount <- function(from, to) {
  return(1/(1+discount_rate)^(from - to))
}

# Returns a vector that contains the discount factor for all periods
discountVector <- function(periods) {
  return((1/(1 + discount_rate))^(0:(periods-1)))
}

# Calculates the present value of a cash flow which occurs every month
discountMonthlyCashFlow <- function(amount, months) {
  full_years <- months %/% 12
  remaining_months <- months %% 12
  if (remaining_months > 0) {
    return(sum(c(rep(amount*12, full_years), remaining_months*amount) * discountVector(full_years + 1)))
  }
  return(sum(rep(amount*12, full_years) * discountVector(full_years)))
}

#----------------------------------------------------------------------------------------------------------------------#
# Bootstrap Related Functions
#----------------------------------------------------------------------------------------------------------------------#
drawBootstrap <- function(path_to_xlsx, number_of_replications) {
  # Set seed
  set.seed(bootstrap_seed)

  # Load Estimates
  estimates <- as.data.frame(read_xlsx(path_to_xlsx))

  if(nrow(estimates) == 0) {
    # If estimates are empty, the code below wont work.
    return(data.frame())
  }

  # Recoup standard error from t-statistic where standard error is missing
  estimates <- estimates %>%
    mutate(standard_error = coalesce(standard_error,
                                     abs(point_estimate / t_statistic),
                                     ((ci95_high - ci95_low) / 2) / qnorm(0.975),
                                     abs(point_estimate / qnorm(p_value / 2))))

  # Calculate correlation between estimates:

  # Generate an identity matrix
  correlation_matrix <- diag(nrow(estimates))

  # Replace the off-diagonal zeroes with the appropriate correlation between the estimates
  for (i in 1:nrow(correlation_matrix)) {
    # If the ith estimate has no correlation direction, it is uncorrelated with other estimates and the ith row
    # of the correlation matrix is all zeros except for the element on the diagonal
    if (is.na(estimates$correlation_direction[i])) {
      next
    }

    for (j in 1:ncol(correlation_matrix)) {

      # If the jth estimate has no correlation direction, it is uncorrelated with other estimates and the jth row
      # of the correlation matrix is all zeros except for the element on the diagonal
      if (is.na(estimates$correlation_direction[j])) {
        next
      }

      if (abs(estimates$correlation_direction[i]) == abs(estimates$correlation_direction[j])) {
        correlation_matrix[i, j] <- sign(estimates$correlation_direction[i] * estimates$correlation_direction[j]) *
          correlation_between_estimates
      }
    }
  }

  # Empty matrix which will store the bootstrapped estimates. One row for each replication.
  bootstrapped_estimates <- matrix(data = NA, nrow = number_of_replications, ncol = nrow(estimates))
  colnames(bootstrapped_estimates) <- estimates$estimate

  # Populate the 'bootstrapped_estimates' matrix by iterating over all rows
  for (i in 1:number_of_replications) {
    # This standard error vector is stochastic if for some estimates only the p-value range is known
    standard_error_vector <- coalesce(estimates$standard_error,
                               abs(estimates$point_estimate / qnorm((estimates$p_value_low + (estimates$p_value_high - estimates$p_value_low) * runif(nrow(estimates))) / 2)),
                                      0)

    bootstrapped_estimates[i, ] <- rmvnorm(n = 1, mean = estimates$point_estimate,
                                          sigma = correlationToCovarianceMatrix(correlation_matrix, standard_error_vector))
  }

  return(bootstrapped_estimates)

}

addBootstrappedConfidenceIntervalls <- function(mvpf_results) {
  # This requires the dataframe that was returned from getPointEstimates as input
  programs <- mvpf_results$program

  all_bootstrap_replications_results <<- list() # Contains a dataframe for each program with the estimated results from
  # each bootstrap replication.

  # Bootstrap
  for (i in 1:length(programs)) {
    message(paste("Running", bootstrap_replications, "bootstrap replications for", programs[i] ))

    # This function generates a single row of a dataframe that contains one bootstrap replication
    single_bootstrap_replication <- function(j) {
      return_values <- do.call(programs[i], list(bootstrap_replication = j))
      return_values <- deflateReturnValues(return_values, results_prices)
      replication_row <- data.frame(replication = j)
      replication_row[, names(return_values)] <- unlist(return_values)
      replication_row$mvpf <- calculateMVPF(replication_row$willingness_to_pay, replication_row$government_net_costs)
      return(replication_row)
    }

    # Call single_bootstrap_replication once for each bootstrap replication and row bind all the dataframe rows
    bootstrapped_mvpf_results <- foreach(j = 1:bootstrap_replications,
                                         .combine = rbind,
                                         .export = ls(globalenv())[!ls(globalenv()) %in% c("programs", "i")], #this gets rid of some warnings
                                         .packages = 'dplyr') %dopar% {
      single_bootstrap_replication(j)
    }
    # Store the results to be accessed later
    all_bootstrap_replications_results[[programs[i]]] <<- bootstrapped_mvpf_results

    # Calculate confidence intervall here for all columns in the dataframe except: replication & mvpf
    ci_variables <- names(bootstrapped_mvpf_results)[!names(bootstrapped_mvpf_results) %in% c("replication", "mvpf")]
    for (k in 1:length(ci_variables)) {
      mvpf_results[i, paste0(ci_variables[k], "_95ci_lower")] <-
        quantile(bootstrapped_mvpf_results[, ci_variables[k]], 0.025)
      mvpf_results[i, paste0(ci_variables[k], "_95ci_upper")] <-
        quantile(bootstrapped_mvpf_results[, ci_variables[k]], 0.975)
    }

    # The calculation of the CI for the MVPF is non-trivial. See comment at the beginning of calculateMVPFCI(.)
    mvpf_ci <- calculateMVPFCI(willingness_to_pay_pe = mvpf_results[i, "willingness_to_pay"],
                               government_net_costs_pe = mvpf_results[i, "government_net_costs"],
                               willingness_to_pay_boostrap = bootstrapped_mvpf_results[, "willingness_to_pay"],
                               government_net_costs_bootstrap = bootstrapped_mvpf_results[, "government_net_costs"])

    mvpf_results$mvpf_95ci_lower[i] <- mvpf_ci[["mvpf_95ci_lower"]]
    mvpf_results$mvpf_95ci_upper[i] <- mvpf_ci[["mvpf_95ci_upper"]]
  }
  return(mvpf_results)
}

# Takes bootstrapped willingness_to_pay & government_net_costs vector as inputs and calculates the MVPF confidence interval.
calculateMVPFCI <- function(willingness_to_pay_pe,
                            willingness_to_pay_boostrap,
                            government_net_costs_pe,
                            government_net_costs_bootstrap) {
  # When calculating the MVPF CI the following has to be taken into account:
  # The interpretation of the MVPF changes when the willingsness to pay and the government net costs are both negative.
  # Although the sign of the MVPF is also positive. Example: Consider MVPF = 0.5
  # This means that either
  # (1) Willingness to pay and goverment net costs are positive. In this case one dollar spent by the government is valued
  # with 0.5 dollar by the beneficiaries of the reform (higher value better)
  # (2) Willingness to pay and goverment net costs are negative. In this case the MVPF measures how much WTP is lost
  # per tax revenue increase. (lower value better)
  # If the point estimate is either (1), and one of the bootstrapped estimates is (2) (or vice versa)
  # the bootstrapped estimate is out of the comparable range and there is uncertainty about the sign of the effect of
  # the policy.
  # When running the bootstrap, this possibility has to be acconted for by removing the replications where the MVPF is
  # out of the comparable range, and adjusting the confidence intervall.

  replication_defined <- rep (TRUE, bootstrap_replications)

  for (j in 1:bootstrap_replications) {
    if (all(c(willingness_to_pay_pe, government_net_costs_pe) >= 0) &
      all(c(willingness_to_pay_boostrap[j], government_net_costs_bootstrap[j]) < 0)) {
      replication_defined[j] <- FALSE
    }
    else if (all(c(willingness_to_pay_pe, government_net_costs_pe) < 0) &
      all(c(willingness_to_pay_boostrap[j], government_net_costs_bootstrap[j]) > 0)) {
      replication_defined[j] <- FALSE
    }
  }

  # Adjust the confindence intervall to account for the fact that the non-defined estimates have been removed.
  # Note that the 95% confidence intervall can reach into the non-defined region of the mvpf. In this case,
  # the confidence intervall spans -Inf to +Inf.
  upper_percentile_95ci <- 1 - (0.05 - sum(!replication_defined) / bootstrap_replications) / 2
  lower_percentile_95ci <- (0.05 - sum(!replication_defined) / bootstrap_replications) / 2

  if (lower_percentile_95ci  <= 0 & upper_percentile_95ci >= 1) {
    return(list(mvpf_95ci_lower = -Inf, mvpf_95ci_upper = Inf))
  }
  else {
    bootstrapped_mvpf <- calculateMVPF(willingness_to_pay_boostrap, government_net_costs_bootstrap)
    return(list(mvpf_95ci_lower = quantile(bootstrapped_mvpf[replication_defined], lower_percentile_95ci),
                mvpf_95ci_upper = quantile(bootstrapped_mvpf[replication_defined], upper_percentile_95ci)))
  }
}

# Converts a correlation matrix to a covarinace matrix
correlationToCovarianceMatrix <- function(correlation_matrix, standard_error_vector) {
  return((standard_error_vector %*% t(standard_error_vector)) * correlation_matrix)
}

#----------------------------------------------------------------------------------------------------------------------#
# Plotting Related Functions
#----------------------------------------------------------------------------------------------------------------------#

# This function is used for all the plotting.
plotResults <- function(y_axis = "mvpf", y_label = "MVPF", x_axis = "year", x_label = "Year",
                        plot_data, category_plot_data, save = "", lower_cutoff = -1, upper_cutoff = 6, confidence_intervalls = FALSE,
                        text_labels = FALSE, legend_label = "Category", vertical_x_axis_labels = FALSE, landscape = FALSE, smaller_scale = FALSE) {

  # Define Colors. These are the same as in the web application:
  colors <- c(rgb(46,139,87, maxColorValue = 255),
              rgb(30,144,255, maxColorValue = 255),
              rgb(255,165,0, maxColorValue = 255),
              rgb(220,20,60, maxColorValue = 255),
              rgb(0,128,128, maxColorValue = 255),
              rgb(0,0,139, maxColorValue = 255),
              rgb(255,20,147, maxColorValue = 255),
              rgb(165,42,42, maxColorValue = 255),
              rgb(154,205,50, maxColorValue = 255),
              rgb(196, 196, 126, maxColorValue = 255),
              rgb(189,189,189, maxColorValue = 255))

  # The text labels may be stochastic. To make them deterministic:
  set.seed(1)


  # Check if y_axis and x_axis actually exist in the plot_data
  if (!all(c(y_axis, x_axis) %in% colnames(plot_data))) {
    warning("Either x or y axis variable is not in the dataset")
    return(-1)
  }

  # Check if confidence intervalls exist for y_axis variable:
  if (!all(c(paste0(y_axis,"_95ci_lower"), paste0(y_axis,"_95ci_upper")) %in% colnames(plot_data))) {
    confidence_intervalls <- FALSE
  }

  # Assign the Program program identifier (folder name) as program name, if none is specified:
  if (y_axis != "grouped_mvpf") {
    plot_data$program_name <- coalesce(plot_data$program_name, plot_data$program)
  }

  # Order the plot_data so that reforms of the same category are displayed next to each other in the overview table
  plot_data <- plot_data %>% arrange(category)

  # Generate missing program costs:
  plot_data <- impute_missing_program_costs(plot_data)

  # Convert x_axis variable to factor. Otherwise the programs are plotted in alphabetical order.
  if (x_axis == "program_name") {
    plot_data[, x_axis] <- factor( plot_data[, x_axis], levels = plot_data[, x_axis])
  }
  else if (x_axis == "cost_benefit_ratio") {
    # We need to censor the data otherwise we dont see anything. This removes 3 programs.
    plot_data[,"cost_benefit_ratio"] <- ifelse(plot_data[,"cost_benefit_ratio"] > 6, 6, plot_data[,"cost_benefit_ratio"])
    plot_data[,"cost_benefit_ratio"] <- ifelse(plot_data[,"cost_benefit_ratio"] < -1, -1, plot_data[,"cost_benefit_ratio"])
    # And cost benefit ratio does not make sense for programs with negative costs:
    plot_data <- plot_data %>% filter(program_cost > 0)
  }

  # Censor all values to match the specified cut-offs. Handle the extra infinity level for the MVPF
  if (y_axis == "mvpf" | y_axis == "grouped_mvpf") {
    # Censor all values that are larger than the the infinity_cutoff and use infinity_cutoff + 1 as 'infinity'
    plot_data <- plot_data %>%
      mutate(!!y_axis := replace(get(y_axis), get(y_axis) > upper_cutoff & get(y_axis) != Inf, upper_cutoff)) %>%
      mutate(!!y_axis := replace(get(y_axis), get(y_axis) == Inf, upper_cutoff + 1))

    if (confidence_intervalls) {
      plot_data <- plot_data %>%
        mutate(!!paste0(y_axis, "_95ci_upper") := replace(get(paste0(y_axis, "_95ci_upper")), get(paste0(y_axis, "_95ci_upper")) > upper_cutoff & get(paste0(y_axis, "_95ci_upper")) != Inf, upper_cutoff)) %>%
        mutate(!!paste0(y_axis, "_95ci_upper") := replace(get(paste0(y_axis, "_95ci_upper")), get(paste0(y_axis, "_95ci_upper")) == Inf, upper_cutoff + 1)) %>%
        mutate(!!paste0(y_axis, "_95ci_lower") := replace(get(paste0(y_axis, "_95ci_lower")), get(paste0(y_axis, "_95ci_lower")) > upper_cutoff & get(paste0(y_axis, "_95ci_lower")) != Inf, upper_cutoff)) %>%
        mutate(!!paste0(y_axis, "_95ci_lower") := replace(get(paste0(y_axis, "_95ci_lower")), get(paste0(y_axis, "_95ci_lower")) == Inf, upper_cutoff + 1))
    }
    if(!missing(category_plot_data)) {
      category_plot_data <- category_plot_data %>%
        mutate(grouped_mvpf = replace(grouped_mvpf, grouped_mvpf > upper_cutoff & grouped_mvpf != Inf, upper_cutoff)) %>%
        mutate(grouped_mvpf = replace(grouped_mvpf, grouped_mvpf == Inf, upper_cutoff + 1)) %>%
        mutate(grouped_mvpf_95ci_upper = replace(grouped_mvpf_95ci_upper, grouped_mvpf_95ci_upper > upper_cutoff & grouped_mvpf_95ci_upper != Inf, upper_cutoff)) %>%
        mutate(grouped_mvpf_95ci_upper = replace(grouped_mvpf_95ci_upper, grouped_mvpf_95ci_upper == Inf, upper_cutoff + 1)) %>%
        mutate(grouped_mvpf_95ci_lower = replace(grouped_mvpf_95ci_lower, grouped_mvpf_95ci_lower > upper_cutoff & grouped_mvpf_95ci_lower != Inf, upper_cutoff)) %>%
        mutate(grouped_mvpf_95ci_lower = replace(grouped_mvpf_95ci_lower, grouped_mvpf_95ci_lower == Inf, upper_cutoff + 1))
    }
  }
  else if (!is.na(upper_cutoff)) {
    plot_data <- plot_data %>%
      mutate(!!y_axis := replace(get(y_axis), get(y_axis) > upper_cutoff, upper_cutoff))
    if (confidence_intervalls) {
      plot_data <-  plot_data %>%
        mutate(!!paste0(y_axis, "_95ci_upper") := replace(get(paste0(y_axis, "_95ci_upper")), get(paste0(y_axis, "_95ci_upper")) > upper_cutoff, upper_cutoff)) %>%
        mutate(!!paste0(y_axis, "_95ci_lower") := replace(get(paste0(y_axis, "_95ci_lower")), get(paste0(y_axis, "_95ci_lower")) > upper_cutoff, upper_cutoff))
    }
    if(!missing(category_plot_data) & confidence_intervalls) {
      category_plot_data <- category_plot_data %>%
        mutate(grouped_mvpf_95ci_upper = replace(grouped_mvpf_95ci_upper, grouped_mvpf_95ci_upper > upper_cutoff, upper_cutoff)) %>%
        mutate(grouped_mvpf_95ci_lower = replace(grouped_mvpf_95ci_lower,grouped_mvpf_95ci_lower > upper_cutoff, upper_cutoff))
    }

  }

  if (!is.na(lower_cutoff)) {
    plot_data <-  plot_data %>%
      mutate(!!y_axis := replace(get(y_axis), get(y_axis) < lower_cutoff, lower_cutoff))
    if (confidence_intervalls) {
      plot_data <-  plot_data %>%
        mutate(!!paste0(y_axis, "_95ci_upper") := replace(get(paste0(y_axis, "_95ci_upper")), get(paste0(y_axis, "_95ci_upper")) < lower_cutoff, lower_cutoff)) %>%
        mutate(!!paste0(y_axis, "_95ci_lower") := replace(get(paste0(y_axis, "_95ci_lower")), get(paste0(y_axis, "_95ci_lower")) < lower_cutoff, lower_cutoff))
    }
    if(!missing(category_plot_data) & confidence_intervalls) {
      category_plot_data <- category_plot_data %>%
        mutate(grouped_mvpf_95ci_upper = replace(grouped_mvpf_95ci_upper, grouped_mvpf_95ci_upper < lower_cutoff, lower_cutoff)) %>%
        mutate(grouped_mvpf_95ci_lower = replace(grouped_mvpf_95ci_lower,grouped_mvpf_95ci_lower < lower_cutoff, lower_cutoff))
    }
  }

  # Censoring Done

  # Start to construct the plot. There is some common structure, but also quite a few special cases that need to be
  # considered
  plot <- ggplot(aes_string(y = y_axis, x= x_axis), data = plot_data) +
    ylab(y_label) +
    xlab(x_label) +
    labs(color = legend_label)

  if (x_axis == "cost_benefit_ratio") {
    plot <- plot + geom_hline(yintercept = 1, linetype = "longdash", alpha = 1) +
      geom_vline(xintercept = 1, linetype = "longdash", alpha = 1) +
      scale_x_continuous(breaks = -1:6, labels = c(paste("\u2264", -1),
                                                   as.character(0:5),
                                                   paste("\u2265", 6))) +
      coord_fixed() + coord_flip()
  }

  if (text_labels) {
    plot <- plot + geom_text_repel(aes(label = program_name),
                                   size = 2,
                                   direction = "both",
                                   box.padding = 0.4,
                                   nudge_x = 0.1,
                                   min.segment.length = unit(0.1, 'lines'),
                                   max.iter = 5000,
                                   segment.size = 0.3,
                                   segment.color = "grey",
                                   color = "black")
  }

  plot <- plot +
    geom_point(aes_string(y = y_axis,
                          x = x_axis,
                          color = "category"),
               size = 2.75,
               alpha = ifelse(missing(category_plot_data), 1, 0.4)) +
    scale_color_manual(values = colors,
                       breaks = unique(plot_data$category)[!unique(plot_data$category) %in% "Other"])

  if(smaller_scale) {
    plot <- plot + theme_modified_minimal_small()
  }
  else {
    plot <- plot + theme_modified_minimal()
  }

  if (confidence_intervalls) {
    if (missing(category_plot_data)) {
      plot <- plot + geom_errorbar(aes_string(ymin = paste0(y_axis, "_95ci_lower"),
                                              ymax = paste0(y_axis, "_95ci_upper"),
                                              color = "category"),
                                              show.legend=FALSE,
                                              size = 0.7,
                                              width = 0.3)
    }
    else {
      plot <- plot + geom_errorbar(aes_string(ymin = "grouped_mvpf_95ci_lower",
                                              ymax = "grouped_mvpf_95ci_upper",
                                              y = "grouped_mvpf",
                                              x = x_axis,
                                              color = "category"),
                                              data = category_plot_data,
                                              show.legend=FALSE,
                                              size = 0.9,
                                              width = ifelse(x_axis == "average_age_beneficiary", 0.4, 100))
    }
  }

  if (!missing(category_plot_data)) {
    plot <- plot + geom_point(aes(y = grouped_mvpf, color = category), data = category_plot_data, size = 4.5)
  }

  if (y_axis == "mvpf" | y_axis == "grouped_mvpf") {
    plot <- plot + scale_y_continuous(breaks = lower_cutoff:(upper_cutoff + 1),
                                      expand = expansion(mult = c(0.05, 0.05)),
                                      minor_breaks = (lower_cutoff:(upper_cutoff - 1)) + 0.5,
                                      limits = c(lower_cutoff, upper_cutoff + 1),
                                      labels = c(paste("\u2264", lower_cutoff),
                                                 as.character((lower_cutoff + 1):(upper_cutoff -1)),
                                                 paste("\u2265", upper_cutoff), "\u221E"))
  }
  else {
    plot <- plot + scale_y_continuous(breaks = lower_cutoff:upper_cutoff,
                                      limits = c(lower_cutoff, upper_cutoff),
                                      labels = c(paste("\u2264", lower_cutoff),
                                                 as.character((lower_cutoff + 1):(upper_cutoff -1)),
                                                 paste("\u2265", upper_cutoff)))
  }

  if (vertical_x_axis_labels) {
    plot <- plot + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
                         axis.title.x = element_blank())
  }

  print(plot)

  if (landscape == FALSE) {
    if (save != "") {
      ggsave(plot, filename = save, device = pdf, path = "./plots/", width = 10, height = 7.5)
    }
  }
  else {
    if (save != "") {
      ggsave(plot, filename = save, device = pdf, path = "./plots/", width = 15, height = 11)
    }
  }

  return(plot)
}

# Adds additional columns to the results data.frame. In particular, things that we need for plotting like
# the average age, earnings and category mappings.
getPlotData <- function(mvpf_results) {
  # This function adds further information about the program to the mvpf_results returned from getPointEstimates and
  # addBootstrappedConfidenceIntervalls

  if (!exists("program_information")) {
    # Load additional information about each policy from the excel file:
    program_information <<- as.data.frame(read_xlsx("programs.xlsx"))
  }

  # Calculate additional variables. CIs can only be calculated if the results already contains CIs
  mvpf_results$government_net_costs_per_program_cost <- mvpf_results$government_net_costs / mvpf_results$program_cost
  if ("government_net_costs_95ci_lower" %in% colnames(mvpf_results)) {
    mvpf_results$government_net_costs_per_program_cost_95ci_lower <- mvpf_results$government_net_costs_95ci_lower / mvpf_results$program_cost
    mvpf_results$government_net_costs_per_program_cost_95ci_upper <- mvpf_results$government_net_costs_95ci_upper / mvpf_results$program_cost
  }

  mvpf_results$willingness_to_pay_per_program_cost <- mvpf_results$willingness_to_pay / mvpf_results$program_cost
  if ("willingness_to_pay_95ci_lower" %in% colnames(mvpf_results)) {
    mvpf_results$willingness_to_pay_per_program_cost_95ci_lower <- mvpf_results$willingness_to_pay_95ci_lower / mvpf_results$program_cost
    mvpf_results$willingness_to_pay_per_program_cost_95ci_upper <- mvpf_results$willingness_to_pay_95ci_upper / mvpf_results$program_cost
  }

  mvpf_results$fiscal_externality_per_euro <- 1 - mvpf_results$government_net_costs / mvpf_results$program_cost
  if ("government_net_costs_95ci_lower" %in% colnames(mvpf_results)) {
    mvpf_results$fiscal_externality_per_euro_95ci_lower <-1 - mvpf_results$government_net_costs_95ci_lower / mvpf_results$program_cost
    mvpf_results$fiscal_externality_per_euro_95ci_upper <-1 - mvpf_results$government_net_costs_95ci_upper / mvpf_results$program_cost
  }

  mvpf_results$cost_benefit_ratio <- (mvpf_results$willingness_to_pay + mvpf_results$fiscal_externality_per_euro) / ((1 + cost_of_raising_public_funds) * mvpf_results$program_cost)

  # Join with additional information
  joined_dataset <- left_join(mvpf_results, program_information, by = c("program" = "program_identifier"))

  # The average_earnings_beneficiary column needs to be deflated:
  joined_dataset <- joined_dataset %>% mutate(average_earnings_beneficiary = Vectorize(deflate)(from = prices_year, to = results_prices) * average_earnings_beneficiary)

  # Remove excluded programs
  joined_dataset <- joined_dataset %>% filter(!(program %in% excluded_from_all_plots))

  # Generate Category 'Other' which contains all programs that have no Category specified:
  joined_dataset$category <- coalesce(joined_dataset$category, "Other")
  # Convert the category column into a factor. This is convenient for plotting
  joined_dataset$category <- factor(joined_dataset$category, levels = order_of_categories)

  return(joined_dataset)
}

# Calculates average mvpf (with CI) for each category specified in programs.xlsx and returns a dataframe that can be used
# to plot the result. Requires plot_data returned by getPlotData as input and bootstrap results
getCategoryPlotData <- function(plot_data, bootstrap_results, include_additional_programs = TRUE) {

  categories <- unique(plot_data$category)
  category_plot_data <- foreach(i = 1:length(categories), .combine = rbind) %do% {
    # Calculating the category average MVPF is not trivial. Because the MVPF can be infinite we cannot just take the average.
    # Instead we imagine a policy that spends one euro on each policy of a category. Where spending means the programmatic spending.
    # There are two problems:
    programs <- plot_data %>% filter(category == categories[i]) %>% pull(program)

    if (include_additional_programs == TRUE) {
      for (j in 1:length(programs)) {

        current_program_name <- programs[j]
        current_program_row <- plot_data %>% filter(program == current_program_name)
        # 1) Some Programs have no programmatic spending. I.e. it costs nothing to implement G8 (Although there are externalities)
        # In this case we cannot use the trick above of looking at a one euro increase in programmatic spending. For reforms
        # with MPVF < infinity we can assume that the program cost is equal to the externalities. If the MVPF is infinity,
        # we cannot spend more on a policy as it costs nothing. These retain their missing value.
        if (is.na(current_program_row$program_cost)) {
          if (current_program_row$government_net_costs > 0 |
            (current_program_row$government_net_costs <= 0 & current_program_row$willingness_to_pay < 0)) {
            # MVPF cannot be infinite
            # Set Program Cost equal to the net cost
            plot_data[plot_data$program == current_program_name, "program_cost"] <- current_program_row$government_net_costs
            # Fiscal Externality equal to zero
            plot_data[plot_data$program == current_program_name, "fiscal_externality_per_euro"] <- 0
            # And the willingness to pay per program cost is equal to the finite MVPF
            plot_data[plot_data$program == current_program_name, "willingness_to_pay_per_program_cost"] <-
              current_program_row$willingness_to_pay / current_program_row$government_net_costs

            # Add this program cost to the bootstrap results
            bootstrap_results[[current_program_name]]$program_cost <- bootstrap_results[[current_program_name]]$government_net_costs
          }
        }

        # Update Program row
        current_program_row <- plot_data %>% filter(program == current_program_name)
        # 2) Programs with negative WTP and Cost generate money. To fix this we need to multiply wtp
        # by -1. Now we get the mvpf of not conducting the reform, i.e. spending money on not doing the reform.
        # fiscal_externality_per_euro & willingness_to_pay_per_program_cost have the correct sign.
        # But we need to flip the sign of program_cost and government_net_costs:
        if (current_program_row$willingness_to_pay < 0 & current_program_row$program_cost < 0) {
          plot_data[plot_data$program == current_program_name, "willingness_to_pay"] <- -current_program_row$willingness_to_pay
          plot_data[plot_data$program == current_program_name, "program_cost"] <- -current_program_row$program_cost
          plot_data[plot_data$program == current_program_name, "government_net_costs"] <- -current_program_row$government_net_costs

          # We also have to do this for the bootstrap
          bootstrap_results[[current_program_name]]$program_cost <- (-1) *bootstrap_results[[current_program_name]]$program_cost
          bootstrap_results[[current_program_name]]$willingness_to_pay <- (-1) * bootstrap_results[[current_program_name]]$willingness_to_pay
          bootstrap_results[[current_program_name]]$government_net_costs <- (-1) * bootstrap_results[[current_program_name]]$government_net_costs
        }
      }
    }

    # Programs that still have a negative or no program cost need to be removed. There is only one program for which
    # this currently applies.
    programs_in_category <- plot_data %>% filter(category == categories[i], !(is.na(program_cost) | program_cost <= 0 | program %in% excluded_from_category_average))

    # If some category only consists of reforms with negative costs, skip it.
    if (nrow(programs_in_category) == 0) {
      # All programs in the category have zero cost or cost is NA.
      # Skip this category
      return(NULL)
    }

    # This is a implementation of Hendren & Sprung-Keyser (2020) Equation 8.
    # There is something weird going on with the sign of fiscal externalities in Hendren & Sprung-Keyser (2020).
    # In equation 4 fiscal externalities which increase government revenue have a positive sign.
    # In equation 8 these FEs must have a negative sign. Not sure if this is intended or a oversight.
    # Here, externalities are positive. I.e. we need to substract the FE in the denominator.
    numerator <- (1 / nrow(programs_in_category)) * sum(programs_in_category$willingness_to_pay_per_program_cost)
    denominator <- (1 / nrow(programs_in_category)) * sum(1 - programs_in_category$fiscal_externality_per_euro)
    grouped_mvpf <- calculateMVPF(numerator, denominator)

    # For the confidence intervall we need to calculate the grouped_mvpf for each bootstrap replication. These are stored in
    # bootstrapped_estimates
    # Get willingness_to_pay_per_program_cost from each bootstrap repilcation (rows) for each program beloning to the category (columns)
    willingness_to_pay_per_euro <- sapply(bootstrap_results[programs_in_category$program], function(bootstrap_results) {
      return(bootstrap_results$willingness_to_pay / bootstrap_results$program_cost)
    })

    # Get fiscal_externality_per_euro from each bootstrap repilcation (rows) for each program beloning to the category (columns)
    fiscal_externality_per_euro <- sapply(bootstrap_results[programs_in_category$program], function(bootstrap_results) {
      return(1 - bootstrap_results$government_net_costs / bootstrap_results$program_cost)
    })

    # Calculate numerator and denominator as before except that the result now is a vector with 'number of bootstrap replications' rows:
    numerator_bootstrap <- 1 / ncol(willingness_to_pay_per_euro) * rowSums(willingness_to_pay_per_euro)
    denominator_bootstrap <- 1 / ncol(willingness_to_pay_per_euro) * (-rowSums(fiscal_externality_per_euro) + ncol(fiscal_externality_per_euro))

    mvpf_ci_grouped <- calculateMVPFCI(willingness_to_pay_pe = numerator,
                                       willingness_to_pay_boostrap = numerator_bootstrap,
                                       government_net_costs_pe = denominator,
                                       government_net_costs_bootstrap = denominator_bootstrap)


    return(data.frame(grouped_mvpf = grouped_mvpf,
                      category = categories[i],
                      grouped_mvpf_95ci_upper = mvpf_ci_grouped[["mvpf_95ci_upper"]],
                      grouped_mvpf_95ci_lower = mvpf_ci_grouped[["mvpf_95ci_lower"]],
                      average_age_beneficiary = mean(programs_in_category$average_age_beneficiary, na.rm = TRUE),
                      year = mean(programs_in_category$year, na.rm = TRUE),
                      average_earnings_beneficiary = mean(programs_in_category$average_earnings_beneficiary, na.rm = TRUE)))
  }
  # There are some unnecessary row names that do not make sense -> remove those
  rownames(category_plot_data) <- NULL
  return(category_plot_data)
}

# Calculates the category average MVPF
calculateAverageMVPF <- function(plot_data) {
  # This is not a true average. The average is infinity once one program has an infinite MVPF.
  # This is the MVPF of spending one euro on every policy
  plot_data <- plot_data %>% filter(!(is.na(program_cost) | program_cost <= 0))
  numerator <- (1 / nrow(plot_data)) * sum(plot_data$willingness_to_pay)
  denominator <- (1 / nrow(plot_data)) * sum(1 - plot_data$fiscal_externality_per_euro)
  return(calculateMVPF(numerator, denominator))
}

# Imputes the program cost for programms which have no direct program cost. We assume that the program cost is equal
# to the fiscal externality
impute_missing_program_costs <- function(plot_data) {
  # This replaces the program cost values and also calculates the derived program costs
  plot_data$program_cost <- coalesce(plot_data$program_cost,
                                     plot_data$government_net_costs)
  plot_data$willingness_to_pay_per_program_cost <- coalesce(plot_data$willingness_to_pay_per_program_cost,
                                                            plot_data$willingness_to_pay / plot_data$program_cost)
  plot_data$government_net_costs_per_program_cost <- coalesce(plot_data$government_net_costs_per_program_cost,
                                                            plot_data$government_net_costs / plot_data$program_cost)
  plot_data$fiscal_externality_per_euro <- 1 - plot_data$government_net_costs / plot_data$program_cost
  plot_data$cost_benefit_ratio <- (plot_data$willingness_to_pay_per_program_cost + plot_data$fiscal_externality_per_euro) / ((1 + cost_of_raising_public_funds))
  return(plot_data)
}

# Define custom ggplot theme:
theme_modified_minimal <- function() {
  # Set the font here. This only works if the font is installed on the system and it is available in R.
  # To make the font available use font_add("font name", "file name") and showtext_auto() from the 'showtext' package.
  theme_minimal(base_size=13, base_family=plot_font) %+replace%
    theme (
      # axis.line.x|y control whether the x and y axis are drawn. It can look stylish to omit one or both:
      # axis.line.y = element_blank() to omit
      # e.g: axis.line.x = element_line(color = "darkgrey", size = 0.5) to draw
      axis.line.y = element_line(color = "grey", size = 0.75),
      axis.line.x = element_line(color = "grey", size = 0.75),
      panel.border = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      # Enable or Disable Axis ticks:
      # axis.ticks = element_blank() to omit
      axis.ticks = element_line(color = "grey", size = 0.75),
      # Removes Legend Explanation:
      legend.title=element_blank(),
      # Set location of legend:
      legend.position = "bottom",
      # Legend font size
      legend.text = element_text(size=10.5),
      # Axis (ticks) size
      axis.text = element_text(size=11.5),
      # Axis label size
      axis.title = element_text(size=14),
      # Change Legend Stacking to vertical:
      legend.box="vertical"
    )
}

theme_modified_minimal_small <- function() {
  # Set the font here. This only works if the font is installed on the system and it is available in R.
  # To make the font available use font_add("font name", "file name") and showtext_auto() from the 'showtext' package.
  theme_minimal(base_size=12, base_family=plot_font) %+replace%
    theme (
    # axis.line.x|y control whether the x and y axis are drawn. It can look stylish to omit one or both:
    # axis.line.y = element_blank() to omit
    # e.g: axis.line.x = element_line(color = "darkgrey", size = 0.5) to draw
    axis.line.y = element_line(color = "grey", size = 0.5),
    axis.line.x = element_line(color = "grey", size = 0.5),
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # Enable or Disable Axis ticks:
    # axis.ticks = element_blank() to omit
    axis.ticks = element_line(color = "grey", size = 0.5),
    # Removes Legend Explanation:
    legend.title=element_blank(),
    # Set location of legend:
    legend.position = "bottom",
    # Axis label size
    axis.title = element_text(size=10))
}

#----------------------------------------------------------------------------------------------------------------------#
# Earnings Projections
#----------------------------------------------------------------------------------------------------------------------#

# This is the earnings projection that is used for all education policies
project_lifetime_impact <- function(impact_age, # the age at which the effect on income comes up for the first time
                                    impact_magnitude, # the effect of treatment on income relative to the control group, i.e. effect / income control
                                    impact_magnitude_matrix, # alllows to specify different impact magnitude for each age, see begin of function
                                    relative_control_income, # the income of the control group relative to the average, i.e. control income / average income
                                    control_income, # the income of the control group per year. Either this or relative_control_income has to be set
                                    start_projection_age, # the age at which the projection starts (optional)
                                    end_projection_age = retirement_age, # the age at which the projection ends (optional)
                                    start_projection_year, # the year at which the projection starts
                                    prices_year, # the year whose prices are used (optional)
                                    discount_to, # the year to which the impacts should be discounted to (optional)
                                    inculde_welfare_benefits_fraction = 1 # The fraction of welfare benefits the beneficiaries receive
) {



  # prices_year and start_projection age are optional:
  if (missing(start_projection_age)) {
    start_projection_age = impact_age
  }
  if (missing(prices_year)) {
    prices_year = start_projection_year
  }

  # Import age-income relation: This is calculated from the IAB Data. see generateIncomeCrossSection.R in the income_projection folder
  if (!exists("age_income_table")) {
    age_income_table <<- read.csv("./income_projection/age_income_cross_section.csv")
  }
  # Handlde impact magnitude:

  # There are two options to specifiy the impact magnitude.
  # 1) impact_magnitude -> constant impact of policy at all projection years
  # 2) impact_magnitude_matrix -> a matrix / data.frame that contains a "age" and "impact_magnitude" column which specifies the impact
  # for a given age. This matrix need not cover all ages the projection covers. Missing ages are replaced by the closest, we have
  # information for.

  if (missing(impact_magnitude) & missing(impact_magnitude_matrix)) {
    warning("Need to specify either impact_magnitude or impact_magnitude_matrix")
    return(-1)
  }
  if (!missing(impact_magnitude)) {
    # constant policy impact at all ages
    age_income_table$impact_magnitude <- impact_magnitude
  }
  else {
    # Select relevant ages from impact_magnitude_matrix
    impact_magnitude_matrix <- impact_magnitude_matrix %>% filter(age >= start_projection_age & age <= end_projection_age)

    # Add age_specific impact_magnitude to age_income_table by iterating over all ages
    for (i in 1:nrow(age_income_table)) {
      if (age_income_table[i , "age"] %in% impact_magnitude_matrix$age) {
        # Impact for the age that is currently being iterated over is available:
        age_income_table[i, "impact_magnitude"] <-
          impact_magnitude_matrix[age_income_table[i , "age"] == impact_magnitude_matrix$age, "impact_magnitude"]
      }
      else {
        # Impact for current age is not available. Choose closest available impact:
        closest_available_impact <- impact_magnitude_matrix$impact_magnitude[
          which(abs(impact_magnitude_matrix$age - age_income_table[i , "age"]) ==
                  min(abs(impact_magnitude_matrix$age - age_income_table[i , "age"])))]

        # In case there are two closest ages, use average:
        if (length(closest_available_impact) > 1) {
          age_income_table[i, "impact_magnitude"] <- (closest_available_impact[1] + closest_available_impact[2]) / 2
        }
        else {
          age_income_table[i, "impact_magnitude"] <- closest_available_impact
        }
      }
    }
  }



  # Calculate the impact year as birth year + impact_age
  impact_year <- (start_projection_year- start_projection_age) + impact_age

  # Adjust for inflation, i.e., convert incomes to 'prices_year' euros.
  age_income_table$income_price_adjusted <- age_income_table$income * deflate(from = 2010, to = prices_year)


  # Calculate the average income trajectory(i.e. average income for each age) for the population that is 'impact_age' years old in the year 'impact_year'
  # We have to take into accout that incomes are observed for only one year, incomes that are realized after (before) this
  # year grow (fall) by the growth rate. From the birth year (start_projection_year - start_projection_age) it is possible to infer the year when they are x years old.
  age_income_table$year <- (start_projection_year - start_projection_age) + age_income_table$age

  # Grow wages by (1+g)^(the number of years the income accrues after the year we have data for) / Or shrink if the exponent is negative
  age_income_table$income_fully_adjusted <- age_income_table$income_price_adjusted * (1 + wage_growth_rate)^(age_income_table$year - 2010)

  # If the income of the control group rather than the relative income is given, we need to calculate the relative
  # income.
  if (missing(relative_control_income)) {
    relative_control_income <- control_income / age_income_table[age_income_table$age == impact_age, "income_fully_adjusted"]
  }

  # Limit the dataset the relevant region for the projection:
  age_income_table <- age_income_table %>% filter(age >= start_projection_age & age <= end_projection_age)

  age_income_table$earnings_impact <- age_income_table$income_fully_adjusted * relative_control_income * age_income_table$impact_magnitude

  # The earnings impact is sufficient to calculate the net-present value of the reform in terms of gross income.
  # However, individuals care about net income (part of numerator of MVPF).
  # The government cares about tax revenue (part of denominator of MVPF)
  # -> We need to estimate how the considered reform affects tax payments.
  # The fact that the German tax system is non-linear causes two problems:
  # 1) Changes in earnings are not enough to calculate the tax, we need the level of earnings
  # 2) Tax payments depend not only on the average income, but also on the distribution
  #
  # 1) can be fixed by calculating the level of earnings with and without the reform
  # 2) cannot be easily solved as we would need some assumption about the control group's distribution of income

  age_income_table$earnings_no_reform <- age_income_table$income_fully_adjusted * relative_control_income
  age_income_table$earnings_reform <- age_income_table$earnings_no_reform + age_income_table$earnings_impact

  age_income_table$tax_payment_no_reform <- sapply(age_income_table$earnings_no_reform,
                                                   getTaxPayment,
                                                   prices_year = prices_year,
                                                   inculde_welfare_benefits_fraction = inculde_welfare_benefits_fraction)
  age_income_table$tax_payment_reform <- sapply(age_income_table$earnings_reform,
                                                getTaxPayment,
                                                prices_year = prices_year,
                                                inculde_welfare_benefits_fraction = inculde_welfare_benefits_fraction)

  age_income_table$net_earnings_no_reform <- age_income_table$earnings_no_reform - age_income_table$tax_payment_no_reform
  age_income_table$net_earings_reform <- age_income_table$earnings_reform - age_income_table$tax_payment_reform

  age_income_table$net_earnings_impact <- age_income_table$net_earings_reform - age_income_table$net_earnings_no_reform
  age_income_table$tax_payment_impact <- age_income_table$tax_payment_reform - age_income_table$tax_payment_no_reform

  # Discount all earning impacts to the start of the projection
  age_income_table$earnings_impact_discounted <-
    age_income_table$earnings_impact * discountVector(end_projection_age - start_projection_age + 1)

  # Discount effect on net incomes:
  age_income_table$net_earnings_impact_discounted <-
    age_income_table$net_earnings_impact * discountVector(end_projection_age - start_projection_age + 1)

  # Discount effect on tax_payments:
  age_income_table$tax_payment_impact_discounted <-
    age_income_table$tax_payment_impact * discountVector(end_projection_age - start_projection_age + 1)


  # Sum discounted impact on earnings, net earnings and tax payments
  present_value_earnings_impact <- sum(age_income_table$earnings_impact_discounted)
  present_value_tax_payment_impact <- sum(age_income_table$tax_payment_impact_discounted)
  present_value_net_earnings_impact <- sum(age_income_table$net_earnings_impact_discounted)

  # Discount to specified year if discount_to is set.
  if (!missing(discount_to)) {
    present_value_earnings_impact <- discount(from = start_projection_year, to = discount_to) *
      present_value_earnings_impact
    present_value_tax_payment_impact <- discount(from = start_projection_year, to = discount_to) *
      present_value_tax_payment_impact
    present_value_net_earnings_impact <- discount(from = start_projection_year, to = discount_to) *
      present_value_net_earnings_impact
  }


  return(data.frame(present_value_earnings_impact = present_value_earnings_impact,
                    present_value_tax_payment_impact = present_value_tax_payment_impact,
                    present_value_net_earnings_impact = present_value_net_earnings_impact))
}


# This function returns the dataframe as project_lifetime_impact but is simpler and requires less assumptions.
# It is intened for reforms for reforms whose beneficiaries vary greatly.
# Also this method does not assume wage growth.
project_medium_run_impact <- function(impact_magnitude, # can be either scalar or a vector containing the effect for each period
                                      absolute_impact_magnitude, #can be either scalar or a vector containing the effect for each period (change in yearly earnings)
                                      yearly_control_income,
                                      number_of_periods,
                                      prices_year,
                                      share_affected = 1, # with labor market policies it can happen that only employed are affected and the share of employed increases over time
                                      inculde_welfare_benefits_fraction = 1) {

  if (missing(yearly_control_income)) {
    yearly_control_income = deflate(from = 2010, to = prices_year) * 30415.17 # average earnings across all ages from the age income cross section
  }

  if (missing(impact_magnitude)) {
    impact_magnitude <- absolute_impact_magnitude / yearly_control_income
  }

  if(length(impact_magnitude) > 1 & length(impact_magnitude) != number_of_periods) {
    # Length of the impact magnitude vector does not match the number of periods of the projection.
    # In this case it is assumed that impact in the last period persists:
    if (length(impact_magnitude) < number_of_periods) {
      impact_magnitude <- c(impact_magnitude, rep(impact_magnitude[length(impact_magnitude)], number_of_periods - length(impact_magnitude)))
    }
    else {
      # If the impact_magnitude vector is too long, the irrelevant periods will be ignored
      impact_magnitude <- impact_magnitude[1:number_of_periods]
    }
  }


  gross_earnings_no_reform <- rep(yearly_control_income, number_of_periods)
  gross_earnings_reform <- rep(yearly_control_income, number_of_periods) *  (1 + impact_magnitude)

  tax_payment_no_reform <- sapply(gross_earnings_no_reform,
                                  getTaxPayment,
                                  prices_year = prices_year,
                                  inculde_welfare_benefits_fraction = inculde_welfare_benefits_fraction)

  tax_payment_reform <- sapply(gross_earnings_reform,
                               getTaxPayment,
                               prices_year = prices_year,
                               inculde_welfare_benefits_fraction = inculde_welfare_benefits_fraction)

  net_earnings_no_reform <- gross_earnings_no_reform - tax_payment_no_reform
  net_earnings_refrom <- gross_earnings_reform - tax_payment_reform

  earnings_difference <- share_affected * (gross_earnings_reform - gross_earnings_no_reform)
  tax_payment_difference <- share_affected * (tax_payment_reform - tax_payment_no_reform)
  net_earnings_difference <- share_affected * (net_earnings_refrom - net_earnings_no_reform)

  present_value_earnings_impact <- sum(earnings_difference * discountVector(number_of_periods))
  present_value_tax_payment_impact <- sum(tax_payment_difference * discountVector(number_of_periods))
  present_value_net_earnings_impact <- sum(net_earnings_difference * discountVector(number_of_periods))

  return(data.frame(present_value_earnings_impact = present_value_earnings_impact,
                    present_value_tax_payment_impact = present_value_tax_payment_impact,
                    present_value_net_earnings_impact = present_value_net_earnings_impact))
}

#----------------------------------------------------------------------------------------------------------------------#
# Tax and Transfer System Simulation
#----------------------------------------------------------------------------------------------------------------------#

# Returns a dataframe which which consists of the tax due, net income, social security contributions and soli as a function of gross income
getTaxSystemEffects <- function(gross_income,
                                inculde_welfare_benefits_fraction = global_inculde_welfare_benefits_fraction,
                                income_fraction_of_pension_contribution = global_income_fraction_of_pension_contribution,
                                income_fraction_of_unemployment_insurance_contribution = global_income_fraction_of_unemployment_insurance_contribution,
                                income_fraction_of_long_term_care_contribution = global_income_fraction_of_long_term_care_contribution,
                                income_fraction_of_health_insurance_contribution = global_income_fraction_of_health_insurance_contribution) {

  # inculde_welfare_benefits_fraction is the fraction of welfare benefits (i.e. Hartz IV) that are considered net income.
  # A value < 1 represents the fact that not everyone who receives low income is entitled to Hartz IV
  # income_fraction_of_pension_contribution denotes the fraction of pension contributions that are considered net income.
  # Higher pension contributions result in higher pension payments but due to demographics the return is probably quite low.

  # adapted from:
  # 'Identifying Laffer Bounds: A Sufficient-Statistics Approach with an Application to Germany' by Lorenz and Sachs (2015) Appendix A
  # and their supplementary excel file

  # Calculations are for a single household without children

  #--------------------------------------------------------------------------------------------------------------------#
  # Assumptions
  #--------------------------------------------------------------------------------------------------------------------#

  # Contribution to pension fund (= Rentenversicherungsbeitrag)
  pension_contribution_rate_total <- 0.186 # 2020
  pension_contribution_rate_employer <- 0.093 # 2020
  pension_contribution_rate_employee <- pension_contribution_rate_total - pension_contribution_rate_employer

  # Contribution to health insurance (= Krankenversicherungsbeitrag)
  health_insurance_contribution_rate_total <- 0.146 # 2020 Does not include the insurance-specific mark-up of ~ 1 percentage point
  health_insurance_contribution_rate_employer <- 0.073 # 2020
  health_insurance_contribution_rate_employee <- health_insurance_contribution_rate_total - health_insurance_contribution_rate_employer

  # Contribution to long term care fund (= Pflegeversicherung)
  long_term_care_contribution_rate_total <- 0.0305 # 2020
  long_term_care_contribution_rate_employer <- 0.01525 # 2020
  long_term_care_contribution_rate_employee <- long_term_care_contribution_rate_total -  long_term_care_contribution_rate_employer

  # Contribution to unemployment insurance (= Arbeitslosenversicherung)
  unemployment_insurance_contribution_rate_total <- 0.024 # 2020
  unemployment_insurance_contribution_rate_employer <- 0.012 # 2020
  unemployment_insurance_contribution_rate_employee <- unemployment_insurance_contribution_rate_total -  unemployment_insurance_contribution_rate_employer

  # Earnings ceiling: For (monthly) incomes above this threshold the marginal contribution to the respective social insurance is zero (=Beitragsbemessungsgrenze)
  pension_earnings_ceiling <- 6900 #2020
  health_insurance_earnings_ceiling <- 4687.50 #2020
  long_term_care_earnings_ceiling <- 4687.50 #2020
  unemployment_insurance_earnings_ceiling <- 6900 #2020

  # Transitional region cut-offs:
  lower_cutoff <- 450 #2020
  upper_cutoff <- 1300 #2020

  # Deductibles:
  standard_deduction_employee <- 1000 # Arbeitnehmerpauschbetrag
  extraordinary_deduction_employee <- 36 # Sonderausgabenpauschbetrag
  correction_factor <- 0.8 # Correction factor that is applied on the pension fund contribution deduction. Increases by 0.04 every year until 1 is reached in 2025

  # Welfare Benefits:
  welfare_benefit_monthly <- global_welfare_benefit_monthly # The amount of welfare if the personal income is 0 (= average Wohngeld + Hartz IV Regelsatz?)
  personal_exemption <- 100 # Grundfreibetrag

  #--------------------------------------------------------------------------------------------------------------------#
  # Calculation
  #--------------------------------------------------------------------------------------------------------------------#

  # For the calculation it is convenient to transform the yearly income to monthly income
  gross_income <- gross_income / 12


  # These cut-offs were recently changed
  if (gross_income <= lower_cutoff) {
    pension_contribution <- 0
    health_insurance_contribution <- 0
    long_term_care_contribution <- 0
    unemployment_insurance_contribution <- 0
  }
  else if (gross_income > lower_cutoff &&  gross_income < upper_cutoff) {
    # The total social insurance rate is the sum of the individiual contribution rates of:
    # 1) pension fund 2) unemployment insurance 3) health insurance 4) long-term care insurance (=Plegeversicherung)
    total_social_insurance_rate <- 0.3975

    # In this transitional region, a fictitious income is used to calculate the individual contributions
    # The forumla for this income involves a factor often called F, which is defined as:
    F <- 0.3 / total_social_insurance_rate
    # This formula was changed after 2012: Sachs and Lorenz (2015) use F * lower_cutoff + (2 - F) * (gross_income - lower_cutoff)
    fictitious_income <- F * lower_cutoff + (upper_cutoff / (upper_cutoff - lower_cutoff) - (lower_cutoff / (upper_cutoff - lower_cutoff)) * F) * (gross_income - lower_cutoff)

    # The contributions to all of the social insurance systems are given by the total contribution times the fictitious income
    # minus the full employer contribution
    pension_contribution <- pension_contribution_rate_total * fictitious_income - pension_contribution_rate_employer * gross_income
    health_insurance_contribution <- health_insurance_contribution_rate_total * fictitious_income - health_insurance_contribution_rate_employer * gross_income
    long_term_care_contribution <- long_term_care_contribution_rate_total * fictitious_income - long_term_care_contribution_rate_employer * gross_income
    unemployment_insurance_contribution <- unemployment_insurance_contribution_rate_total * fictitious_income - unemployment_insurance_contribution_rate_employer * gross_income
  }
  else {
    pension_contribution <- pension_contribution_rate_employee * min(gross_income, pension_earnings_ceiling)
    health_insurance_contribution <- health_insurance_contribution_rate_employee * min(gross_income, health_insurance_earnings_ceiling)
    long_term_care_contribution <- long_term_care_contribution_rate_employee * min(gross_income, long_term_care_earnings_ceiling)
    unemployment_insurance_contribution <- unemployment_insurance_contribution_rate_employee * min(gross_income, unemployment_insurance_earnings_ceiling)
  }

  # Sum of social security contributions
  social_security_contributions <- pension_contribution + long_term_care_contribution +
    unemployment_insurance_contribution + health_insurance_contribution

  # Deductibles:
  deductibles <- 0

  # Standard deduction (Arbeitnehmerpauschbetrag)
  deductibles <- deductibles + standard_deduction_employee / 12

  # Extraordinary expenses deduction (Sonderausgabenpauschbetrag)
  deductibles <- deductibles + extraordinary_deduction_employee  / 12

  # Deduction of social insurance contribution:
  # 1) Dedudctable share of pension fund contributions:
  deductibles <- deductibles + 0.5 * pension_contribution * correction_factor
  # 2) Health insurance and long term care contributions can be fully deducted. Until 2019 a alternative calculation method was
  # available (12% of gross income, at most 1900€). If this alternative method was favorable for the tax payer, it was used instead.
  deductibles <- deductibles + health_insurance_contribution + long_term_care_contribution

  taxable_income_monthly <- max(0, gross_income - deductibles)
  taxable_income_yearly <- 12 * taxable_income_monthly

  income_tax_yearly <- incomeTax(taxable_income_yearly)
  solidarity_charge_yearly <- solidarityCharge(taxable_income_yearly)

  income_tax_monthly <- income_tax_yearly / 12
  solidarity_charge_monthly <- solidarity_charge_yearly / 12

  # Net income, social security contributions not yet deducted
  income_minus_income_tax_yearly <- gross_income * 12 - income_tax_yearly - solidarity_charge_yearly
  income_minus_income_tax_monthly <- income_minus_income_tax_yearly / 12

  # Welfare Benefits (=Arbeitslosengeld II)
  # Everyone is entitled to a minimum income. If an individual earns an income above the minimum income, they might
  # still receive welfare benefits because their own wage income is not fully counted towards the minimum income.
  # If this were the case, the marginal rate would be 100% for all incomes below the the minimum income.

  # Everyone get's the personal excemption of currently 100€.
  excemptions <- personal_exemption
  # All social security contributions and the income tax are excempted:
  excemptions <- excemptions + social_security_contributions
  # 20 percent of the income between 100 and 1000 are excempted:
  if (gross_income >= 100) {
    excemptions <- excemptions + min(0.2*(gross_income - 100), 180)
  }
  # 10 Percent of the income between 1000 and 1200 are excempted:
  if (gross_income >= 1000) {
    excemptions <- excemptions + min(0.1*(gross_income - 1000), 20)
  }

  # The income relevant for the receipt of welfare benefits
  income_minus_excemptions <- max(gross_income - excemptions, 0)

  # The individual welfare benefit depending on personal income
  welfare_benefit <- max(welfare_benefit_monthly - income_minus_excemptions, 0)

  # Net income defined as gross income MINUS social security contributions, income tax, solidarity chage PLUS welfare benefit
  net_income_monthly <- gross_income -
    (social_security_contributions + income_tax_monthly + solidarity_charge_monthly) +
    welfare_benefit * inculde_welfare_benefits_fraction +
    pension_contribution * income_fraction_of_pension_contribution +
    unemployment_insurance_contribution * income_fraction_of_unemployment_insurance_contribution +
    health_insurance_contribution * income_fraction_of_health_insurance_contribution +
    long_term_care_contribution * income_fraction_of_long_term_care_contribution



  net_income_yearly <- net_income_monthly * 12

  # Tax defined as gross income MINUS net income
  tax_monthly <- gross_income - net_income_monthly
  tax_yearly <- tax_monthly * 12


  return_df <- data.frame(net_income_monthly = net_income_monthly,
                          net_income_yearly = net_income_yearly,
                          gross_income_monthly = gross_income,
                          gross_income_yearly = gross_income * 12,
                          tax_monthly = tax_monthly,
                          tax_yearly = tax_yearly,
                          taxable_income_monthly = taxable_income_monthly,
                          taxable_income_yearly = taxable_income_yearly,
                          income_tax_monthly = income_tax_monthly,
                          welfare_benefit = welfare_benefit,
                          solidarity_charge_monthly = solidarity_charge_monthly,
                          social_security_contributions = social_security_contributions,
                          pension_contribution = pension_contribution,
                          health_insurance_contribution = health_insurance_contribution,
                          unemployment_insurance_contribution = unemployment_insurance_contribution,
                          long_term_care_contribution = long_term_care_contribution,
                          deductibles = deductibles)

  return(return_df)
}

# Just gives the net income back as scalar.
getNetIncome <- function(gross_income,
                         inculde_welfare_benefits_fraction = global_inculde_welfare_benefits_fraction,
                         income_fraction_of_pension_contribution = global_income_fraction_of_pension_contribution,
                         income_fraction_of_unemployment_insurance_contribution = global_income_fraction_of_unemployment_insurance_contribution,
                         income_fraction_of_long_term_care_contribution = global_income_fraction_of_long_term_care_contribution,
                         income_fraction_of_health_insurance_contribution = global_income_fraction_of_health_insurance_contribution) {
  return(getTaxSystemEffects(gross_income,
                             inculde_welfare_benefits_fraction,
                             income_fraction_of_pension_contribution,
                             income_fraction_of_unemployment_insurance_contribution,
                             income_fraction_of_long_term_care_contribution,
                             income_fraction_of_health_insurance_contribution)$net_income_yearly)
}


# Calculates the average tax rate
getAverageTaxRate <- function(gross_income,
                              inculde_welfare_benefits_fraction = global_inculde_welfare_benefits_fraction,
                              income_fraction_of_pension_contribution = global_income_fraction_of_pension_contribution,
                              income_fraction_of_unemployment_insurance_contribution = global_income_fraction_of_unemployment_insurance_contribution,
                              income_fraction_of_long_term_care_contribution = global_income_fraction_of_long_term_care_contribution,
                              income_fraction_of_health_insurance_contribution = global_income_fraction_of_health_insurance_contribution,
                              income_tax_only = global_income_tax_only) {

  return(getTaxPayment(gross_income,
                       inculde_welfare_benefits_fraction = inculde_welfare_benefits_fraction,
                       income_fraction_of_pension_contribution = income_fraction_of_pension_contribution,
                       income_fraction_of_unemployment_insurance_contribution = income_fraction_of_unemployment_insurance_contribution,
                       income_fraction_of_long_term_care_contribution = income_fraction_of_long_term_care_contribution,
                       income_fraction_of_health_insurance_contribution = income_fraction_of_health_insurance_contribution,
                       income_tax_only = income_tax_only) / gross_income)
}

# Calculates the tax payment. Most programs call this functions somewhere. It includes some special features, such as
# assuming a flat tax and adjusting prices or ingnoring all taxes other than the income tax.
getTaxPayment <- function(gross_income,
                          inculde_welfare_benefits_fraction = global_inculde_welfare_benefits_fraction,
                          income_fraction_of_pension_contribution = global_income_fraction_of_pension_contribution,
                          income_fraction_of_unemployment_insurance_contribution = global_income_fraction_of_unemployment_insurance_contribution,
                          income_fraction_of_long_term_care_contribution = global_income_fraction_of_long_term_care_contribution,
                          income_fraction_of_health_insurance_contribution = global_income_fraction_of_health_insurance_contribution,
                          prices_year = 2019,
                          flat_tax = global_flat_tax,
                          assume_flat_tax = global_assume_flat_tax,
                          income_tax_only = global_income_tax_only) {
  # gross_income should be in prices_year euros. This assumes that the tax system is regularly adjusted to make it independent
  # from prices. I.e. there are no inflation incuded tax increases ("Kalte Progression")

  if(assume_flat_tax) {
    return(flat_tax * gross_income)
  }

  # Inflate gross income to the year for which the tax system is modeled
  gross_income_inflated <- deflate(prices_year, 2019) * gross_income

  if (!income_tax_only) {
    tax_payment <- getTaxSystemEffects(gross_income_inflated,
                                       inculde_welfare_benefits_fraction,
                                       income_fraction_of_pension_contribution,
                                       income_fraction_of_unemployment_insurance_contribution,
                                       income_fraction_of_long_term_care_contribution,
                                       income_fraction_of_health_insurance_contribution)$tax_yearly
  }
  else {
    tax_payment <- 12 * getTaxSystemEffects(gross_income_inflated,
                                            inculde_welfare_benefits_fraction,
                                            income_fraction_of_pension_contribution,
                                            income_fraction_of_unemployment_insurance_contribution,
                                            income_fraction_of_long_term_care_contribution,
                                            income_fraction_of_health_insurance_contribution)$income_tax_monthly
  }

  # Deflate tax_payment back to initial year:
  tax_payment_deflated <-  deflate(2019, prices_year) * tax_payment

  return(tax_payment_deflated)
}

# This functions inverts the tax calculations and allows us to infer the gross income from the net income.
getGrossIncome <- function(net_income,
                           flat_tax = global_flat_tax,
                           assume_flat_tax = global_assume_flat_tax,
                           inculde_welfare_benefits_fraction = global_inculde_welfare_benefits_fraction,
                           welfare_benefit = global_welfare_benefit_monthly,
                           income_fraction_of_pension_contribution = global_income_fraction_of_pension_contribution) {
  # This function aims to infer the gross income from the net_income:

  # If the relevant tax is flat, this is rather simple:
  if (assume_flat_tax) {
    return(net_income / (1- flat_tax))
  }

  # If the non-linear German tax system is assumed, the function that maps from gross income to net income has to be reversed.
  # This is equal to solving f = getNetIncome(gross_income) - net_income = 0
  # A problem that might arise is that if the tax system generates income regions where the marginal tax rate is > 1, the
  # the function cannot be uniquely inverted.

  # Define f as above:
  f <- function(gross_income) {
    getNetIncome(gross_income = gross_income,
                 inculde_welfare_benefits_fraction = inculde_welfare_benefits_fraction,
                 income_fraction_of_pension_contribution = income_fraction_of_pension_contribution) - net_income
  }

  # Net incomes below the welfare benefit do not make sense. In this case the root finding below
  # would fail.
  if (inculde_welfare_benefits_fraction > 0 & net_income < inculde_welfare_benefits_fraction * welfare_benefit * 12) {
    warning("Net income cannot be lower than the yearly welfare benefit. Assuming 0 gross income")
    return(0)
  }

  # Find the root of the f function (see above)
  gross_income <- uniroot(f = f, interval = c(0, 2*net_income))$root

  return(gross_income)
}

# Calculates the marginal rate at some gross income.
getMarginalTaxRate <- function(gross_income,
                               inculde_welfare_benefits_fraction = global_inculde_welfare_benefits_fraction,
                               income_fraction_of_pension_contribution = global_income_fraction_of_pension_contribution,
                               income_fraction_of_unemployment_insurance_contribution = global_income_fraction_of_unemployment_insurance_contribution,
                               income_fraction_of_long_term_care_contribution = global_income_fraction_of_long_term_care_contribution,
                               income_fraction_of_health_insurance_contribution = global_income_fraction_of_health_insurance_contribution,
                               income_tax_only = global_income_tax_only) {

  # Marginal Tax Rate = 1 - (NetIncome(gross income + 1) - NetIncome(gross income))
  if (!income_tax_only) {
    return(1 - (getNetIncome(gross_income + 1,
                             inculde_welfare_benefits_fraction,
                             income_fraction_of_pension_contribution,
                             income_fraction_of_unemployment_insurance_contribution,
                             income_fraction_of_long_term_care_contribution,
                             income_fraction_of_health_insurance_contribution) -
      getNetIncome(gross_income,
                   inculde_welfare_benefits_fraction,
                   income_fraction_of_pension_contribution,
                   income_fraction_of_unemployment_insurance_contribution,
                   income_fraction_of_long_term_care_contribution,
                   income_fraction_of_health_insurance_contribution)))
  }
  else {
    return(12*(getTaxSystemEffects(gross_income + 1)$income_tax_monthly - getTaxSystemEffects(gross_income)$income_tax_monthly))
  }
}

# This gives back the income tax due as a function of taxable income. Calculations are for the year 2020.
incomeTax <- function(taxable_income) {
  # There income tax is a piece-wise defined function. There are 4 cut-off points. These cut-offs are regularly adjusted.
  personal_exemption <- 9408 # also known as "Grundfreibetrag", i.e. the income below which no tax has to be payed
  section_2 <- 14532
  section_3 <- 57051
  section_4 <- 270500

  if (taxable_income <= personal_exemption) {
    return(0)
  }
  else if (taxable_income <= section_2) {
    return((0.14 + (taxable_income- 9408) * 972.87 * 10^-8) * (taxable_income - 9408))
  }
  else if (taxable_income <= section_3) {
    return((0.2397 + (taxable_income- 14532) * 212.02 * 10^-8) * (taxable_income - 14532) + 972.79)
  }
  else if (taxable_income <= section_4) {
    return(0.42 * taxable_income - 8963.74)
  }
  else {
    return(0.45 * taxable_income - 17078.74)
  }
}

# Returns the marginal income tax only
marginalIncomeTaxRate <- function(taxable_income) {
  return(incomeTax(taxable_income + 1) - incomeTax(taxable_income))
}

# Returns the average income tax only
averageIncomeTaxRate <- function(taxable_income) {
  return(incomeTax(taxable_income) / taxable_income)
}

# Calculates the solidarity charge in 2020.
solidarityCharge <- function(taxable_income) {
  excemption_limit <- 972 # Will be raised to 16956 in 2021
  solidarity_charge <- 0.055

  # This "tax" is payed on top of the income tax if it exceeds the excemption limit
  income_tax <- incomeTax(taxable_income)
  if (income_tax <= excemption_limit) {
    return(0)
  }
  else {
    # The Soli cannot exceed 20% of the income tax above the excemption limit.
    # Without this limit there would be a discontinous jump at the excemption limit.
    # This way, the "marginal soli" is 20 percent in the region above and close to the excemption limit.
    return(min(solidarity_charge * income_tax, 0.2 * (income_tax - excemption_limit)))
  }
}

# Generates and saves plots that illustrate the German Tax and Transfer System. (Mostly marginal and averages
# rates as a function of gross earnings)
plotTaxRates <- function(income_tax_only = FALSE) {
  # Assumptions for the plots:
  inculde_welfare_benefits_fraction = global_inculde_welfare_benefits_fraction
  income_fraction_of_pension_contribution = global_income_fraction_of_pension_contribution
  income_fraction_of_unemployment_insurance_contribution = global_income_fraction_of_unemployment_insurance_contribution
  income_fraction_of_long_term_care_contribution = global_income_fraction_of_long_term_care_contribution
  income_fraction_of_health_insurance_contribution = global_income_fraction_of_health_insurance_contribution


  # Calculate all the Tax Payment, Net incomes, social insurance contributions etc. for a wide range of incomes
  # Income Range
  income_range <- 0:2000*100

  get_tax_and_transfer_data <- function() {
    # Initialize dataframe by calculating the first row
    taxes_and_transfers <- getTaxSystemEffects(income_range[1],
                                               inculde_welfare_benefits_fraction,
                                               income_fraction_of_pension_contribution,
                                               income_fraction_of_unemployment_insurance_contribution,
                                               income_fraction_of_long_term_care_contribution,
                                               income_fraction_of_health_insurance_contribution)
    # Complete the dataframe by iterating over range of incomes
    for (i in 2:length(income_range)) {
      taxes_and_transfers[i, ] <- getTaxSystemEffects(income_range[i],
                                                      inculde_welfare_benefits_fraction,
                                                      income_fraction_of_pension_contribution,
                                                      income_fraction_of_unemployment_insurance_contribution,
                                                      income_fraction_of_long_term_care_contribution,
                                                      income_fraction_of_health_insurance_contribution)
    }
    return(taxes_and_transfers)
  }

  taxes_and_transfers <- get_tax_and_transfer_data()


  # Keep the relevant information for each of the figures:
  figure_net_income_data <- taxes_and_transfers %>% select(gross_income_monthly, solidarity_charge_monthly, income_tax_monthly,
                                                           long_term_care_contribution , health_insurance_contribution,
                                                           unemployment_insurance_contribution, pension_contribution,
                                                           welfare_benefit)
  # Welfare Benefit is a negative tax
  figure_net_income_data$welfare_benefit <- -figure_net_income_data$welfare_benefit

  figure_net_income_data <- gather(data = figure_net_income_data, key = "tax_group", value = "Euro",
                                   -gross_income_monthly)

  # Order Plot:
  figure_net_income_data$tax_group <- factor(figure_net_income_data$tax_group,
                                             levels = c("solidarity_charge_monthly",
                                                        "income_tax_monthly",
                                                        "long_term_care_contribution",
                                                        "health_insurance_contribution",
                                                        "unemployment_insurance_contribution",
                                                        "pension_contribution",
                                                        "welfare_benefit"))


  #This figure depicts the composition of the tax system
  figure_net_income <- ggplot(aes(x = gross_income_monthly, y = Euro, fill = tax_group), data = figure_net_income_data) +
    geom_area() +
    theme_modified_minimal() +
    coord_equal() +
    xlab("Gross Earnings per Month") +
    ylab("Euro per Month") +
    scale_x_continuous(limits = c(0, 10000)) +
    scale_y_continuous(limits = c(-1000, 9000),
                       expand = expansion(mult = c(0, 0))) +
    scale_fill_discrete(name = "Legend:", labels = c("Solidarity Charge", "Income Tax",
                                                     "Long Term Care Insurance", "Health Insurance",
                                                     "Unemployment Insurance",  "Pension Contribution",
                                                     "Welfare Benefit"))
  ggsave(figure_net_income, filename = "tax_components.pdf", device = pdf, path = "./plots/",  width = 7.6, height = 5)

  #Plot gross income against net income and tax payed:
  figure_net_income_data_reduced <- taxes_and_transfers %>% select(gross_income_monthly, net_income_monthly, tax_monthly)
  figure_net_income_data_reduced <- gather(data = figure_net_income_data_reduced, key = "net_income_tax", value = "Euro", -gross_income_monthly)

  figure_net_income_reduced <- ggplot(aes(x = gross_income_monthly, y = Euro, color = net_income_tax), data = figure_net_income_data_reduced) +
    geom_line() +
    theme_modified_minimal() +
    xlab("Gross Earnings per Month") +
    ylab("Euro per Month") +
    scale_x_continuous(limits = c(0, 7000)) +
    scale_y_continuous(limits = c(-1000, 4000),
                       expand = expansion(mult = c(0, 0))) +
    scale_colour_discrete(name = "Legend:", labels = c("Net Income", "Tax Net of Transfers"))

  print(figure_net_income_reduced)
  ggsave(figure_net_income_reduced, filename = "figure_net_income_reduced.pdf", device = pdf, path = "./plots/",  width = 7.6, height = 5)



  # Keep the relevant information for each of the Figures:


  get_averge_marginal_tax_data <- function(taxes_and_transfers, income_tax_only = FALSE) {
    if (income_tax_only) {
      figure_averge_marginal_tax_data <- taxes_and_transfers %>% select(gross_income_monthly, income_tax_monthly)
      figure_averge_marginal_tax_data$average_tax_rate <- figure_averge_marginal_tax_data$income_tax_monthly /
        figure_averge_marginal_tax_data$gross_income_monthly
      figure_averge_marginal_tax_data$marginal_tax_rate <- sapply(figure_averge_marginal_tax_data$gross_income_monthly * 12,
                                                                  getMarginalTaxRate,
                                                                  income_tax_only = TRUE)
    }
    else {
      figure_averge_marginal_tax_data <- taxes_and_transfers %>% select(gross_income_monthly, net_income_monthly)
      figure_averge_marginal_tax_data$average_tax_rate <- 1 - figure_averge_marginal_tax_data$net_income_monthly /
        figure_averge_marginal_tax_data$gross_income_monthly

      figure_averge_marginal_tax_data$marginal_tax_rate <- sapply(figure_averge_marginal_tax_data$gross_income_monthly * 12,
                                                                  getMarginalTaxRate,
                                                                  inculde_welfare_benefits_fraction = inculde_welfare_benefits_fraction,
                                                                  income_fraction_of_pension_contribution = income_fraction_of_pension_contribution)
    }
    if (income_tax_only) {
      figure_averge_marginal_tax_data <- figure_averge_marginal_tax_data %>% select(-income_tax_monthly)
    }
    else {
      figure_averge_marginal_tax_data <- figure_averge_marginal_tax_data %>% select(-net_income_monthly)
    }

    figure_averge_marginal_tax_data <- gather(data = figure_averge_marginal_tax_data, key = "marginal_average", value = "tax_rate", -gross_income_monthly)
    return(figure_averge_marginal_tax_data)
  }

  figure_averge_marginal_tax_data <- get_averge_marginal_tax_data(taxes_and_transfers)

  figure_averge_marginal_tax <- ggplot(aes(x = gross_income_monthly * 12, y = tax_rate, color = marginal_average), data = figure_averge_marginal_tax_data) +
    geom_line() +
    theme_modified_minimal() +
    scale_x_continuous(limits = c(0, 150000)) +
    scale_y_continuous(limits = c(0, 1.1),
                       breaks = c(0, 0.25, 0.5 ,0.75, 1),
                       expand = expansion(mult = c(0, 0))) +
    scale_colour_discrete(name = "Legend:", labels = c("Average Tax Rate", "Marginal Tax Rate")) +
    xlab("Gross Earnings per Year") +
    ylab("Tax Rate")

  print(figure_averge_marginal_tax)
  ggsave(figure_averge_marginal_tax, filename = "marginal_and_average_taxrate.pdf", device = pdf, path = "./plots/", width = 7.6, height = 5)

  # Plot Income Tax Only:

  # Change Assumptions
  inculde_welfare_benefits_fraction <- 0
  income_fraction_of_pension_contribution <- 1
  income_fraction_of_unemployment_insurance_contribution <- 1
  income_fraction_of_health_insurance_contribution <- 1 #The fraction of pension contributions that is considered income
  income_fraction_of_long_term_care_contribution <- 1

  figure_averge_marginal_tax_data <- get_averge_marginal_tax_data(get_tax_and_transfer_data(), income_tax_only = TRUE)

  figure_averge_marginal_income_tax <- ggplot(aes(x = gross_income_monthly * 12, y = tax_rate, color = marginal_average), data = figure_averge_marginal_tax_data) +
    geom_line() +
    theme_modified_minimal() +
    scale_x_continuous(limits = c(0, 150000)) +
    scale_y_continuous(limits = c(0, 0.6),
                       breaks = c(0, 0.25, 0.5),
                       expand = expansion(mult = c(0, 0))) +
    scale_colour_discrete(name = "Legend:", labels = c("Average Tax Rate", "Marginal Tax Rate")) +
    xlab("Gross Earnings per Year") +
    ylab("Income Tax Rate")


  print(figure_averge_marginal_income_tax)
  ggsave(figure_averge_marginal_income_tax, filename = "marginal_and_average_income_taxrate.pdf", device = pdf, path = "./plots/", width = 7.6, height = 5)

}

generateCIString <- function(lowerCI, upperCI) {
  lowerCI <- sapply(lowerCI, roundAndreplace)
  upperCI <- sapply(upperCI, roundAndreplace)
  CIString <- paste0("[", lowerCI, ", ", upperCI, "]")
  CIString[grepl(pattern = "-", x = CIString) & is.na(lowerCI) & is.na(upperCI)] <- "-"
  CIString[grepl(pattern = "\\[-, -\\]", x = CIString)] <- "-"
  return(CIString)
}



roundAndreplace <-  function(value) {
  if (is.na(value)) {
    return("-")
  }
  if (value == Inf) {
    return("$\\infty$") # This is a work around. It should be ∞ or \u221e. But there is a bug in R on Windows that converts infinty to 8.
  }
  else if(value == -Inf) {
    return("$\\infty$") # This is a work around using Latex's math mode. It should be ∞ or \u221e. But there is a bug in R on Windows that converts infinty to 8.
  }
  else {
    return(round(value, 2))
  }
}

roundToString <- function(values, remove_trailing_zeros = TRUE) {
  return (sapply(values, function(value) {
    if (is.na(value)) {
      return("-")
    }
    rounded_string <- paste0("", round(value,2))
    if (remove_trailing_zeros) {
      while(nchar(rounded_string) > 1 & grepl(",", rounded_string)) {
        if (substr(rounded_string, nchar(rounded_string), nchar(rounded_string)) != "0") {
          break
        }
        else if (substr(rounded_string, nchar(rounded_string), nchar(rounded_string)) == ",") {
          rounded_string <- substr(rounded_string, 1, nchar(rounded_string) - 1)
          break
        }
        else {
          rounded_string <- substr(rounded_string, 1, nchar(rounded_string) - 1)
        }
      }
      return(rounded_string)
    }
  }))
}

#----------------------------------------------------------------------------------------------------------------------#
# Latex Export
#----------------------------------------------------------------------------------------------------------------------#

exportLatexTables <- function(plot_data)  {
  # Store category names and count in a data frame
  categories <- plot_data %>% group_by(category) %>% summarize(count=n()) %>% as.data.frame()
  # Define order of programs:
  order <- order_of_categories

  # Add program cost = fiscal externalities to programs with no program cost
  export_data <- impute_missing_program_costs(plot_data)
  # Sort programs by category
  export_data$category <- factor(export_data$category, levels = order)
  export_data <- export_data %>% arrange(category)

  categories$category <- factor(categories$category, levels = order)
  categories <- categories %>% arrange(category)

  # Generate additional columns:
  export_data$mvpf <- sapply(export_data$mvpf, roundAndreplace)
  export_data$ci_string_mvpf <- generateCIString(export_data$mvpf_95ci_lower,
                                                 export_data$mvpf_95ci_upper)

  export_data$ci_string_cost <- generateCIString(export_data$government_net_costs_per_program_cost_95ci_lower,
                                                 export_data$government_net_costs_per_program_cost_95ci_upper)

  export_data$ci_string_wtp  <- generateCIString(export_data$willingness_to_pay_per_program_cost_95ci_lower,
                                                 export_data$willingness_to_pay_per_program_cost_95ci_upper)

  export_data$ci_string_fe  <- generateCIString(export_data$fiscal_externality_per_euro_95ci_upper,
                                                export_data$fiscal_externality_per_euro_95ci_lower)

  # Set the font size of tables here:
  table_font_size <- 11

  # Put the first column in a mini page environment to fix the non-indented second line:
  # as it's done here: https://stackoverflow.com/questions/50859270/
  fixKableLineBreaks <- function(width, font_size, column) {
    paste0("\\begin{minipage}[t]{", width ,"}\\raggedright\\setstretch{1}\\fontsize{",font_size,"}{",font_size * 1.2,"}\\selectfont ", column, "\\vspace{1.2ex}\\end{minipage}")
  }
  export_data$program_name <- fixKableLineBreaks("5cm", table_font_size, export_data$program_name)
  #--------------------------------------------------------------------------------------------------------------------#
  # Table with main results & CIs
  #--------------------------------------------------------------------------------------------------------------------#

  # Select columns to be shown in resulting table
  program_export_results <- export_data %>% arrange(category) %>%
    select(program_name, mvpf, ci_string_mvpf, government_net_costs_per_program_cost, ci_string_cost, willingness_to_pay_per_program_cost, ci_string_wtp)


  # Give each of the columns names without underscores
  colnames(program_export_results) <- c("Program", "MVPF", "MVPF CI", "Net Cost per Euro", "Cost CI", "WTP per Euro", "WTP CI")

  programs_table <- kable(program_export_results, digits = 2,
                          format = "latex",
                          booktabs = T,
                          escape = F,
                          longtable = T,
                          caption = "List of all programs \\label{programsTableResults}",
                          align = c('l', rep('c', ncol(program_export_results) - 1))) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"), font_size = table_font_size)

  program_count <- 1
  for (i in 1:nrow(categories)) {
    programs_table <- programs_table %>% pack_rows(categories[i, "category"], program_count, program_count + categories[i, "count"] - 1)
    program_count <- program_count + categories[i, "count"]
  }

  programs_table %>% save_kable("tex/programsTableResults.tex")

  #--------------------------------------------------------------------------------------------------------------------#
  # Table with Literature & Description
  #--------------------------------------------------------------------------------------------------------------------#
  program_export_additional_info <- export_data %>% arrange(category) %>%
    select(program_name, short_description)

  program_export_additional_info$short_description <- xtable::sanitize(program_export_additional_info$short_description)
  # Add space to make table more structured
  program_export_additional_info$short_description <- paste0(program_export_additional_info$short_description, "\\vspace{0.35cm}")
  program_export_additional_info$literature <- sapply(export_data$bibtexkeys, function(key) {
    # Return Latex Code that cites the relevant bibtex key and begins a new line for each cited paper
    paste(paste0("\\cite{",unlist(strsplit(key, ";")), "}"), collapse = "\\newline")
  })

  # Give each of the columns names without underscores
  colnames(program_export_additional_info) <- c("Program", "Short Description", "Literature")

  additional_info_table <- kable(program_export_additional_info, digits = 2,
                          format = "latex",
                          booktabs = TRUE,
                          escape = FALSE,
                          longtable = TRUE,
                          caption = "Programs \\label{programsTableLiterature}",
                          align = c('l','l','l')) %>%
                          column_spec(2, width = "11cm") %>%
                          column_spec(3, width = "6cm") %>%
                          kable_styling(latex_options = c("hold_position", "repeat_header"), font_size = table_font_size)

  # Add Category Headlines
  program_count <- 1
  for (i in 1:nrow(categories)) {
    additional_info_table <- additional_info_table %>% pack_rows(categories[i, "category"], program_count, program_count + categories[i, "count"] - 1,
                                                                 latex_gap_space = "0.6em", latex_wrap_text = TRUE)
    program_count <- program_count + categories[i, "count"]
  }

  additional_info_table %>% save_kable("tex/programsTableLiterature.tex")


  #--------------------------------------------------------------------------------------------------------------------#
  # Table with descriptive Statistics
  #--------------------------------------------------------------------------------------------------------------------#

  program_export_descriptives <- export_data %>% arrange(category) %>%
    select(program_name, average_age_beneficiary , average_earnings_beneficiary, year)

  program_export_descriptives$average_age_beneficiary <- roundToString(program_export_descriptives$average_age_beneficiary)
  program_export_descriptives$average_earnings_beneficiary <- roundToString(program_export_descriptives$average_earnings_beneficiary)

  # Give each of the columns names without underscores
  colnames(program_export_descriptives) <- c("Program", "Age Beneficiary", "Average Earnings", "Year")

  additional_info_table <- kable(program_export_descriptives, digits = 2,
                                 format = "latex",
                                 booktabs = TRUE,
                                 escape = FALSE,
                                 longtable = TRUE,
                                 caption = "Programs \\label{programsTableDescriptive}",
                                 align = c('l','c','c', 'c')) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"), font_size = table_font_size)

  # Add Category Headlines
  program_count <- 1
  for (i in 1:nrow(categories)) {
    additional_info_table <- additional_info_table %>% pack_rows(categories[i, "category"], program_count, program_count + categories[i, "count"] - 1)
    program_count <- program_count + categories[i, "count"]
  }

  additional_info_table %>% save_kable("tex/programsTableDescriptive.tex")
  
}

FolderCopy <- function() {
  # Copies some files to my Latex Workspace. Tried to do this with R. Failed miserably because R does not allow to pass system commands
  # or filenames to file.copy which contain Umlaute... Had to use python.
  system("python web/copyFiles.py")
}

#----------------------------------------------------------------------------------------------------------------------#
# Cost and Returns of Schooling and College
#----------------------------------------------------------------------------------------------------------------------#

# Returns the fiscal cost of studying 'duration_of_study' of years starting in 'year' at German university.
# The results is returned  in prices_year. state_token allows specifying a 'Bundesland'
costOfCollege <- function(duration_of_study,
                          year,
                          state_token = "DE",
                          prices_year) {
  if (missing(prices_year)) {
    prices_year = year
  }
  # Years in which the student studies:
  years_at_college <- year:(year + duration_of_study -1)
  # Cost for each of the years:
  cost <- sapply(years_at_college,
                 getCollegeCostInformation,
                 state_token = state_token,
                 prices_year = prices_year)

  discounted_cost <- cost * discountVector(duration_of_study)

  return(sum(discounted_cost))
}

# Returns the average cost per student for a given year and a given state
# prices_year can be set to return the college cost in 'prices_year' euro.
# By default the prices are in 'year' euro.
getCollegeCostInformation <- function(year, state_token, prices_year) {

  if (missing(prices_year)) {
    prices_year <- year
  }

  if(!exists("college_costs")) {
    college_costs <<- read.csv(file = "./school_costs/college_costs_per_state.csv")
  }

  available_years <- as.numeric(substring(colnames(college_costs)[3:ncol(college_costs)],2))
  relevant_row <- college_costs %>% filter(grepl(state_token, token))

  if (year %in% available_years) {
    return(deflate(from = year, to = prices_year) * unlist(relevant_row[3:ncol(college_costs)][year == available_years]))
  }
  else {
    # If no data is available for a given year return the closest year adjusted for prices to the year of interest
    closest_year <- available_years[which(abs(available_years - year) == min(abs(available_years - year)))]
    return(deflate(from = closest_year,to = prices_year) * unlist(relevant_row[3:ncol(college_costs)][closest_year == available_years]))
  }
}
# Returns the fiscal cost of going to school for 'year' years.
# The results is returned  in prices_year. state_token allows specifying a 'Bundesland'
costOfSchool <- function(duration_of_schooling,
                         year,
                         school_type = "all_schools",
                         prices_year) {
  # Possible values for school_type are elementary_school, hauptschule, various_tracks, realschule, gymnasium,
  # gesamtschule, allgemeinbildende_schulen, berufsschule, berufsschule_dual, all_schools

  if (missing(prices_year)) {
    prices_year = year
  }

  # Years in which the student studies:
  years_at_school <- year:(year + duration_of_schooling -1)

  # Cost for each of the years:
  cost <- sapply(years_at_school,
                 getSchoolCostInformation,
                 school_type = school_type,
                 prices_year = prices_year)


  discounted_cost <- cost * discountVector(duration_of_schooling)

  return(sum(discounted_cost))
}

# Returns the cost of going to a school of type 'school_type' in 'year'.
getSchoolCostInformation <- function(year, school_type , prices_year) {
  # Possible values for school_type are elementary_school, hauptschule, various_tracks, realschule, gymnasium,
  # gesamtschule, allgemeinbildende_schulen, berufsschule, berufsschule_dual, all_schools

  if (missing(prices_year)) {
    prices_year <- year
  }

  if(!exists("school_costs")) {
    school_costs <<- read.csv(file = "./school_costs/school_cost.csv")
    # There are some years for which we only have information on the average cost over all school tracks
    # Replace the missing values by the average ratio of cost for this specific school type / average cost:

    mean_ratios <- colMeans(school_costs[complete.cases(school_costs), ] / school_costs[complete.cases(school_costs), "all_schools"])

    # This solution is not pretty, but it replaces the missing values as described above
    school_costs[!complete.cases(school_costs), -which(names(school_costs) == "year")] <-
      (bind_rows(replicate(sum(!complete.cases(school_costs)), mean_ratios, simplify = FALSE)) *
        school_costs[!complete.cases(school_costs), "all_schools"]) %>% select(-year)
  }

  available_years <- pull(school_costs %>% select(!!school_type, year) %>% filter(complete.cases(.)), year)

  if (year %in% available_years) {
    return(deflate(from = year, to = prices_year) * school_costs %>%
      filter(year == !!year) %>%
      select(!!school_type))  %>%
      unlist()
  }
  else {
    # If no data is available for a given year return the closest year adjusted for prices to the year of interest
    closest_year <- available_years[which(abs(available_years - year) == min(abs(available_years - year)))]
    return(deflate(from = closest_year, to = prices_year) * school_costs %>%
      filter(year == closest_year) %>%
      select(!!school_type))  %>%
      unlist()
  }
}

# Returns a matrix which contains the relative earnings differences between education_decision  and alternative.
# To be used in conjunction with project_lifetime_impact and the impact_magnitude_matrix option.
getEducationEffectOnEarnings <- function(education_decision = "university_degree",
                                         alternative = "vocational_educ",
                                         assume_constant_effect_from) {

  # The Impact of education decisions is based on data from IAB (2014) http://doku.iab.de/kurzber/2014/kb0114.pdf
  #
  # Possible values for education decision and alternatives are:
  # university_degree
  # applied_sciences_degree : Fachhochschule
  # abitur : includes everyone with abitur but without university degree / or applied sciences university degree
  # vocational_educ : vocational_educ implies no abitur
  # no_vocational_educ : no_vocational_educ implies no abitur

  # assume_constant_effect_from can be used to set an age above which the impact of the considered education path remains
  # constant. This is similar to the approach by Hendren & Sprung-Keyser (2019) who extrapolate the average effect from
  # years 7 to 14 after college enrollment into the future. This leads to lower returns to education. The education
  # premium increases with age (At least in the IAB data)

  # Check if the data has already been loaded. If not load the csv file.
  if (!exists("age_income_degree_table")) {
    age_income_degree_table <<- read.csv("./income_projection/age_income_degree.csv")
  }

  # Construct the impact for all ages:
  impact_magnitude_matrix <- data.frame(age = age_income_degree_table$age)
  # Higher education levels are have hard coded 0 earnings during education in the IAB data. The impact maginude is -1 in these
  # years. This is equivalent to assuming that individuals who attained more education would have earned the average earnings
  # of the alternative.
  impact_magnitude_matrix$impact_magnitude <- age_income_degree_table[, education_decision] / age_income_degree_table[, alternative] -1


  if (!missing(assume_constant_effect_from)) {
    impact_magnitude_matrix <- impact_magnitude_matrix %>% filter(age <= assume_constant_effect_from)
  }

  # Remove ages where both education paths have zero income, i.e. there is a 0 by 0 divison.
  impact_magnitude_matrix <- impact_magnitude_matrix %>% filter(!is.nan(impact_magnitude))

  # If the education decision results is zero income, and the alternative implies a positive income, the impact_magnitude
  # would be infinity. But what this acutally means is that the alternative gets a zero income and and the education
  # decision gets income as in the data. -> The impact should be 0.
  impact_magnitude_matrix[impact_magnitude_matrix$impact_magnitude == Inf, "impact_magnitude"] <- 0


  return(impact_magnitude_matrix)
}

# Returns the averages earnings of holders of a 'education_decision' degree relative to the average earnings
# averaged over all degrees
getRelativeControlGroupEarnings <- function(education_decision) {
  # Check if the relevant data has already been loaded. If not load the csv file.
  if (!exists("age_income_degree_table")) {
    age_income_degree_table <<- read.csv("./income_projection/age_income_degree.csv")
  }
  if (!exists("age_income_table")) {
    age_income_table <<- read.csv("./income_projection/age_income_cross_section.csv")
  }
  # In theory this could vary with age, but quick tests suggest that this would have a very small impact, and also
  # cause problems where the control group has zero earnings. -> Take the ratio of the present values of the lifetime
  # control group earnings to the average lifetime earnings.
  control_group_average_ratio <- sum(age_income_degree_table[, education_decision] * discountVector(length(age_income_degree_table[, education_decision] ))) /
    sum(age_income_table[, "income"] * discountVector(length(age_income_table[, "income"])))
  return(control_group_average_ratio)
}

# Returns the average earnigs at some 'age' and some 'education level'
getAverageIncome <- function(age, education) {
  if (!exists("age_income_degree_table")) {
    age_income_degree_table <<- read.csv("./income_projection/age_income_degree.csv")
  }
  if (missing(age)) {
    # average over all ages
    return(mean(age_income_degree_table[, education]))
  }
  if (missing(education)) {
    # average income for a given age, currently not possible
  }
  return(age_income_degree_table[age_income_degree_table$age == age, education])
}

# This function conducts robustness checks and exports a plot that shows the MVPF plotted against the
# average age age of beneficiaries for each specification. Specifications are defined by passing a
# function to the 'robustnesscheck_assumptions' parameter. This function has to take integers as input.
# And then set the assumptions accordingly. I.e. if the 1 is passed to this function, it has to exectute
# all the code that characterizes specification 1. See the usage of robustnessCheck in main.R
robustnessCheck <- function(programs,
                            robustnesscheck_assumptions,
                            headlines,
                            overwrite_bootstrap_replications = bootstrap_replications,
                            save = "") {
  # programs is the vector of programs returned by getCompletePrograms
  # robustnesscheck_assumptions is function which takes the specification number 1 to n and stores the assumptions
  # in the global environment
  # headLines is vector containing the headlines of each of the subplots. This is not optional.
  # overwrite_bootstrap_replications can be set to reduce the number of bootstrap replications. It cannot be higher than
  # the value set for 'bootstrap_replications' in assumptions.R
  if (overwrite_bootstrap_replications > bootstrap_replications) {
    warning("Bootstrap replications cannot exceed than the number of replications set in 'bootstrap_replications' in assumptions.R")
    return(-1)
  }

  plots <- lapply(1:length(headlines), function(specification) {
    # Reset assumptions to be save:
    source("assumptions.R")

    bootstrap_replications <<- overwrite_bootstrap_replications

    # Load Assumptions
    robustnesscheck_assumptions(specification)
    applyAssumptions()

    # Pont Estimates
    mvpf_results <- getPointEstimates(programs)

    # Bootstrap
    mvpf_results <- addBootstrappedConfidenceIntervalls(mvpf_results)

    # Additional Data for Plots
    plot_data <- getPlotData(mvpf_results)

    # Category Plot data
    category_plot_data <- getCategoryPlotData(plot_data, all_bootstrap_replications_results)
    plot <- plotResults(plot_data = plot_data,
                        category_plot_data = category_plot_data,
                        x_axis = "average_age_beneficiary",
                        x_label = "Average Age of Beneficiaries",
                        smaller_scale = TRUE,
                        confidence_intervalls =  TRUE)
    plot <- plot + ggtitle(headlines[specification]) + theme(plot.title = element_text(size=12))
    source("assumptions.R")
    return(plot)
  })

  combined <- (wrap_plots(plots) & theme(legend.position = "bottom"))
  combined <- combined + plot_annotation(tag_levels = 'A')
  combined <- combined + plot_layout(guides = "collect", ncol = 2)
  print(combined)

  if (save != "") {
    ggsave(combined, filename = save, device = pdf, path = "./plots/", width = 11, height = 8.5)
  }
}

#----------------------------------------------------------------------------------------------------------------------#
# CSV Export for web visualization
#----------------------------------------------------------------------------------------------------------------------#

# Returns a list of key value pairs, where the key specifies the name of the assumption and
# the value is a vector containing all possible values
getListOfAllMetaAssumptions <- function() {
  # Update this function when updating setMetaAssumptions!
  list_of_all_meta_assumptions <- list(
    discount_rate = c("7", "3", "1"),
    tax_rate = c("0", "10", "30", "50", "incometaxonly", "nonlinear"),
    returns_to_schooling = c("5","7", "11", "IAB"),
    value_of_statistical_life = c("1million", "2.5million", "5million"),
    co2_externality = c("0", "100","250"),
    wage_growth_rate = c("0", "05", "1", "15"),
    eti = c("base","0", "6")
  )
}

getWhatWorksMetaAssumptions <- function() {
  list_of_all_meta_assumptions <- list(
    discount_rate = c("7", "3", "1"),
    tax_rate = c("0", "10", "30", "50", "incometaxonly", "nonlinear"),
    returns_to_schooling = c("5","7", "11", "IAB"),
    co2_externality = c("0", "100","250"),
    eti = c("base","0", "6")
  )
}

# Sets assumption "key" to value "value". This functione essentially specifies what the assumptions specified
# in getListOfAllMetaAssumptions() actually do.
setMetaAssumption <- function(key, value) {
  # Update getListOfAllMetaAssumptions function when updating this function
  if (key == "discount_rate") {
    if (value == "7") {
      discount_rate <<- 0.07
    }
    else if (value == "3") {
      discount_rate <<- 0.03
    }
    else if (value == "1") {
      discount_rate <<- 0.01
    }
    else {
      warning(paste("Value", value, "of assumption", key, "not found"))
    }
  }
  else if (key == "returns_to_schooling") {
    if (value == "IAB") {
      global_use_constant_ols_return_to_schooling <<- FALSE
    }
    else if (value == "5") {
      global_use_constant_ols_return_to_schooling <<- TRUE
      yearly_return_to_schooling <<- 0.05
    }
    else if (value == "7") {
      global_use_constant_ols_return_to_schooling <<- TRUE
      yearly_return_to_schooling <<- 0.07
    }
    else if (value == "11") {
      global_use_constant_ols_return_to_schooling <<- TRUE
      yearly_return_to_schooling <<- 0.11
    }
    else {
      warning(paste("Value", value, "of assumption", key, "not found"))
    }
  }
  else if (key == "tax_rate") {
    if (value == "0") {
      global_assume_flat_tax <<- TRUE
      global_flat_tax <<- 0
    }
    else if (value == "10") {
      global_assume_flat_tax <<- TRUE
      global_flat_tax <<- 0.1
    }
    else if (value == "30" ) {
      global_assume_flat_tax <<- TRUE
      global_flat_tax <<- 0.3
    }
    else if(value == "50") {
      global_assume_flat_tax <<- TRUE
      global_flat_tax <<- 0.5
    }
    else if(value == "nonlinear") {
      # No variables need to be set, this is the baseline!
    }
    else if(value == "incometaxonly") {
      # Assume that only the income tax (without soli) is relevant
      global_income_tax_only <<- TRUE
    }
    else {
      warning(paste("Value", value, "of assumption", key, "not found"))
    }
  }
  else if (key == "value_of_statistical_life") {
    if (value == "1million") {
      use_single_statistical_life_value <<- TRUE
      value_of_statistical_life <<- 10^6
    }
    else if (value == "2.5million") {
      use_single_statistical_life_value <<- TRUE
      value_of_statistical_life <<- 2.5 * 10^6
    }
    else if (value == "5million") {
      use_single_statistical_life_value <<- TRUE
      value_of_statistical_life <<- 5 * 10^6
    }
    else {
      warning(paste("Value", value, "of assumption", key, "not found"))
    }
  }
  else if (key == "co2_externality") {
    if (value == "0") {
      co2_externality <<- 0
    }
    else if (value == "100") {
      co2_externality <<- 100
    }
    else if (value == "250") {
      co2_externality <<- 250
    }
    else {
      warning(paste("Value", value, "of assumption", key, "not found"))
    }
  }
  else if (key == "wage_growth_rate") {
    if (value == "0") {
      wage_growth_rate <<- 0
    }
    else if (value == "05") {
      wage_growth_rate <<- 0.005
    }
    else if (value == "1") {
      wage_growth_rate <<- 0.01
    }
    else if (value == "15") {
      wage_growth_rate <<- 0.015
    }
    else {
      warning(paste("Value", value, "of assumption", key, "not found"))
    }
  }
  else if (key == "eti") {
    if (value == "base") {
      # Baseline
    }
    else if (value == "0") {
      overwrite_eti <<- TRUE
      global_eti <<- 0
    }
    else if (value == "6") {
      overwrite_eti <<- TRUE
      global_eti <<- 0.6
    }
    else {
      warning(paste("Value", value, "of assumption", key, "not found"))
    }
  }
  else {
    warning(paste("Assumption", key, "not found"))
    return(-1)
  }
  applyAssumptions()
}

exportJSON <- function(programs = getCompletePrograms(),
                       all_programs = getCompletePrograms(),
                       only_default_specification = FALSE) {
  # Exports the json files for all programs in the programs vector + the json files that contain the information about the variable mapping
  # description, categories etc. Run this when adding a single reform with programs = "reformToAdd". Default is to export
  # all programs which can be a lenghty process.
  # subset_programs should be a vector of programs for which the json files should be exported (can also be only one string)
  # all_programs should be all programs e.g. the vector returned from getCompletePrograms()

  exportReformJSON(all_programs)
  exportCategoriesJSON()
  exportPlotJSON(programs, only_default_specification = only_default_specification)
  file.copy("variable_mapping.json", "./json_export/variable_mapping.json", overwrite = T)
}

exportReformJSON <- function(programs = getCompletePrograms()) {
  # Generates the programs.json file which contains basic characteristics about each reform like
  # year, name, age of beneficiaries etc. All info is read from the programs.xlsx
  if (!exists("program_information")) {
    # Load additional information about each policy from the excel file:
    program_information <<- as.data.frame(read_xlsx("programs.xlsx"))
  }

  df_to_json <- data.frame()
  for (i in 1:length(programs)) {
    current_program = programs[i]

    df_row <- program_information %>%
      filter(program_identifier == current_program) %>%
      select(program_identifier,
             program_name,
             category,
             year,
             average_age_beneficiary,
             average_earnings_beneficiary,
             sources,
             links,
             bibtexkeys,
             what_works_included,
             identification,
             maryland_scale,
             peer_reviewed,
             short_description)

    df_to_json[i, names(df_row)] <- df_row
  }
  # Create directory (if it does not exist yet):
  dir.create(file.path(getwd(), "json_export"), showWarnings = FALSE)
  writeLines(jsonlite::toJSON(df_to_json, pretty = T), paste0("./json_export/programs.json"), useBytes = T)
}

exportCategoriesJSON <- function() {
  if (exists("order_of_categories")) {
    # Create directory (if it does not exist yet):
    dir.create(file.path(getwd(), "json_export"), showWarnings = FALSE)
    writeLines(rjson::toJSON(order_of_categories, indent = 1),"./json_export/categories.json")
  }
  else {
    print("Order of categories not found. Cannot export categories.json Has assumptions.R been run?")
  }
}

getVariableMapping <- function(program) {
  # Read variable_mapping.json
  variable_mapping = rjson::fromJSON(file = "variable_mapping.json")

  # Here we are looking for the program in the variable_mapping json structure. If we find it we return the list
  for (i in 1:length(variable_mapping)) {
    if (variable_mapping[[i]]$program == program) {
      return(variable_mapping[[i]])
    }
  }
  warning(paste0("No entry in variable_mappings.json was found for program ", current_program))
}

# This function calculates the MVPF of all possible combinations of assumptions. The resulting
# json file can be read by the web visualization in the ./web/ folder. This function replaces the exportPlotCSV
# function which creates one csv file per specification. We switch to JSON because this is easier to read for the browser.
# Additionally, we add the option export individual programs. So we get a subfolder for each program, which contains a json
# file for each specification
exportPlotJSON <- function(programs = getCompletePrograms(),
                           assumption_list = getWhatWorksMetaAssumptions(),
                           bootstrap  = FALSE,
                           meta_assumptions = TRUE,
                           only_default_specification = FALSE) {


  # Run with default assumption
  source("assumptions.R")
  results <- getPlotData(quietelyRunPrograms(programs, bootstrap))


  # Check if ./csv_export/ directory exists. If not create it.
  dir.create(file.path(getwd(), "json_export"), showWarnings = FALSE)

  # Read variable_mapping.json
  variable_mapping = rjson::fromJSON(file = "variable_mapping.json")

  # Store relevant variables in a list
  relevant_variables <- list()

  # Store relevant results
  store_relevant_results_in_JSON <- function(estimation_results, filename) {
    for (j in 1:nrow(estimation_results)) {
      current_program <- estimation_results[j, "program"]
      current_program_results <- estimation_results[j,]
      relevant_variables_vector <- c(relevant_variables[[current_program]],
                                     "mvpf",
                                     "prices_year",
                                     "willingness_to_pay_per_program_cost",
                                     "willingness_to_pay",
                                     "government_net_costs",
                                     "government_net_costs_per_program_cost")
      # Remove unnecesarry
      if ((!length(relevant_variables[[current_program]]) == 1) || relevant_variables[[current_program]] == "") {
        current_program_results <- current_program_results %>% select(all_of(relevant_variables_vector))
      }
      json_current_program <- rjson::toJSON(as.list(current_program_results), indent = 1)
      writeLines(json_current_program, paste0("./json_export/", current_program, "/", filename, ".json"))
    }
  }

  for (j in 1:length(programs)) {
    current_program = programs[j]
    # Create directory for each program
    dir.create(file.path(paste0(file.path(getwd()) , "/json_export"), current_program), showWarnings = FALSE)

    # Here we are looking for the program x in the variable_mapping json structure. If we find it we take note
    # of the relevant paramters for this reform
    program_found = FALSE
    for (i in 1:length(variable_mapping)) {
      if (variable_mapping[[i]]$program == current_program) {
        relevant_variables[[current_program]] <- unique(c(names(variable_mapping[[i]]$willingness_to_pay),
                                            names(variable_mapping[[i]]$government_net_costs)))
        program_found = TRUE
      }
    }
    if (!program_found) {
      warning(paste0("No entry in variable_mappings.json was found for program ", current_program))
      relevant_variables[[current_program]] <- ""
    }
  }

  # Store the results from the default assumptions run
  store_relevant_results_in_JSON(results, "default")

  if (only_default_specification) {
    return(0)
  }

  # Now work on all the possible combinations of the assumptions

  possible_assumption_combinations <- expand.grid(assumption_list)

  message("Running and Exporting the results of ", nrow(possible_assumption_combinations),
          " specifications as json. This can take a while.\n")

  start_time <- Sys.time()

  foreach(i = 1:nrow(possible_assumption_combinations),
          .export =  ls(globalenv())[!ls(globalenv()) %in% c("programs")], #this gets rid of some warnings
          .packages = c("dplyr", "readxl")) %do% {
    # Iterate over all possible assumption combinations

    # First reset all assumptions back to default:
    source("assumptions.R")

    #Generate a string that summarizes the assumptions:
    assumptions_string <- ""

    assumptions <- colnames(possible_assumption_combinations)
    for (j in 1:length(assumptions)) {
      # Iterate over all assumptions:
      if (!meta_assumptions) {
        assign(assumptions[j], possible_assumption_combinations[i,j], envir = .GlobalEnv)
      }
      else {
        setMetaAssumption(assumptions[j], possible_assumption_combinations[i,j])
      }
      #Generate a string that summarizes the assumptions:
      assumptions_string <- paste0(assumptions_string, assumptions[j], possible_assumption_combinations[i,j])
    }

    # Now that the assumptions have been set, we can run the estimation
    results <- getPlotData(quietelyRunPrograms(programs, bootstrap))

    # Store the results in JSON with the appropriate filename
    store_relevant_results_in_JSON(results, assumptions_string)
  }
  # Reset the assumptions.
  source("assumptions.R")

  message("Exporting results completed in ", difftime(Sys.time(), start_time, units='mins'), " minutes \n")
}

# This function calculates the MVPF of all possible combinations of assumptions. The resulting
# csv file can be read by the web visualization in the ./web/ folder
exportPlotCSV <- function(programs, assumption_list = getWhatWorksMetaAssumptions(), bootstrap  = FALSE, meta_assumptions = TRUE) {
  # assumption list is a list that specifies all the possible assumptions for which the code should be run.
  # Example:
  # assumption_list = list(tax_rate = c(0.5,0.3), discount_rate = c(0.1,0.2))
  # Would export csvs for all possible combinations of the tax_rate assumption and the discount_rate assumption
  # If meta_assumptions is set to false, the assumption_list has to contain actual variable names and values.
  # If meta_asumptions is set to true, the assumption will be looked up in setMetaAssumptions(). This useful to make
  # assumptions that require multiple variables to be set

  # Run with default assumption and save the csv file:
  source("assumptions.R")
  results <- quietelyRunPrograms(programs, bootstrap)

  # Check if ./csv_export/ directory exists. If not create it.
  dir.create(file.path(getwd(), "csv_export"), showWarnings = FALSE)

  write.csv(x = getPlotData(results),
            file = "./csv_export/default.csv",
            row.names = FALSE,
            fileEncoding = "UTF-8"
  )

  if (missing(assumption_list)) {
    return(0)
  }

  possible_assumption_combinations <- expand.grid(assumption_list)

  message("Running and Exporting the results of ", nrow(possible_assumption_combinations),
          " specifications as csv. This can take a while.\n")

  start_time <- Sys.time()

  foreach(i = 1:nrow(possible_assumption_combinations),
          .export =  ls(globalenv())[!ls(globalenv()) %in% c("programs")], #this gets rid of some warnings
          .packages = c("dplyr", "readxl")) %dopar% {
    # Iterate over all possible assumption combinations

    # First reset all assumptions back to default:
    source("assumptions.R")

    #Generate a string that summarizes the assumptions:
    assumptions_string <- ""

    assumptions <- colnames(possible_assumption_combinations)
    for (j in 1:length(assumptions)) {
      # Iterate over all assumptions:
      if (!meta_assumptions) {
        assign(assumptions[j], possible_assumption_combinations[i,j], envir = .GlobalEnv)
      }
      else {
        setMetaAssumption(assumptions[j], possible_assumption_combinations[i,j])
      }
      #Generate a string that summarizes the assumptions:
      assumptions_string <- paste0(assumptions_string, assumptions[j], possible_assumption_combinations[i,j])
    }

    # Now that the assumptions have been set, we can run the estimation
    results <- quietelyRunPrograms(programs, bootstrap)
    # load the additional data
    plot_data <- getPlotData(results)
    # remove columns not needed for visualization
    plot_data <- plot_data %>% select(-c(bibtexkeys, notes))
    write.csv(x = plot_data,
              file = paste0("./csv_export/", assumptions_string, ".csv"),
              row.names = FALSE,
              fileEncoding = "UTF-8")
  }
  # Reset the assumptions.
  source("assumptions.R")

  message("Exporting results completed in ", difftime(Sys.time(), start_time, units='mins'), " minutes \n")
}