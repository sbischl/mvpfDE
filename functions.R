drawBootstrap <- function(path_to_xlsx, number_of_replications) {
  # Set seed
  set.seed(bootstrap_seed)

  # Load Estimates
  estimates <- as.data.frame(read_xlsx(path_to_xlsx))

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

  bootstrapped_estimates <- matrix(data = NA, nrow = number_of_replications, ncol = nrow(estimates))
  colnames(bootstrapped_estimates) <- estimates$estimate

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

getEstimates <- function(program, bootstrap_replication) {
  if (bootstrap_replication == 0) {
    estimates <- read_xlsx(paste0("./estimates/", program, ".xlsx"))
    point_estimates <- data.frame(t(estimates$point_estimate))
    colnames(point_estimates) <- estimates$estimate
    return(point_estimates)
  }

  # Reading only the two required lines, this should be faster than reading the whole csv everytime
  bootstrap_estimates <- read.csv(paste0("./bootstrap/", program, "_bootstrap.csv"),
                                  header = FALSE, nrows = 1, skip = bootstrap_replication)
  colnames(bootstrap_estimates) <- read.csv(paste0("./bootstrap/", program, "_bootstrap.csv"),
                                            header = FALSE, nrows = 1)
  return(bootstrap_estimates)
}

correlationToCovarianceMatrix <- function(correlation_matrix, standard_error_vector) {
  return((standard_error_vector %*% t(standard_error_vector)) * correlation_matrix)
}

calculateMVPF <- function(willingness_to_pay , government_net_costs) {
  if (willingness_to_pay > 0 & government_net_costs <= 0) {
    return(Inf)
  }
  else if (willingness_to_pay < 0 & government_net_costs == 0) {
    return(-Inf)
  }
  else {
    return(willingness_to_pay / government_net_costs)
  }
}

deflate <- function(from, to) {
  if (!exists("cpi")) {
    # Load consumer price index into the global environment
    cpi <<- read.csv("./cpi/cpi.csv")
  }
  index_from <- cpi[cpi$year == from, "index"]
  index_to <- cpi[cpi$year == to, "index"]
  return(index_to / index_from)
}

splitAndDiscount <- function(amount, periods, discount_rate) {
  # Use this function if a cost / gain is split evenly across multiple periods.
  # This function then returns the present value of the cost / gain
  cash_flows <- rep(amount / periods, periods)
  present_value_cash_flows <- cash_flows / sapply(1:periods, function(x) {
    (1 + discount_rate)^x})
  return(sum(present_value_cash_flows))
}

plotResults <- function(y_axis = "mvpf", y_label = "MVPF", x_axis = "year", x_label = "Year", plot_data, save = "") {
  # Settings:
  # The highest MVPF to plot that is not infinity
  infinity_cutoff <- 6

  # Check if y_axis and x_axis actually exist in the plot_data
  if (!all(c(y_axis, x_axis) %in% colnames(plot_data))) {
    warning("Either x or y axis variable is not in the dataset")
    return(-1)
  }

  if (y_axis == "mvpf") {
    # Censor all values that are larger than the the infinity_cutoff and use infinity_cutoff + 1 as 'infinity'
    plot_data <- plot_data %>%
      mutate(mvpf = replace(mvpf, mvpf > infinity_cutoff & mvpf != Inf, infinity_cutoff)) %>%
      mutate(mvpf = replace(mvpf, mvpf == Inf, infinity_cutoff + 1))
  }

  plot <- ggplot(aes_string(y = y_axis, x= x_axis, color = "category"), data = plot_data) +
    ylab(y_label) +
    xlab(x_label) +
    geom_point() +
    theme_modified_minimal()

  if (y_axis == "mvpf") {
    plot <- plot + scale_y_continuous(breaks = floor(min(plot_data$mvpf)):(infinity_cutoff + 1),
                                      labels = c(as.character(min(plot_data$mvpf)):(infinity_cutoff -1), paste("\u2265", infinity_cutoff), "\u221E"))
  }

  print(plot)
  if (save != "") {
    #This gives a lot of warnings in case the font is not available
    ggsave(plot, filename = save, device = pdf, path = "./plots/", width = 6, height = 4)
  }
}

# Define custom ggplot theme:
theme_modified_minimal <- function() {
  # Set the font here. This only works if the font is installed on the system and it is available in R.
  # To make the font available use font_add("font name", "file name") and showtext_auto() from the 'showtext' package.
  theme_minimal(base_size=12, base_family=plot_font) %+replace%
    theme (
      axis.line = element_line(color = "black"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.ticks = element_line()
    )
}