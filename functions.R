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

splitAndDiscount <- function(amount, periods, discount_rate) {
  # Use this function if a cost / gain is split evenly across multiple periods.
  # This function then returns the present value of the cost / gain
  cash_flows <- rep(amount / periods, periods)
  present_value_cash_flows <- cash_flows / sapply(1:periods, function(x) {
    (1 + discount_rate)^x})
  return(sum(present_value_cash_flows))
}

discountMonthlyCashFlow <- function(amount, months) {
  full_years <- months %/% 12
  remaining_months <- months %% 12
  if (remaining_months > 0) {
    return(sum(c(rep(amount*12, full_years), remaining_months*amount) * discountVector(full_years + 1)))
  }
  return(sum(rep(amount*12, full_years) * discountVector(full_years)))
}

plotResults <- function(y_axis = "mvpf", y_label = "MVPF", x_axis = "year", x_label = "Year",
                        plot_data, save = "", lower_cutoff = -1, upper_cutoff = 6, confidence_intervalls = TRUE,
                        text_labels = TRUE, legend_label = "Category", vertical_x_axis_labels = FALSE) {

  # Check if y_axis and x_axis actually exist in the plot_data
  if (!all(c(y_axis, x_axis) %in% colnames(plot_data))) {
    warning("Either x or y axis variable is not in the dataset")
    return(-1)
  }

  # Check if confidence intervalls exist for y_axis variable:
  if (!all(c(paste0(y_axis,"_95ci_lower"), paste0(y_axis,"_95ci_upper")) %in% colnames(plot_data))) {
    confidence_intervalls <- FALSE
  }

  if (y_axis == "mvpf") {
    # Censor all values that are larger than the the infinity_cutoff and use infinity_cutoff + 1 as 'infinity'
    plot_data <- plot_data %>%
      mutate(mvpf = replace(mvpf, mvpf > upper_cutoff & mvpf != Inf, upper_cutoff)) %>%
      mutate(mvpf = replace(mvpf, mvpf == Inf, upper_cutoff + 1))

    if (confidence_intervalls) {
      plot_data <- plot_data %>%
        mutate(mvpf_95ci_upper = replace(mvpf_95ci_upper, mvpf_95ci_upper > upper_cutoff & mvpf_95ci_upper != Inf, upper_cutoff)) %>%
        mutate(mvpf_95ci_upper = replace(mvpf_95ci_upper, mvpf_95ci_upper == Inf, upper_cutoff + 1)) %>%
        mutate(mvpf_95ci_lower = replace(mvpf_95ci_lower, mvpf_95ci_lower > upper_cutoff & mvpf_95ci_lower != Inf, upper_cutoff)) %>%
        mutate(mvpf_95ci_lower = replace(mvpf_95ci_lower, mvpf_95ci_lower == Inf, upper_cutoff + 1))
    }
  }
  else if (!is.na(upper_cutoff)) {
    plot_data <- plot_data %>%
      mutate(!!y_axis := replace(get(y_axis), get(y_axis) > upper_cutoff, upper_cutoff)) %>%
      mutate(!!paste0(y_axis, "_95ci_upper") := replace(get(paste0(y_axis, "_95ci_upper")), get(paste0(y_axis, "_95ci_upper")) > upper_cutoff, upper_cutoff)) %>%
      mutate(!!paste0(y_axis, "_95ci_lower") := replace(get(paste0(y_axis, "_95ci_lower")), get(paste0(y_axis, "_95ci_lower")) > upper_cutoff, upper_cutoff))
  }

  if (!is.na(lower_cutoff)) {
    plot_data <-  plot_data %>%
      mutate(!!y_axis := replace(get(y_axis), get(y_axis) < lower_cutoff, lower_cutoff)) %>%
      mutate(!!paste0(y_axis, "_95ci_upper") := replace(get(paste0(y_axis, "_95ci_upper")), get(paste0(y_axis, "_95ci_upper")) < lower_cutoff, lower_cutoff)) %>%
      mutate(!!paste0(y_axis, "_95ci_lower") := replace(get(paste0(y_axis, "_95ci_lower")), get(paste0(y_axis, "_95ci_lower")) < lower_cutoff, lower_cutoff))
  }

  # Generate Category 'Other' which contains all programs that have no Category specified:
  plot_data$category <- coalesce(plot_data$category, "Other")
  # Assign the Program program identifier (folder name) as program name, if none is specified:
  plot_data$program_name <- coalesce(plot_data$program_name, plot_data$program)


  plot <- ggplot(aes_string(y = y_axis, x= x_axis), data = plot_data) +
    ylab(y_label) +
    xlab(x_label) +
    labs(color = legend_label)

  if (confidence_intervalls) {
    plot <- plot + geom_errorbar(aes_string(ymin = paste0(y_axis, "_95ci_lower"),
                                            ymax = paste0(y_axis, "_95ci_upper"),
                                            color = "category"),
                                 width = 0.1)
  }

  if (text_labels) {
    plot <- plot + geom_text_repel(aes(label = program_name), size = 2, color = "black")
  }

  if (y_axis == "mvpf") {
    plot <- plot + scale_y_continuous(breaks = lower_cutoff:(upper_cutoff + 1),
                                      minor_breaks = (lower_cutoff:(upper_cutoff - 1)) + 0.5,
                                      labels = c(paste("\u2264", lower_cutoff),
                                                 as.character((lower_cutoff + 1):(upper_cutoff -1)),
                                                 paste("\u2265", upper_cutoff), "\u221E"))
  }
  else {
    plot <- plot + scale_y_continuous(breaks = lower_cutoff:upper_cutoff,
                                      labels = c(paste("\u2264", lower_cutoff),
                                                 as.character((lower_cutoff + 1):(upper_cutoff -1)),
                                                 paste("\u2265", upper_cutoff)))
  }

  plot <- plot + geom_point(aes_string(y = y_axis, x= x_axis, color = "category")) +
    theme_modified_minimal()

  if (vertical_x_axis_labels) {
    plot <- plot + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
                         axis.title.x = element_blank())
  }

  print(plot)

  if (save != "") {
    ggsave(plot, filename = save, device = pdf, path = "./plots/", width = 7.6, height = 5)
  }
}

# Define custom ggplot theme:
theme_modified_minimal <- function() {
  # Set the font here. This only works if the font is installed on the system and it is available in R.
  # To make the font available use font_add("font name", "file name") and showtext_auto() from the 'showtext' package.
  theme_minimal(base_size=12, base_family=plot_font) %+replace%
    theme (
      axis.line = element_line(color = "black"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.ticks = element_line()
    )
}

project_medium_run_impact <- function(impact_magnitude, # can be either scalar or a vector containing the effect for each period
                                      absolute_impact_magnitude, #can be either scalar or a vector containing the effect for each period (change in yearly earnings)
                                      yearly_control_income,
                                      number_of_periods,
                                      prices_year) {

  # This function returns the dataframe as project_lifetime_impact but is simpler and requires less assumptions.
  # It is intened for reforms for reforms whose beneficiaries vary greatly.
  # Also this method does not assume wage growth

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
                                  prices_year = prices_year)

  tax_payment_reform <- sapply(gross_earnings_reform,
                               getTaxPayment,
                               prices_year = prices_year)

  net_earnings_no_reform <- gross_earnings_no_reform - tax_payment_no_reform
  net_earnings_refrom <- gross_earnings_reform - tax_payment_reform

  earnings_difference <- gross_earnings_reform - gross_earnings_no_reform
  tax_payment_difference <- tax_payment_reform - tax_payment_no_reform
  net_earnings_difference <- net_earnings_refrom - net_earnings_no_reform

  present_value_earnings_impact <- sum(earnings_difference * discountVector(number_of_periods))
  present_value_tax_payment_impact <- sum(tax_payment_difference * discountVector(number_of_periods))
  present_value_net_earnings_impact <- sum(net_earnings_difference * discountVector(number_of_periods))

  return(data.frame(present_value_earnings_impact = present_value_earnings_impact,
                    present_value_tax_payment_impact = present_value_tax_payment_impact,
                    present_value_net_earnings_impact = present_value_net_earnings_impact))
}



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

  # Import age-income relation: This should be estimated from the data, but for testing purposes use the data from Hendren & Sprung-Keyser (2020)
  age_income_table <- read.csv("./income_projection/age_income_cross_section.csv")

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
  age_income_table$income_price_adjusted <- age_income_table$income * deflate(from = 2015, to = prices_year)


  # Calculate the average income trajectory(i.e. average income for each age) for the population that is 'impact_age' years old in the year 'impact_year'
  # We have to take into accout that incomes are observed for only one year, incomes that are realized after (before) this
  # year grow (fall) by the growth rate. From the birth year (start_projection_year - start_projection_age) it is possible to infer the year when they are x years old.
  age_income_table$year <- (start_projection_year - start_projection_age) + age_income_table$age

  # Grow wages by (1+g)^(the number of years the income accrues after the year we have data for) / Or shrink if the exponent is negative
  age_income_table$income_fully_adjusted <- age_income_table$income_price_adjusted * (1 + wage_growth_rate)^(age_income_table$year - 2015)

  # If the income of the control group rather than the relative income is given, we need to calculate the relative
  # income.
  if (missing(relative_control_income)) {
    relative_control_income <- control_income / age_income_table[age_income_table$age == impact_age , "income_fully_adjusted"]
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

getNetIncome <- function(gross_income,
                         inculde_welfare_benefits_fraction = global_inculde_welfare_benefits_fraction,
                         income_fraction_of_pension_contribution = global_income_fraction_of_pension_contribution) {
  return(getTaxSystemEffects(gross_income, inculde_welfare_benefits_fraction, income_fraction_of_pension_contribution)$net_income_yearly)
}

getTaxSystemEffects <- function(gross_income,
                                inculde_welfare_benefits_fraction = global_inculde_welfare_benefits_fraction,
                                income_fraction_of_pension_contribution = global_income_fraction_of_pension_contribution) {

  # inculde_welfare_benefits_fraction is the fraction of welfare benefits (i.e. Hartz IV) that are considered net income.
  # A value < 1 represents the fact that not everyone who receives low income is entitled to Hartz IV
  # income_fraction_of_pension_contribution denotes the fraction of pension contributions that are considered net income.
  # Higher pension contributions result in higher pension payments but due to demographics the return is probably quite low.

  # adapted from:
  # 'Identifying Laffer Bounds: A Sufficient-Statistics Approach with an Application to Germany' by Sachs and Lorenz (2015) Appendix A
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
  net_income_monthly <- gross_income - social_security_contributions - income_tax_monthly - solidarity_charge_monthly +
    welfare_benefit * inculde_welfare_benefits_fraction +
    pension_contribution * income_fraction_of_pension_contribution

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

getAverageTaxRate <- function(gross_income,
                              inculde_welfare_benefits_fraction = global_inculde_welfare_benefits_fraction,
                              income_fraction_of_pension_contribution = global_income_fraction_of_pension_contribution) {

  # Average Tax Rate = 1 - NetIncome(gross income) / gross income)
  return(1 - getNetIncome(gross_income, inculde_welfare_benefits_fraction, income_fraction_of_pension_contribution) / gross_income)
}

getTaxPayment <- function(gross_income,
                          inculde_welfare_benefits_fraction = global_inculde_welfare_benefits_fraction,
                          income_fraction_of_pension_contribution = global_income_fraction_of_pension_contribution,
                          prices_year = 2019,
                          flat_tax = global_flat_tax,
                          assume_flat_tax = global_assume_flat_tax) {
  # gross_income should be in prices_year euros. This assumes that the tax system is regularly adjusted to make it independent
  # from prices. I.e. there are no inflation incuded tax increases ("Kalte Progression")

  if(assume_flat_tax) {
    return(flat_tax * gross_income)
  }

  # Inflate gross income to the year for which the tax system is modeled
  gross_income_inflated <- deflate(prices_year, 2019) * gross_income

  tax_payment <- getTaxSystemEffects(gross_income_inflated,
                                     inculde_welfare_benefits_fraction,
                                     income_fraction_of_pension_contribution)$tax_yearly

  # Deflate tax_payment back to initial year:
  tax_payment_deflated <-  deflate(2019, prices_year) * tax_payment

  return(tax_payment_deflated)
}

getMarginalTaxRate <- function(gross_income,
                               inculde_welfare_benefits_fraction = global_inculde_welfare_benefits_fraction,
                               income_fraction_of_pension_contribution = global_income_fraction_of_pension_contribution) {

  # Marginal Tax Rate = 1 - (NetIncome(gross income + 1) - NetIncome(gross income))
  return(1 - (getNetIncome(gross_income + 1, inculde_welfare_benefits_fraction, income_fraction_of_pension_contribution) -
    getNetIncome(gross_income, inculde_welfare_benefits_fraction, income_fraction_of_pension_contribution)))
}

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

marginalIncomeTaxRate <- function(taxable_income) {
  return(incomeTax(taxable_income + 1) - incomeTax(taxable_income))
}

averageIncomeTaxRate <- function(taxable_income) {
  return(incomeTax(taxable_income) / taxable_income)
}

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

plotTaxRates <- function() {
  # Assumptions for the plots:
  inculde_welfare_benefits_fraction = global_inculde_welfare_benefits_fraction
  income_fraction_of_pension_contribution = global_income_fraction_of_pension_contribution

  # Calculate all the Tax Payment, Net incomes, social insurance contributions etc. for a wide range of incomes
  # Income Range
  income_range <- 0:2000*100
  # Initialize dataframe by calculating the first row
  taxes_and_transfers <- getTaxSystemEffects(income_range[1],
                                             inculde_welfare_benefits_fraction,
                                             income_fraction_of_pension_contribution)

  # Complete the dataframe by iterating over range of incomes
  for (i in 2:length(income_range)) {
    taxes_and_transfers[i, ] <- getTaxSystemEffects(income_range[i],
                                                    inculde_welfare_benefits_fraction,
                                                    income_fraction_of_pension_contribution)
  }

  # Keep the relevant information for each of the Figures:
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
    xlab("Gross Income per Month") +
    ylab("Euro per Month") +
    scale_x_continuous(limits = c(0, 10000)) +
    scale_y_continuous(limits = c(-1000, 9000)) +
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
    xlab("Gross Income per Month") +
    ylab("Euro per Month") +
    scale_x_continuous(limits = c(0, 7000)) +
    scale_y_continuous(limits = c(-1000, 4000)) +
    scale_colour_discrete(name = "Legend:",
                          labels = c("Net Income", "Tax Net of Transfers"))

  print(figure_net_income_reduced)
  ggsave(figure_net_income_reduced, filename = "figure_net_income_reduced.pdf", device = pdf, path = "./plots/",  width = 7.6, height = 5)



  # Keep the relevant information for each of the Figures:
  figure_averge_marginal_tax_data <- taxes_and_transfers %>% select(gross_income_monthly, net_income_monthly)
  figure_averge_marginal_tax_data$average_tax_rate <- 1 - figure_averge_marginal_tax_data$net_income_monthly /
    figure_averge_marginal_tax_data$gross_income_monthly
  figure_averge_marginal_tax_data$marginal_tax_rate <- sapply(figure_averge_marginal_tax_data$gross_income_monthly * 12,
                                                              getMarginalTaxRate,
                                                              inculde_welfare_benefits_fraction = inculde_welfare_benefits_fraction,
                                                              income_fraction_of_pension_contribution = income_fraction_of_pension_contribution)

  figure_averge_marginal_tax_data <- figure_averge_marginal_tax_data %>% select(-net_income_monthly)

  figure_averge_marginal_tax_data <- gather(data = figure_averge_marginal_tax_data, key = "marginal_average", value = "tax_rate", -gross_income_monthly)

  figure_averge_marginal_tax <- ggplot(aes(x = gross_income_monthly, y = tax_rate, color = marginal_average), data = figure_averge_marginal_tax_data) +
    geom_line() +
    theme_modified_minimal() +
    scale_x_continuous(limits = c(0, 7000)) +
    scale_y_continuous(limits = c(0, 1.2)) +
    scale_colour_discrete(name = "Legend:", labels = c("Average Tax Rate", "Marginal Tax Rate")) +
    xlab("Gross Income per Month") +
    ylab("Tax Rate")



  print(figure_averge_marginal_tax)
  ggsave(figure_averge_marginal_tax, filename = "marginal_and_average_taxrate.pdf", device = pdf, path = "./plots/", width = 7.6, height = 5)

}

discountVector <- function(periods) {
  # Returns a vector that contains the discount factor for all periods
  return((1/(1 + discount_rate))^(0:(periods-1)))
}

discount <- function(from, to) {
  # Returns the factor that some cash flow in year "from" has to be multiplied by to get the discounted value in year "to"
  return(1/(1+discount_rate)^(from - to))
}

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

getCollegeCostInformation <- function(year, state_token, prices_year) {
  # Returns the average cost per student for a given year and a given state
  # prices_year can be set to return the college cost in 'prices_year' dollar. By default the prices are in 'year' euro.

  if (missing(prices_year)) {
    prices_year <- year
  }

  if(!exists("college_costs")) {
    college_costs <<- read.csv(file = "./college_costs/college_costs_per_state.csv")
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

costOfSchool <- function(duration_of_schooling,
                         year,
                         school_type = "all_schools",
                         prices_year) {

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

  available_years <- school_costs$year

  if (year %in% available_years) {
    return(deflate(from = year, to = prices_year) * school_costs %>%
      filter(year == !!year) %>%
      select(!!school_type))  %>%
      unlist()
  }
  else {
    # If no data is available for a given year return the closest year adjusted for prices to the year of interest
    closest_year <- available_years[which(abs(available_years - year) == min(abs(available_years - year)))]
    return(deflate(from = closest_year,to = prices_year) * school_costs %>%
      filter(year == closest_year) %>%
      select(!!school_type))  %>%
      unlist()
  }
}

getEducationEffectOnEarnings <- function(education_decision = "university_degree",
                                         alternative = "vocational_educ",
                                         assume_constant_effect_from) {

  # Returns a matrix which contains the relative earnings differences between education_decision  and alternative.
  # To be used in conjunction with project_lifetime_impact and the impact_magnitude_matrix option.

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
  # years 7 to 14 after college enrollment into the future.

  # Effect

  if (!exists("age_income_degree_table")) {
    age_income_degree_table <<- read.csv("./college_costs/age_income_degree.csv")
  }

  # Construct the impact for all ages:
  impact_magnitude_matrix <- data.frame(age = age_income_degree_table$age)
  impact_magnitude_matrix$impact_magnitude <- age_income_degree_table[, education_decision] / age_income_degree_table[, alternative] -1


  if (!missing(assume_constant_effect_from)) {
    impact_magnitude_matrix <- impact_magnitude_matrix %>% filter(age <= assume_constant_effect_from)
  }

  # Remove ages where both education paths have zero income, i.e. there is a 0 by 0 divison.
  impact_magnitude_matrix <- impact_magnitude_matrix %>% filter(!is.nan(impact_magnitude))

  # If the education decision results is zero income, and the alternative implies a positive income, the impact_magnitude
  # would be infinity. But what this acutally means is that the alternative gets a zero income and and the education
  # decision gets income as in the data. -> The impact should be 1.
  # Maybe it is wise to avoid this problem. And
  impact_magnitude_matrix[impact_magnitude_matrix$impact_magnitude == Inf, "impact_magnitude"] <- 1


  return(impact_magnitude_matrix)
}

getAverageIncome <- function(age, education, year) {
  if (!exists("age_income_degree_table")) {
    age_income_degree_table <<- read.csv("./college_costs/age_income_degree.csv")
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

returnsToSchool <- function(effect, schooltrack = "all") {
  # Returns the value of an additional year of schooling.
  # Estimates:
  # Estimating returns to schooling is difficult. OLS is likely biased. The direction is not clear. IV Studies find vastly different
  # results depending on the instrument.
  # Pischke and von Wachter (OLS) estimate a slightly modified Mincerian Equation using Mikrozensus ~939736 observations:
  # p. 595 Table 2. It is not clear whether this is a particularly good estimate but it falls in the region where most estimates are in.
  # (7% - 10%)
  pischke_von_wachter_estimate <- 0.074
}

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