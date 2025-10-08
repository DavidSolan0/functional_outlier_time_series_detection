uncontaminated_rates <- function(
    rho,
    method = outlier_bootstrap,
    boot = SmBoD,
    M = 100,
    dfunc = MBD,
    ns = 0.01) {
  # This function calculates the error ratios for the uncontaminated model
  # params:
  #   rho: Correlation parameter
  #   method: Outlier detection procedure
  #   boot: Bootstrap procedure to estimate the cutoff
  #   M: Number of simulations
  #   dfunc: Functional depth to use
  #   ns: Quantile to use in each bootstrap iteration to estimate the cutoff
  # returns:
  #   list containing:
  #     false_positive_rate: Mean false positive rate
  #     false_positive_vector: Vector of false positive rates for
  #       each simulation

  # Initialize positive false rate vector
  false_positive_vector <- NULL
  for (l in 1:M) {
    x <- uncontaminated(rho = rho, plot = FALSE)

    # Calculate depths and clean outliers
    depths <- dfunc(x[["data"]])
    fdataobj <- clean_outliers(x, depths)

    # Detect outliers
    detected_outliers <- method(fdataobj, dfunc = dfunc, boot = boot)$outliers

    # Calculate positive false rate
    false_positive_vector[l] <- ifelse(
      length(detected_outliers) != 0,
      length(detected_outliers) / 200,
      0
    )
  }

  # Calculate mean positive false rate
  false_positive_rate <- mean(false_positive_vector)

  # Return positive false rate and vector of positive false rates
  return_list <- list(
    false_positive_rate = false_positive_rate,
    false_positive_vector = false_positive_vector
  )
  return(return_list)
}

calculate_rates <- function(
    rho, k = 10,
    model = magnitude,
    method = outlier_bootstrap,
    M = 100,
    dfunc = MBD,
    boot = SmBoD) {
  # This function calculates the error ratios for the contaminated models
  # params:
  #   rho: Correlation parameter
  #   k: Contamination level
  #   model: Function that generates the contaminated data
  #   method: Outlier detection procedure
  #   M: Number of simulations
  #   dfunc: Functional depth to use
  #   boot: Bootstrap procedure to estimate the cutoff
  # returns:
  #   list containing:
  #     false_positive_rate: Mean false positive rate
  #     true_positive_rate: Mean true positive rate
  #     sd_true_positive_rate: Standard deviation of true positive rate
  #     true_positive_rate_zero_clean: Mean true positive rate when data
  #       not cleaned
  #     cut_off: Mean estimated cutoff value

  # Initialize variables
  false_positive_vector <- NULL
  true_positive_vector <- NULL
  cut_off_vector <- NULL
  for (l in 1:M) {
    # Generate data
    fit <- model(rho = rho, k = k, plot = FALSE)
    fdataobj <- fit$fdataobj

    # Get generated outliers
    generated_outliers <- fit$outliers

    # Detect outliers
    resultado <- method(fdataobj, dfunc = dfunc, boot = boot)

    # Get detected outliers
    detected_outliers <- resultado$outliers

    # Get estimated cutoff
    cut_off_vector[l] <- resultado$quantile

    # Initialize variables
    false_positive_rate <- 0
    true_positive_rate <- 0

    # Identify if the detected outliers are generated outliers
    if (length(detected_outliers) != 0) {
      # For each detected outlier, check if it matches a generated outlier
      is_true_positive <- detected_outliers %in% generated_outliers

      # Count true and false positives
      true_positive_rate <- sum(is_true_positive)
      false_positive_rate <- sum(!is_true_positive)

      # Calculate the false positive and true positive rates
      denominator <- 200 - length(generated_outliers)
      false_positive_rate <- false_positive_rate / denominator
      true_positive_rate <- true_positive_rate / length(generated_outliers)

      # Add the false positive and true positive rates to the vectors
      false_positive_vector[l] <- false_positive_rate
      true_positive_vector[l] <- true_positive_rate
    } else {
      # If there are no detected outliers, set the false positive
      # and true positive rates to 0
      false_positive_vector[l] <- 0
      true_positive_vector[l] <- 0
    }
  }

  # Calculate the mean false positive and true positive rates
  false_positive_rate <- mean(false_positive_vector)

  # Calculate the mean true positive rate when the data is not cleaned
  true_positive_rate_zero_clean <- mean(true_positive_vector != 0)
  true_positive_rate <- mean(true_positive_vector)

  # Calculate the standard deviation of the true positive rate
  sd_true_positive_rate <- sd(true_positive_vector)

  # Calculate the mean cutoff
  cutoff <- mean(cut_off_vector)


  return(list(
    false_positive_rate = false_positive_rate,
    true_positive_rate = true_positive_rate,
    sd_true_positive_rate = sd_true_positive_rate,
    true_positive_rate_zero_clean = true_positive_rate_zero_clean,
    cut_off = cutoff
  ))
}


# calculate_rates_full_contamination <- function(
#     rho, k1 = 10, k2 = 4, k3 = 10, method = outlier_bootstrap, M = 100, dfunc = MBD,
#     boot = MBBo) {
#   # This function calculates the error ratios for the contaminated model with
#   # all types of contamination
#   # params:
#   #   rho: Correlation parameter
#   #   k1: First contamination level
#   #   k2: Second contamination level
#   #   k3: Third contamination level
#   #   method: Outlier detection procedure
#   #   M: Number of simulations
#   #   dfunc: Functional depth to use
#   #   boot: Bootstrap procedure to estimate the cutoff
#   # returns:
#   #   list containing:
#   #     false_positive_rate: Mean false positive rate
#   #     true_positive_rate: Mean true positive rate
#   #     sd_true_positive_rate: Standard deviation of true positive rate
#   #     true_positive_rate_zero_clean: Mean true positive rate when data
#   #       not cleaned
#   #     cut_off: Mean estimated cutoff value

#   # Initialize variables
#   false_positive_vector <- NULL
#   true_positive_vector <- NULL
#   cut_off_vector <- NULL
#   for (l in 1:M) {
#     # Generate data
#     fit <- f_data(0.8, k1 = k1, k2 = k2, k3 = k3, plot = FALSE)
#     fdataobj <- fit$fdataobj

#     # Get generated outliers
#     generated_outliers <- fit$outliers

#     # Detect outliers
#     resultado <- method(fdataobj, dfunc = dfunc, boot = boot)

#     # Get detected outliers
#     detected_outliers <- resultado$outliers

#     # Get estimated cutoff
#     cut_off_vector[l] <- resultado$quantile

#     # Initialize variables
#     false_positive_rate <- 0
#     true_positive_rate <- 0

#     # Identify if the detected outliers are generated outliers
#     if (length(detected_outliers) != 0) {
#       # For each detected outlier, check if it matches a generated outlier
#       is_true_positive <- detected_outliers %in% generated_outliers

#       # Count true and false positives
#       true_positive_rate <- sum(is_true_positive)
#       false_positive_rate <- sum(!is_true_positive)

#       # Calculate the false positive and true positive rates
#       denominator <- nrow.fdata(fdataobj) - length(generated_outliers)
#       false_positive_rate <- false_positive_rate / denominator
#       true_positive_rate <- true_positive_rate / length(generated_outliers)

#       # Add the false positive and true positive rates to the vectors
#       false_positive_vector[l] <- false_positive_rate
#       true_positive_vector[l] <- true_positive_rate
#     } else {
#       # If there are no detected outliers, set the false positive
#       # and true positive rates to 0
#       false_positive_vector[l] <- 0
#       true_positive_vector[l] <- 0
#     }
#   }

#   # Calculate the mean false positive and true positive rates
#   false_positive_rate <- mean(false_positive_vector)

#   # Calculate the mean true positive rate when the data is not cleaned
#   true_positive_rate_zero_clean <- mean(true_positive_vector != 0)
#   true_positive_rate <- mean(true_positive_vector)

#   # Calculate the standard deviation of the true positive rate
#   sd_true_positive_rate <- sd(true_positive_vector)

#   # Calculate the mean cutoff
#   cutoff <- mean(cut_off_vector)


#   return(list(
#     false_positive_rate = false_positive_rate,
#     true_positive_rate = true_positive_rate,
#     sd_true_positive_rate = sd_true_positive_rate,
#     true_positive_rate_zero_clean = true_positive_rate_zero_clean,
#     cut_off = cutoff
#   ))
# }
