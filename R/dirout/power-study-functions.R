calculate_dirout_rates <- function(
    rho, 
    k = 10,
    model = magnitude,
    method = outlier_dirout,
    M = 100,
    dfunc = "RP",
    boot = MBBo.DirOut,
    ...) {
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
  cut_off_vector_avr <- NULL
  cut_off_vector_var <- NULL
  for (l in 1:M) {
    # Generate data
    fit <- model(rho = rho, k = k, plot = FALSE)
    fdataobj <- fit$fdataobj
    ndata <- nrow.fdata(fdataobj)

    # Get generated outliers
    generated_outliers <- fit$outliers

    # Detect outliers
    resultado <- method(fdataobj, dfunc = dfunc, boot = boot)

    # Get detected outliers
    detected_outliers <- resultado$outliers

    # Get estimated cutoff
    cut_off_vector_avr[l] <- resultado$q_avr
    cut_off_vector_var[l] <- resultado$q_var

    # Initialize variables
    false_positive_rate <- 0
    true_positive_rate <- 0

    # Identify if the detected outliers are generated outliers
    if (length(detected_outliers) != 0) {
      # For each detected outlier, check if it matches a generated outlier
      is_true_positive <- detected_outliers %in% generated_outliers

      # Count true and false positives
      true_positive_count <- sum(is_true_positive)
      false_positive_count <- sum(!is_true_positive)

      # Calculate the false positive and true positive rates
      denominator <- ndata - length(generated_outliers)
      false_positive_rate <- false_positive_count / denominator
      true_positive_rate <- true_positive_count / length(generated_outliers)

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
  cutoff_avr <- mean(cut_off_vector_avr)
  cutoff_var <- mean(cut_off_vector_var)


  return(list(
    false_positive_rate = false_positive_rate,
    true_positive_rate = true_positive_rate,
    sd_true_positive_rate = sd_true_positive_rate,
    true_positive_rate_zero_clean = true_positive_rate_zero_clean,
    cut_off_avr = cutoff_avr,
    cut_off_var = cutoff_var
  ))
}

calculate_multivariate_dirout_rates <- function(
    rho, 
    k = 10,
    method = outlier_dirout,
    M = 100,
    dfunc = "RP",
    boot = MBBo.DirOut,
    model = shape,
    ...) {
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
  cut_off_vector_avr <- NULL
  cut_off_vector_var <- NULL
  for (l in 1:M) {
    # Generate data
    fit <- multifdata(rho = rho, k = k, plot = FALSE)
    fdataobj <- fit$mfdataobj

    # Get number of curves
    ndata <- nrow(fdataobj[[1]])

    # Get generated outliers
    generated_outliers <- fit$outliers

    # Detect outliers
    resultado <- method(fdataobj, dfunc = dfunc, boot = boot)

    # Get detected outliers
    detected_outliers <- resultado$outliers

    # Get estimated cutoff
    cut_off_vector_avr[l] <- resultado$q_avr
    cut_off_vector_var[l] <- resultado$q_var

    # Initialize variables
    false_positive_rate <- 0
    true_positive_rate <- 0

    # Identify if the detected outliers are generated outliers
    if (length(detected_outliers) != 0) {
      # For each detected outlier, check if it matches a generated outlier
      is_true_positive <- detected_outliers %in% generated_outliers

      # Count true and false positives
      true_positive_count <- sum(is_true_positive)
      false_positive_count <- sum(!is_true_positive)

      # Calculate the false positive and true positive rates
      denominator <- ndata - length(generated_outliers)
      false_positive_rate <- false_positive_count / denominator
      true_positive_rate <- true_positive_count / length(generated_outliers)

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
  cutoff_avr <- mean(cut_off_vector_avr)
  cutoff_var <- mean(cut_off_vector_var)


  return(list(
    false_positive_rate = false_positive_rate,
    true_positive_rate = true_positive_rate,
    sd_true_positive_rate = sd_true_positive_rate,
    true_positive_rate_zero_clean = true_positive_rate_zero_clean,
    cut_off_avr = cutoff_avr,
    cut_off_var = cutoff_var
  ))
}