calculate_rates_multivariate <- function(
    rho,
    k = 4,
    model = shape,
    method = outlier_multivariate_bootstrap,
    M = 100,
    dfunc = multiMBD,
    boot = multiMBBo,
    weights = c(1 / 3, 1 / 3, 1 / 3),
    dim = 3) {
  # This function calculates the error ratios for the multivariate model
  # params:
  #   rho: Correlation parameter
  #   k: Contamination level
  #   model: Function that generates the contaminated data
  #   method: Outlier detection procedure
  #   M: Number of simulations
  #   dfunc: Functional depth to use
  #   boot: Bootstrap procedure to estimate the cutoff
  #   weights: Weights to assign for each univariate process in the
  #     multivariate process
  #   dim: Number of univariate processes
  # returns:
  #   list containing:
  #     pf: Mean false positive rate
  #     pc: Mean true positive rate
  #     sd: Standard deviation of true positive rate
  #     pdc: Mean true positive rate when data not cleaned
  #     cutoff: Mean estimated cutoff value

  # Initialize variables
  false_positive_vector <- NULL
  true_positive_vector <- NULL
  cutoff_vector <- NULL

  # Loop through simulations
  for (l in 1:M) {
    # Generate multivariate data
    fit <- multifdata(rho = rho, k = k, model = model, plot = FALSE, dim = dim)
    fdataobj <- fit$mfdataobj

    # Get observed outliers
    generated_outliers <- fit$outliers

    # Detect outliers
    resultado <- method(fdataobj, dfunc = dfunc, boot = boot, weights = weights)

    # Get detected outliers
    detected_outliers <- as.numeric(unique(resultado$outliers))

    # Get cutoff
    cutoff_vector[l] <- resultado$quantile

    false_positive_rate <- 0
    true_positive_rate <- 0
    if (length(detected_outliers) > 0) {
      # Count true and false positives
      denominator <- 200 - length(generated_outliers)
      is_true_positive <- detected_outliers %in% generated_outliers
      true_positive_rate <- sum(is_true_positive) / length(generated_outliers)
      false_positive_rate <- sum(!is_true_positive) / denominator

      # Store rates for this simulation
      false_positive_vector[l] <- false_positive_rate
      true_positive_vector[l] <- true_positive_rate
    } else {
      # No outliers detected
      false_positive_vector[l] <- 0
      true_positive_vector[l] <- 0
    }
  }

  # Calculate the mean false positive and true positive rates
  false_positive_rate <- mean(false_positive_vector)
  true_positive_rate_zero_clean <- mean(true_positive_vector != 0)
  true_positive_rate <- mean(true_positive_vector)
  sd_true_positive_rate <- sd(true_positive_vector)
  cutoff <- mean(cutoff_vector)

  return(list(
    false_positive_rate = false_positive_rate,
    true_positive_rate = true_positive_rate,
    sd_true_positive_rate = sd_true_positive_rate,
    true_positive_rate_zero_clean = true_positive_rate_zero_clean,
    cutoff = cutoff
  ))
}
