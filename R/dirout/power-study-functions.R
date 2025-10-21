calculate_dirout_rates <- function(
    rho,
    k = 10,
    model = magnitude,
    method = outlier_dirout,
    M = 100,
    dfunc = "RP",
    boot = MBBo.DirOut,
    multivariate = FALSE,
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
  #   multivariate: Whether to use multivariate data (TRUE) or 
  #     univariate (FALSE)
  # returns:
  #   list containing:
  #     false_positive_rate: Mean false positive rate
  #     true_positive_rate: Mean true positive rate
  #     sd_true_positive_rate: Standard deviation of true positive rate
  #     true_positive_rate_zero_clean: Mean true positive rate when data
  #       not cleaned

  # Initialize variables
  false_positive_vector <- NULL
  true_positive_vector <- NULL

  for (l in 1:M) {
    # Generate data based on multivariate flag
    if (multivariate) {
      fit <- multifdata(rho = rho, model = model, k = k, plot = FALSE)
      mfdata <- fit$mfdataobj

      # Convert list to array format required by multivariate_outlier_dirout
      mfdata <- array(
        unlist(mfdata),
        dim = c(
          dim(mfdata[[1]])[1],
          dim(mfdata[[1]])[2],
          length(mfdata)
        )
      )
      ndata <- dim(mfdata)[1]

      # Detect outliers using multivariate method
      resultado <- method(mfdata, dfunc = dfunc, boot = boot)
    } else {
      # Univariate case
      fit <- model(rho = rho, k = k, plot = FALSE)
      fdataobj <- fit$fdataobj
      ndata <- nrow.fdata(fdataobj)

      # Detect outliers using univariate method
      resultado <- method(fdataobj, dfunc = dfunc, boot = boot)
    }

    # Get generated and detected outliers
    generated_outliers <- fit$outliers
    detected_outliers <- unique(as.numeric(resultado$outliers))

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

  return(list(
    false_positive_rate = false_positive_rate,
    true_positive_rate = true_positive_rate,
    sd_true_positive_rate = sd_true_positive_rate,
    true_positive_rate_zero_clean = true_positive_rate_zero_clean
  ))
}

calculate_sliding_window_dirout_rates <- function(
    rho,
    k = 10,
    model = magnitude,
    window_size = 8,
    M = 100,
    dfunc = "RP",
    boot = SlidingWindow.DirOut,
    multivariate = FALSE,
    ...) {
  # This function calculates the error ratios for the contaminated models
  # using the sliding window approach
  # params:
  #   rho: Correlation parameter
  #   k: Contamination level
  #   model: Function that generates the contaminated data
  #   window_size: Size of the symmetric window (default 8)
  #   M: Number of simulations
  #   dfunc: Functional depth to use
  #   boot: Bootstrap procedure to estimate the cutoff (SlidingWindow.DirOut)
  #   multivariate: Whether to use multivariate data (TRUE) or 
  #     univariate (FALSE)
  # returns:
  #   list containing:
  #     false_positive_rate: Mean false positive rate
  #     true_positive_rate: Mean true positive rate
  #     sd_true_positive_rate: Standard deviation of true positive rate
  #     true_positive_rate_zero_clean: Mean true positive rate when data
  #       not cleaned

  # Initialize variables
  false_positive_vector <- NULL
  true_positive_vector <- NULL

  for (l in 1:M) {
    # Generate data based on multivariate flag
    if (multivariate) {
      fit <- multifdata(rho = rho, model = model, k = k, plot = FALSE)
      mfdata <- fit$mfdataobj

      # Convert list to array format required by multivariate_outlier_dirout
      mfdata <- array(
        unlist(mfdata),
        dim = c(
          dim(mfdata[[1]])[1],
          dim(mfdata[[1]])[2],
          length(mfdata)
        )
      )
      ndata <- dim(mfdata)[1]

      # Detect outliers using multivariate sliding window method
      resultado <- sliding_window_outlier_dirout(
        fdataobj = mfdata, 
        window_size = window_size,
        dfunc = dfunc, 
        boot = boot
      )
    } else {
      # Univariate case
      fit <- model(rho = rho, k = k, plot = FALSE)
      fdataobj <- fit$fdataobj
      ndata <- nrow.fdata(fdataobj)

      # Detect outliers using univariate sliding window method
      resultado <- sliding_window_outlier_dirout(
        fdataobj = fdataobj, 
        window_size = window_size,
        dfunc = dfunc, 
        boot = boot
      )
    }

    # Get generated and detected outliers
    generated_outliers <- fit$outliers
    detected_outliers <- unique(as.numeric(resultado$outliers))

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

  return(list(
    false_positive_rate = false_positive_rate,
    true_positive_rate = true_positive_rate,
    sd_true_positive_rate = sd_true_positive_rate,
    true_positive_rate_zero_clean = true_positive_rate_zero_clean
  ))
}
