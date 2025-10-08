# This function runs the simulation for a given model and depth function
run_simulation <- function(
    K,
    depths,
    depth_names,
    rho = 0.8,
    M = 100,
    seed = NULL,
    model = magnitude,
    model_name = "",
    outlier_detection_method = outlier_bootstrap,
    bootstrap_estimation_method = SmBoD) {
  # This function runs a simulation study for outlier detection with
  # different depth functions
  # params:
  #   K: Vector of contamination levels to test
  #   depths: List of depth functions to evaluate
  #   seed: Random seed for reproducibility (optional)
  #   model: Function that generates the contaminated data
  #   model_name: String identifier for the model being tested
  # returns:
  #   tabla: Matrix containing false positive rates, true positive rates,
  #         standard deviations and clean detection rates for each K value

  tabla <- NULL
  nn <- length(depths)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  for (i in 1:nn) {
    iter <- 1
    tabla_depth <- NULL
    cat(paste("Processing depth function", i, "of", nn, model_name, "\n"))

    for (k in K) {
      cat(paste("  Processing k =", k, "(", iter, "of", length(K), ")\n"))

      rates <- calculate_rates(
        rho,
        k = k,
        model = model,
        dfunc = depths[i][[1]],
        method = outlier_detection_method,
        M = M,
        boot = bootstrap_estimation_method
      )

      vector <- c(
        rates$false_positive_rate,
        rates$true_positive_rate,
        rates$sd_true_positive_rate,
        rates$true_positive_rate_zero_clean
      )

      vector_names <- c(
        "false_positive_rate",
        "true_positive_rate",
        "sd_true_positive_rate",
        "true_positive_rate_zero_clean"
      )

      names(vector) <- paste0(vector_names, "-", k)
      tabla_K <- t(vector)
      tabla_depth <- cbind(tabla_depth, t(vector))

      iter <- iter + 1
    }

    tabla <- rbind(tabla, tabla_depth)
  }

  row.names(tabla) <- depth_names
  tabla
}


# This function runs the simulation for a given model and depth function
run_simulation_multivariate <- function(
    K,
    depths,
    depth_names,
    rho = 0.8,
    M = 100,
    seed = NULL,
    model = magnitude,
    model_name = "",
    outlier_detection_method = outlier_multivariate_bootstrap,
    bootstrap_estimation_method = multiMBBo,
    weights = c(1 / 3, 1 / 3, 1 / 3),
    dim = 3) {
  # This function runs a simulation study for outlier detection with
  # different depth functions
  # params:
  #   K: Vector of contamination levels to test
  #   depths: List of depth functions to evaluate
  #   seed: Random seed for reproducibility (optional)
  #   model: Function that generates the contaminated data
  #   model_name: String identifier for the model being tested
  # returns:
  #   tabla: Matrix containing false positive rates, true positive rates,
  #         standard deviations and clean detection rates for each K value

  tabla <- NULL
  nn <- length(depths)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  for (i in 1:nn) {
    iter <- 1
    tabla_depth <- NULL
    cat(paste("Processing depth function", i, "of", nn, model_name, "\n"))

    for (k in K) {
      cat(paste("  Processing k =", k, "(", iter, "of", length(K), ")\n"))

      rates <- calculate_rates_multivariate(
        rho,
        k = k,
        model = model,
        dfunc = depths[i][[1]],
        method = outlier_detection_method,
        M = M,
        boot = bootstrap_estimation_method,
        weights = weights,
        dim = dim
      )

      vector <- c(
        rates$false_positive_rate,
        rates$true_positive_rate,
        rates$sd_true_positive_rate,
        rates$true_positive_rate_zero_clean
      )

      vector_names <- c(
        "false_positive_rate",
        "true_positive_rate",
        "sd_true_positive_rate",
        "true_positive_rate_zero_clean"
      )

      names(vector) <- paste0(vector_names, "-", k)
      tabla_K <- t(vector)
      tabla_depth <- cbind(tabla_depth, t(vector))

      iter <- iter + 1
    }

    tabla <- rbind(tabla, tabla_depth)
  }

  row.names(tabla) <- depth_names
  tabla
}


#* Helper function to run simulation and create table
run_simulation_dirout <- function(K, model = magnitude, M = 100, seed = NULL) {
  # This function runs a simulation study for outlier detection with
  # different depth functions and using DirOut method
  # params:
  #   K: Vector of contamination levels to test
  #   model: Function that generates the contaminated data
  #   M: Number of simulations
  #   seed: Random seed for reproducibility (optional)
  # returns:
  #   tabla: Matrix containing false positive rates, true positive rates,
  #         standard deviations and clean detection rates for each K value

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Initialize table
  tabla_depth <- NULL

  for (k in K) {
    # Run simulation with DirOut method
    rates <- tasas.DirOut(0.8, model = model, k = k, M = M)

    # Create vector of results
    # pf: False positive rate
    # pc: True positive rate
    # sd: Standard deviation of true positive rate
    # pdc: Clean detection rate
    vector <- c(rates$pf, rates$pc, rates$sd, rates$pdc)
    names(vector) <- paste0(c("pf", "pc", "sd", "pdc"), "-", k)

    # Build table
    # If table is empty, create it
    if (is.null(tabla_depth)) {
      tabla_depth <- t(vector)
    } else {
      # If table is not empty, add results to it
      tabla_depth <- cbind(tabla_depth, t(vector))
    }
  }

  # Format final table
  # Add row names
  # Convert table to data frame
  tabla <- tabla_depth
  row.names(tabla) <- "DirOut"
  tabla <- tabla %>% data.frame()

  return(tabla)
}