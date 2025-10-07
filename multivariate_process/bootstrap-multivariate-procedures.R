multiMBBo <- function(
    mfdataobj, l = 4, nb = 200, dfunc = multiMBD, ns = 0.01,
    weights = c(1 / 3, 1 / 3, 1 / 3)) {
  # This function runs a simulation study for outlier detection with
  # different depth functions
  # params:
  #   x: multivariate functional data
  #   l: block size
  #   dfunc: depth function to use
  #   nb: number of bootstrap samples
  #   ns: quantile for cutoff estimation
  #   weights: weights to assign for each univariate process in the 
  #     multivariate process
  # returns:
  #   cuantiles: Vector containing quantiles for each bootstrap sample


  # Get number of functional data objects and block size
  dim <- length(mfdataobj)
  n <- nrow(mfdataobj[[1]])
  k <- ceiling(n / l)

  # Remove outliers and trim the sample
  depths <- dfunc(mfdataobj, weights = weights)
  trimmed_sample <- clean_outliers_multivariate(mfdataobj, depths)

  # Get number of functional data objects and block size
  nn <- nrow(trimmed_sample[[1]])
  i <- 1:(nn - l + 1)

  # Initialize vector for quantiles
  cuantiles <- numeric(nb)

  # Loop through bootstrap samples
  for (j in 1:nb) {
    # Sample indices with replacement with a discrete uniform distribution
    idx_sample <- runifdisc(k, min = min(i), max = max(i))

    # Initialize vector for indices
    sample_b_idx <- c()

    for (m in 1:k) {
      # Generate block indices
      sample_b_idx <- c(sample_b_idx, block_i_generator(idx_sample[m], l = l))
    }

    # Keep only the first n indices
    sample_b_idx <- sample_b_idx[1:n]

    # Initialize vector for multivariate functional data
    sample_b <- list()

    # Loop through multivariate functional data objects
    for (i in 1:dim) sample_b[[i]] <- trimmed_sample[[i]][sample_b_idx, ]

    # Calculate depths
    d <- dfunc(sample_b, weights = weights)

    # Calculate quantile
    cuantiles[j] <- quantile(d, probs = ns, type = 8)
  }

  return(cuantiles)
}
