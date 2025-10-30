MBBo.DirOut <- function(x, l = 4, nb = 200, ns = 0.99, dfunc = "random_projections", ...) {
  # This function implements the moving blocks bootstrap procedure for DirOut
  # outlier detection
  # params:
  #   x: Functional data object to analyze
  #   l: Block size for sampling
  #   nb: Number of bootstrap samples to generate
  #   ns: Quantile used for cutoff estimation
  #   dfunc: Depth function to use ("RP", "MhD", "SD", or "HS")
  # returns:
  #   list containing:
  #     q_avr: Vector of cutoff values for average directional outlyingness
  #     q_var: Vector of cutoff values for variance of directional outlyingness

  # Get number of functional data objects and block size
  n <- nrow.fdata(x)
  k <- ceiling(n / l)

  # Get data matrix
  trimmed_sample <- x[["data"]]

  # Initialize indices for block sampling
  i <- 1:(n - l + 1)

  # Initialize vectors for quantiles
  cuantiles.avr <- numeric(nb)
  cuantiles.var <- numeric(nb)

  # Loop through bootstrap samples
  for (j in 1:nb) {
    # Sample indices with replacement with a discrete uniform distribution
    idx_sample <- runifdisc(k, min = min(i), max = max(i))

    # Initialize vector for indices
    sample_b_idx <- c()

    # Generate blocks of indices
    for (m in 1:k) {
      sample_b_idx <- c(sample_b_idx, block_i_generator(idx_sample[m], l = l))
    }

    # Keep only the first n indices
    sample_b_idx <- sample_b_idx[1:n]
    sample_b <- trimmed_sample[sample_b_idx, ]

    # Calculate directional outlyingness measures
    dir_out_result <- dir_out(sample_b, data_depth = dfunc)
    d.avr <- dir_out_result$mean_outlyingness
    d.var <- dir_out_result$var_outlyingness

    # Calculate quantiles
    cuantiles.avr[j] <- quantile(d.avr, probs = ns, type = 8)
    cuantiles.var[j] <- quantile(d.var, probs = ns, type = 8)
  }
  return(list(q_avr = cuantiles.avr, q_var = cuantiles.var))
}

StBo_DirOut <- function(
    x, p = 0.1, nb = 200, dfunc = "random_projections", ns = 0.01, ...) {
  # This function implements the stationary bootstrap procedure for DirOut
  # outlier detection
  # params:
  #   x: Functional data object to analyze
  #   p: Success probability for geometric distribution
  #   nb: Number of bootstrap samples to generate
  #   dfunc: Depth function to use ("RP", "MhD", "SD", or "HS")
  #   ns: Quantile used for cutoff estimation
  # returns:
  #   list containing:
  #     q_avr: Vector of cutoff values for average directional outlyingness
  #     q_var: Vector of cutoff values for variance of directional outlyingness

  # Get dimensions and data
  n <- nrow.fdata(x)
  trimmed_sample <- x[["data"]]

  # Initialize indices for sampling
  i <- 1:n
  min <- 1
  max <- n

  # Initialize vectors for quantiles
  cuantiles.avr <- numeric(nb)
  cuantiles.var <- numeric(nb)

  # Loop through bootstrap samples
  for (j in 1:nb) {
    # Generate geometric lengths for blocks
    lengths <- c()
    i <- 1
    while (sum(lengths) < n) {
      lengths[i] <- rgeom(1, prob = p) + 1
      i <- i + 1
    }

    # Get length of lengths vector
    l.ls <- length(lengths)

    # Sample indices with replacement with a discrete uniform distribution
    idx_sample <- runifdisc(l.ls, min = min, max = max)

    # Generate blocks using stationary bootstrap
    b_i <- c()
    for (m in 1:l.ls)
    {
      b_i <- c(b_i, block_StBo_generator(idx_sample[m], n, lengths[m]))
    }

    # Extract bootstrap sample
    sample_b <- trimmed_sample[b_i, ]

    # Calculate directional outlyingness measures
    dir_out_result <- dir_out(sample_b, data_depth = dfunc)
    d.avr <- dir_out_result$mean_outlyingness
    d.var <- dir_out_result$var_outlyingness

    # Calculate quantiles
    cuantiles.avr[j] <- quantile(d.avr, probs = ns, type = 8)
    cuantiles.var[j] <- quantile(d.var, probs = ns, type = 8)
  }
  return(list(q_avr = cuantiles.avr, q_var = cuantiles.var))
}


multiMBBo.DirOut <- function(x, l = 4, nb = 200, ns = 0.99, dfunc = "random_projections", ...) {
  # This function implements the moving blocks bootstrap procedure for DirOut
  # outlier detection
  # params:
  #   x: Array to analyze
  #   l: Block size for sampling
  #   nb: Number of bootstrap samples to generate
  #   ns: Quantile used for cutoff estimation
  #   dfunc: Depth function to use ("RP", "MhD", "SD", or "HS")
  # returns:
  #   list containing:
  #     q_avr: List of vectors of cutoff values for average directional outlyingness
  #     q_var: Vector of cutoff values for variance of directional outlyingness

  # Get number of observations and block size
  n <- dim(x)[1]
  k <- ceiling(n / l)
  p <- dim(x)[3] # Number of variables

  # Initialize indices for block sampling
  i <- 1:(n - l + 1)

  # Initialize vectors for quantiles
  cuantiles.avr <- vector("list", p)
  for (v in 1:p) {
    cuantiles.avr[[v]] <- numeric(nb)
  }
  cuantiles.var <- numeric(nb)

  # Loop through bootstrap samples
  for (j in 1:nb) {
    # Sample indices with replacement with a discrete uniform distribution
    idx_sample <- runifdisc(k, min = min(i), max = max(i))

    # Initialize vector for indices
    sample_b_idx <- c()

    # Generate blocks of indices
    for (m in 1:k) {
      sample_b_idx <- c(sample_b_idx, block_i_generator(idx_sample[m], l = l))
    }

    # Keep only the first n indices
    sample_b_idx <- sample_b_idx[1:n]
    sample_b <- x[sample_b_idx, , ]

    # Calculate directional outlyingness measures
    dir_out_result <- dir_out(sample_b, data_depth = dfunc)

    # Handle multiple average outlyingness values
    for (v in 1:p) {
      d.avr <- dir_out_result$mean_outlyingness[, v]
      cuantiles.avr[[v]][j] <- quantile(d.avr, probs = ns, type = 8)
    }

    # Calculate variance quantile
    d.var <- dir_out_result$var_outlyingness
    cuantiles.var[j] <- quantile(d.var, probs = ns, type = 8)
  }
  return(list(q_avr = cuantiles.avr, q_var = cuantiles.var))
}
