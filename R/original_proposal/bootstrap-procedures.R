SmBoD <- function(x, dfunc = MBD, nb = 200, smo = 0.05, ns = 0.01) {
  # This function implements the smooth bootstrap procedure for outlier detection
  # params:
  #   x: Functional data object to analyze
  #   dfunc: Depth function to use for outlier detection
  #   nb: Number of bootstrap samples to generate
  #   smo: Smoothness parameter for adding noise
  #   ns: Quantile used for cutoff estimation
  # returns:
  #   cuantiles: Vector of cutoff values from bootstrap samples

  # Check if x is a functional data object
  if (!is.fdata(x)) {
    x <- fdata(x)
  }

  # Extract data, arguments, and range values
  dat <- x[["data"]]
  tt <- x[["argvals"]]
  rtt <- x[["rangeval"]]
  n <- nrow(x)
  m <- ncol(x)

  # Check if data dimensions are valid
  if (is.null(n) && is.null(m)) {
    stop("ERROR IN THE DATA DIMENSIONS")
  }

  # Calculate depths and trim the sample by removing outliers
  # using naive approach (fbplot)
  depths <- dfunc(x[["data"]])
  trimmed_sample <- clean_outliers(x, depths)
  nn <- nrow(trimmed_sample)

  # Initialize vector for quantiles
  cuantiles <- numeric(nb)

  # Calculate variance of data
  vv <- var(dat)

  # Loop through bootstrap samples
  for (i in 1:nb) {
    # Sample indices with replacement
    sample_index <- sample(1:nn, size = n, replace = TRUE)
    sample_b <- trimmed_sample[sample_index, ]

    # Add noise to the sample
    mvnorm_noise <- mvrnorm(n = n, rep(0, m), vv * smo)
    sample_b[["data"]] <- sample_b[["data"]] + mvnorm_noise

    # Calculate depths
    d <- dfunc(sample_b[["data"]])

    # Calculate quantile
    cuantiles[i] <- quantile(d, probs = ns, type = 8)
  }
  return(cuantiles)
}

MBBo <- function(x, l = 4, nb = 200, dfunc = MBD, ns = 0.01, smo = 0) {
  # This function implements the moving blocks bootstrap procedure for outlier detection
  # params:
  #   x: Functional data object to analyze
  #   l: Block size for sampling
  #   dfunc: Depth function to use for outlier detection
  #   nb: Number of bootstrap samples to generate
  #   smo: Smoothness parameter for adding noise
  #   ns: Quantile used for cutoff estimation
  # returns:
  #   cuantiles: Vector of cutoff values from bootstrap samples

  # Get number of functional data objects
  # and block size
  n <- nrow.fdata(x)
  k <- ceiling(n / l)

  # Calculate depths and trim the sample by removing outliers
  # using naive approach (fbplot)
  depths <- dfunc(x[["data"]])
  trimmed_sample <- clean_outliers(x, depths)
  nn <- nrow.fdata(trimmed_sample)

  # Initialize vector for quantiles
  i <- 1:(nn - l + 1)
  cuantiles <- numeric(nb)

  # Loop through bootstrap samples
  for (j in 1:nb) {
    # Sample indices with replacement with a
    # discrete uniform distribution
    idx_sample <- runifdisc(k, min = min(i), max = max(i))

    # Initialize vector for indices
    sample_b_idx <- c()

    # Loop through block size
    for (m in 1:k) {
      sample_b_idx <- c(sample_b_idx, block_i_generator(idx_sample[m], l = l))
    }

    # Keep only the first n indices
    sample_b_idx <- sample_b_idx[1:n]

    # Extract the bootstrap sample
    sample_b <- trimmed_sample[sample_b_idx, ]

    # Calculate depths
    d <- dfunc(sample_b[["data"]])

    # Calculate quantile
    cuantiles[j] <- quantile(d, probs = ns, type = 8)
  }
  return(cuantiles)
}

StBo <- function(x, p = 0.1, nb = 200, dfunc = MBD, ns = 0.01, smo = 0) {
  # This function implements the stationary bootstrap procedure for outlier detection
  # params:
  #   x: Functional data object to analyze
  #   p: Success probability for geometric distribution
  #   dfunc: Depth function to use for outlier detection
  #   nb: Number of bootstrap samples to generate
  #   smo: Smoothness parameter for adding noise
  #   ns: Quantile used for cutoff estimation
  # returns:
  #   cuantiles: Vector of cutoff values from bootstrap samples

  # Get number of functional data objects
  # and block size
  n <- nrow.fdata(x)
  k <- ceiling(n / l)

  # Calculate depths and trim the sample by removing outliers
  # using naive approach (fbplot)
  depths <- dfunc(x[["data"]])
  trimmed_sample <- clean_outliers(x, depths)
  nn <- nrow.fdata(trimmed_sample)

  # Initialize vector for indices
  i <- 1:nn
  min <- 1
  max <- nn
  cuantiles <- numeric(nb)

  # Loop through bootstrap samples
  for (j in 1:nb) {
    # Initialize vector for lengths
    lengths <- c()
    i <- 1

    # Loop through block size
    while (sum(lengths) < n) {
      lengths[i] <- rgeom(1, prob = p) + 1
      i <- i + 1
    }

    # Get length of lengths
    l.ls <- length(lengths)

    # Sample indices with replacement with a
    # discrete uniform distribution
    idx_sample <- runifdisc(l.ls, min = min, max = max)

    # Initialize vector for indices
    block_i <- c()
    for (m in 1:l.ls)
    {
      block_i <- c(block_i, block_StBo_generator(idx_sample[m], nn, lengths[m]))
    }

    # Extract the bootstrap sample
    sample_b <- trimmed_sample[block_i, ]

    # Calculate depths
    d <- dfunc(sample_b[["data"]])

    # Calculate quantile
    cuantiles[j] <- quantile(d, probs = ns, type = 8)
  }
  return(cuantiles)
}
