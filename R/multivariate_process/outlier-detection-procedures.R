outlier_multivariate_bootstrap <- function(
    mfdataobj, nb = 200, smo = 0.05, quan = 0.5,
    dfunc = multiMBD, l = 4, p = 0.1, ns = 0.01, boot = multiMBBo,
    weights = c(1 / 3, 1 / 3, 1 / 3)) {
  # This function performs outlier detection for multivariate functional data using bootstrap
  # params:
  #   mfdataobj: List containing multivariate functional data objects
  #   nb: Number of bootstrap samples
  #   smo: Smoothness parameter
  #   quan: Quantile used to obtain cutoff estimation from bootstrap
  #   dfunc: Depth function to use
  #   l: Block size for moving block bootstrap
  #   p: Success probability for geometric distribution
  #   ns: Quantile calculated in each bootstrap iteration for cutoff
  #   boot: Bootstrap type (SmBoD, MBBo, or StBo)
  #   weights: Vector of weights for each univariate process
  # returns:
  #   list containing:
  #     outliers: Vector of detected outlier indices
  #     outlier_depths: Depths of detected outliers
  #     iteration: Iteration when each outlier was detected
  #     quantile: Estimated cutoff value
  #     Dep: Depth values for all curves

  # Check if mfdataobj is a list
  if (!is.list(mfdataobj)) {
    "object must be a list"
  }

  # Check if row names are present
  if (is.null(row.names(mfdataobj[[1]]))) {
    row.names(mfdataobj[[1]]) <- 1:n
  }

  # Get number of functional data objects and block size
  dim <- length(mfdataobj)
  n <- nrow(mfdataobj[[1]])

  # Calculate cutoff
  bootstrap_sample <- boot(
    mfdataobj,
    dfunc = dfunc, nb = nb, ns = ns, weights = weights
  )
  cutoff <- quantile(bootstrap_sample, probs = quan)

  # Initialize variables
  hay <- 1
  outliers <- outlier_depths <- ite <- c()
  ii <- 1
  curvasgood <- mfdataobj

  # Calculate depths
  functional_depths <- dfunc(curvasgood, weights = weights)

  # Loop through the data
  while (hay == 1) {
    if (is.null(outliers)) {
      dtotal <- functional_depths
    }

    # Check if the depth is less than the cutoff
    cutt <- functional_depths < cutoff
    fecha <- as.numeric(rownames(curvasgood[[1]])[cutt])
    elim <- which(cutt)

    # Check if there are any outliers
    if (length(elim) > 0) {
      outlier_depths <- c(outlier_depths, functional_depths[elim])

      for (i in 1:dim)
      {
        curvasgood[[i]] <- curvasgood[[i]][-elim, ]
      }

      outliers <- c(outliers, fecha)
    }
    if (length(elim) == 0 || length(outliers) > n / 5) {
      hay <- 0
    } else {
      functional_depths <- dfunc(curvasgood, weights = weights)
    }
    ite <- c(ite, rep(ii, length(elim)))
    ii <- ii + 1
  }

  # Get the names of the outliers
  outliers <- rownames(mfdataobj[[1]])[unique(outliers)]
  names(outlier_depths) <- NULL

  # Return the outliers, the depths of the outliers,
  # the indices of the iterations, the cutoff, and the depths
  return(list(
    outliers = outliers, outlier_depths = outlier_depths, iteration = ite,
    quantile = cutoff, Dep = dtotal
  ))
}


#* functional_boxplot_outlier_detection_method
multivariate_functional_boxplot <- function(
    mfdataobj, dfunc = multiMBD, weights = NULL, ...) {
  # This function implements outlier detection using functional boxplots
  # params:
  #   mfdataobj: Functional data object to analyze
  #   dfunc: Depth function to use for outlier detection
  #   weights: Vector of weights for each univariate process
  # returns:
  #   list containing:
  #     outliers: Vector of detected outlier indices

  # Ensure input is multivariate functional data object
  if (!is.list(mfdataobj)) {
    mfdataobj <- list(mfdataobj)
  }

  # Set default weights if not provided
  if (is.null(weights)) {
    weights <- rep(1/length(mfdataobj), length(mfdataobj))
  }

  # Initialize variables
  all_outliers <- c()
  has_outliers <- TRUE
  pct_outliers <- 0

  while (has_outliers && (nrow(mfdataobj[[1]]) > 0)) {
    # Calculate depths and find outliers
    depths <- dfunc(mfdataobj, weights = weights)

    # Initialize vector for current outliers
    current_outliers <- c()

    # Check each component for outliers
    for (i in seq_along(mfdataobj)) {

      # Calculate outliers using functional boxplot
      fbplot_result <- fda::fbplot(
        t(mfdataobj[[i]]),
        plot = FALSE, depth = depths
      )

      # Update vector for outliers
      current_outliers <- unique(c(current_outliers, fbplot_result$outpoint))
    }

    if (length(current_outliers) == 0) {
      has_outliers <- FALSE
    } else {
      # Update outliers list and remove from each component
      all_outliers <- c(all_outliers, current_outliers)
      for (i in seq_along(mfdataobj)) {
        mfdataobj[[i]] <- mfdataobj[[i]][-current_outliers, ]
      }
    }
  }

  return(list(outliers = all_outliers))
}
