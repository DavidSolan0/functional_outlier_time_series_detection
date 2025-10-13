#* Outlier Bootstrap
outlier_bootstrap <- function(
    fdataobj,
    nb = 200,
    smo = 0.05,
    quan = 0.5,
    dfunc = MBD,
    l = 4,
    p = 0.1,
    ns = 0.01,
    boot = SmBoD) {
  # This function implements outlier detection using bootstrap procedures
  # params:
  #   fdataobj: Functional data object to analyze
  #   nb: Number of bootstrap samples to generate
  #   smo: Smoothness parameter for adding noise. Only used for
  #     SmBoD bootstrap.
  #   quan: Quantile used for cutoff estimation from bootstrap
  #   dfunc: Depth function to use for outlier detection
  #   l: Block size (only used for MBBo bootstrap)
  #   p: Success probability for geometric distribution (only used for StBo)
  #   ns: Quantile to calculate in each bootstrap iteration
  #   boot: Bootstrap procedure to use (SmBoD, MBBo, or StBo)
  # returns:
  #   list containing:
  #     outliers: Vector of detected outlier indices
  #     dep.out: Vector of depth values for outliers
  #     iteration: Vector indicating iteration when each outlier was found
  #     quantile: Estimated cutoff value
  #     Dep: Vector of depth values for all curves

  # Check if fdataobj is a functional data object
  if (!is.fdata(fdataobj)) {
    fdataobj <- fdata(fdataobj)
  }

  # Check if fdataobj contains any NA values
  nas1 <- is.na(fdataobj)
  if (any(nas1)) {
    stop("fdataobj contain ", sum(nas1), " curves with some NA value \n")
  }

  # Extract data, arguments, and range values
  n <- nrow(fdataobj)
  m <- ncol(fdataobj)
  if (is.null(n) && is.null(m)) {
    stop("ERROR IN THE DATA DIMENSIONS")
  }
  if (is.null(row.names(fdataobj[["data"]]))) {
    row.names(fdataobj[["data"]]) <- 1:n
  }

  # Calculate cutoff using the bootstrap procedure
  bootstrap_sample <- boot(fdataobj, dfunc = dfunc, nb = nb, smo = smo, ns = ns)
  cutoff <- quantile(bootstrap_sample, probs = quan)

  # Initialize variables
  hay <- 1
  outliers <- dep.out <- ite <- c()
  ii <- 1
  curvasgood <- fdataobj

  # Calculate depths
  d <- dfunc(curvasgood[["data"]])

  # Loop through the data
  while (hay == 1) {
    # Check if the depth is less than the cutoff
    cutt <- d < cutoff

    # Get the indices of the outliers
    fecha <- rownames(curvasgood[["data"]])[cutt]
    elim <- which(cutt)

    # Check if there are any outliers
    if (length(elim) > 0) {
      # Add the depths of the outliers
      dep.out <- c(dep.out, d[elim])

      # Remove the outliers from the data
      curvasgood <- curvasgood[-elim, ]

      # Add the indices of the outliers
      outliers <- c(outliers, fecha)
    }

    # Check if there are no outliers or if there are more than 20% of the data
    if (length(elim) == 0 || length(outliers) > n / 5) {
      hay <- 0
    } else {
      d <- dfunc(curvasgood[["data"]])
    }

    # Add the indices of the iterations
    ite <- c(ite, rep(ii, length(elim)))
    ii <- ii + 1
  }

  # Return the outliers, the depths of the outliers,
  # the indices of the iterations, the cutoff, and the depths
  return(list(
    outliers = outliers, dep.out = dep.out, iteration = ite,
    quantile = cutoff
  ))
}

#* functional_boxplot_outlier_detection_method
functional_boxplot_outlier_detection_method <- function(
    fdataobj, dfunc = MBD, ...) {
  # This function implements outlier detection using functional boxplots
  # params:
  #   fdataobj: Functional data object to analyze
  #   dfunc: Depth function to use for outlier detection
  # returns:
  #   list containing:
  #     outliers: Vector of detected outlier indices

  # Ensure input is functional data object
  if (!is.fdata(fdataobj)) {
    fdataobj <- fdata(fdataobj)
  }

  # Initialize variables
  data <- fdataobj[["data"]]
  all_outliers <- c()
  has_outliers <- TRUE

  while (has_outliers && nrow(data) > 0) {
    # Calculate depths and find outliers
    depths <- dfunc(data)
    fbplot_result <- fda::fbplot(t(data), plot = FALSE, depth = depths)
    current_outliers <- fbplot_result$outpoint

    if (length(current_outliers) == 0) {
      has_outliers <- FALSE
    } else {
      # Update outliers list and remove from data
      all_outliers <- c(all_outliers, current_outliers)
      data <- data[-current_outliers, ]
    }
  }

  return(list(outliers = all_outliers))
}
