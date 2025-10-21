outlier_dirout <- function(
    fdataobj,
    nb = 200,
    quan = 0.5,
    l = 4,
    ns = 0.99,
    dfunc = "RP",
    smo = 0,
    boot = MBBo.DirOut,
    plot = FALSE) {
  # This function implements outlier detection using DirOut
  # params:
  #   fdataobj: Functional data object to analyze
  #   nb: Number of bootstrap samples to generate
  #   quan: Quantile used for cutoff estimation from bootstrap
  #   l: Block size (only used for MBBo bootstrap)
  #   ns: Quantile to calculate in each bootstrap iteration to cutoff estimation
  #   dfunc: Depth function to use
  #   boot: Bootstrap procedure to use (MBBo.DirOut to
  #     moving block for DirOut procedure)
  # returns:
  #   list containing:
  #     outliers: Vector of detected outlier indices
  #     dep.out_avr: Vector of depth values for outliers based on average
  #       directional outlyingness
  #     dep.out_var: Vector of depth values for outliers based on variance of
  #       directional outlyingness
  #     q_avr: Estimated cutoff value for average directional outlyingness
  #     q_var: Estimated cutoff value for variance of directional outlyingness

  # Check if fdataobj is a functional data object
  if (!is.fdata(fdataobj)) {
    fdataobj <- fdata(fdataobj)
  }

  # Check if fdataobj contains any NA values
  nas1 <- is.na(fdataobj)
  if (any(nas1)) {
    stop("fdataobj contain ", sum(nas1), " curves with some NA value \n")
  }

  # Get number of functional data objects and block size
  n <- nrow(fdataobj)
  m <- ncol(fdataobj)
  if (is.null(n) && is.null(m)) {
    stop("ERROR IN THE DATA DIMENSIONS")
  }
  if (is.null(row.names(fdataobj[["data"]]))) {
    row.names(fdataobj[["data"]]) <- 1:n
  }

  # Calculate cutoff
  qs <- boot(fdataobj, nb = nb, ns = ns, dfunc = dfunc, smo = smo)
  cutoff_avr <- quantile(qs$q_avr, probs = quan)
  cutoff_var <- quantile(qs$q_var, probs = quan)

  # Initialize variables
  hay <- 1
  dep.out_avr <- dep.out_var <- c()
  outliers <- c()
  ite <- c()
  ii <- 1
  curvasgood <- fdataobj

  # Calculate DirOut results
  DirOut.Obj <- DirOut(curvasgood[["data"]], depth.dir = dfunc)
  d_avr <- DirOut.Obj$out_avr
  d_var <- DirOut.Obj$out_var

  if (plot == TRUE) {
    plot(d_avr, d_var,
      main = "Directional Outlyingness Plot",
      xlab = "Average Directional Outlyingness",
      ylab = "Variance of Directional Outlyingness",
      col = "steelblue",
      pch = 19,
      cex = 0.8
    )
    grid()
  }

  while (hay == 1) {
    # Calculate outliers based on average directional outlyingness
    cutt <- cutoff_avr < d_avr
    elim_avr <- which(cutt)

    # If there are any outliers, add them to the list
    if (length(elim_avr) > 0) {
      dep.out_avr <- c(dep.out_avr, d_avr[elim_avr])
    }

    # Calculate outliers based on variance of directional outlyingness
    cutt <- cutoff_var < d_var
    elim_var <- which(cutt)

    # If there are any outliers, add them to the list
    if (length(elim_var) > 0) {
      dep.out_var <- c(dep.out_var, d_var[elim_var])
    }

    # Get unique outliers
    elim <- unique(c(elim_avr, elim_var))
    fecha <- rownames(curvasgood[["data"]])[elim]

    # Clean dataset
    curvasgood <- curvasgood[-elim, ]

    # Update outliers
    outliers <- c(outliers, fecha)

    # Check if there are no outliers or if there are more than 20% of the data
    if (length(elim) == 0) {
      hay <- 0
    } else {
      DirOut.Obj <- DirOut(curvasgood[["data"]], depth.dir = dfunc)
      d_avr <- DirOut.Obj$out_avr
      d_var <- DirOut.Obj$out_var
    }
    ite <- c(ite, rep(ii, length(elim)))
    ii <- ii + 1
  }

  # Return the outliers, the depths of the outliers,
  # the indices of the iterations, the cutoff, and the depths
  return(list(
    outliers = outliers,
    dep.out_avr = dep.out_avr,
    dep.out_var = dep.out_var,
    q_avr = cutoff_avr,
    q_var = cutoff_var,
    iteration = ite
  ))
}


multivariate_outlier_dirout <- function(
    fdataobj,
    nb = 200,
    quan = 0.5,
    l = 4,
    ns = 0.99,
    dfunc = "RP",
    smo = 0,
    boot = multiMBBo.DirOut,
    plot = FALSE) {
  # This function implements outlier detection using DirOut
  # params:
  #   fdataobj: Array containing multivariate functional data
  #   nb: Number of bootstrap samples to generate
  #   quan: Quantile used for cutoff estimation from bootstrap
  #   l: Block size (only used for MBBo bootstrap)
  #   ns: Quantile to calculate in each bootstrap iteration to cutoff estimation
  #   dfunc: Depth function to use
  #   boot: Bootstrap procedure to use (MBBo.DirOut to
  #     moving block for DirOut procedure)
  # returns:
  #   list containing:
  #     outliers: Vector of detected outlier indices
  #     dep.out_avr: Vector of depth values for outliers based on average
  #       directional outlyingness
  #     dep.out_var: Vector of depth values for outliers based on variance of
  #       directional outlyingness
  #     q_avr: Estimated cutoff value for average directional outlyingness
  #     q_var: Estimated cutoff value for variance of directional outlyingness

  # Get dimensions
  n <- dim(fdataobj)[1]
  p <- dim(fdataobj)[3]

  # Add row names
  if (is.null(rownames(fdataobj))) {
    rownames(fdataobj) <- seq_len(nrow(fdataobj))
  }

  # Check dimensions
  if (is.null(n) || is.null(p)) {
    stop("ERROR IN THE DATA DIMENSIONS")
  }

  # Calculate cutoffs
  qs <- boot(fdataobj, l = l, nb = nb, ns = ns, dfunc = dfunc)
  cutoff_avr <- lapply(qs$q_avr, function(x) quantile(x, probs = quan))
  cutoff_var <- quantile(qs$q_var, probs = quan)

  # Initialize variables
  dep.out_avr <- dep.out_var <- c()

  # Calculate DirOut results
  DirOut.Obj <- DirOut(fdataobj, depth.dir = dfunc)
  d_avr <- DirOut.Obj$out_avr
  d_var <- DirOut.Obj$out_var

  if (plot == TRUE) {
    # Create a plot for each column of d_avr against d_var
    par(mfrow = c(1, p))
    for (v in 1:p) {
      plot(d_avr[, v], d_var,
        main = paste("Directional Outlyingness Plot -", v),
        xlab = "Average Directional Outlyingness",
        ylab = "Variance of Directional Outlyingness",
        col = "steelblue",
        pch = 19,
        cex = 0.8
      )
      grid()
    }
    par(mfrow = c(1, 1))
  }

  # Calculate outliers based on average directional
  # outlyingness for each variable
  elim_avr <- c()
  for (v in 1:p) {
    cutt <- cutoff_avr[[v]] < d_avr[, v]
    elim_avr_v <- which(cutt)

    # If there are any outliers, add them to the list
    if (length(elim_avr_v) > 0) {
      dep.out_avr <- c(dep.out_avr, d_avr[elim_avr_v, v])
      elim_avr <- c(elim_avr, elim_avr_v)
    }
  }

  # Calculate outliers based on variance of directional outlyingness
  cutt <- cutoff_var < d_var
  elim_var <- which(cutt)

  # If there are any outliers, add them to the list
  if (length(elim_var) > 0) {
    dep.out_var <- c(dep.out_var, d_var[elim_var])
  }

  # Get unique outliers
  elim <- unique(c(elim_avr, elim_var))
  outliers <- rownames(fdataobj)[elim]

  # Return results
  return(list(
    outliers = outliers,
    dep.out_avr = dep.out_avr,
    dep.out_var = dep.out_var,
    q_avr = cutoff_avr,
    q_var = cutoff_var
  ))
}

sliding_window_outlier_dirout <- function(
    fdataobj,
    window_size = 8,
    nb = 200,
    quan = 0.5,
    ns = 0.99,
    dfunc = "RP",
    smo = 0,
    boot = SlidingWindow.DirOut,
    plot = FALSE) {
  # This function implements outlier detection using symmetric window DirOut
  # params:
  #   fdataobj: Functional data object to analyze
  #   window_size: Size of the symmetric window (default 8)
  #   nb: Number of bootstrap samples to generate
  #   quan: Quantile used for cutoff estimation from bootstrap
  #   ns: Quantile to calculate in each bootstrap iteration to cutoff estimation
  #   dfunc: Depth function to use
  #   boot: Bootstrap procedure to use (SlidingWindow.DirOut)
  # returns:
  #   list containing:
  #     outliers: Vector of detected outlier indices
  #     dep.out_avr: Vector of depth values for outliers based on average
  #       directional outlyingness
  #     dep.out_var: Vector of depth values for outliers based on variance of
  #       directional outlyingness
  #     q_avr: Estimated cutoff value for average directional outlyingness
  #     q_var: Estimated cutoff value for variance of directional outlyingness

  # Check if fdataobj is a functional data object
  if (!is.fdata(fdataobj)) {
    fdataobj <- fdata(fdataobj)
  }

  # Check if fdataobj contains any NA values
  nas1 <- is.na(fdataobj)
  if (any(nas1)) {
    stop("fdataobj contain ", sum(nas1), " curves with some NA value \n")
  }

  # Get number of functional data objects
  n <- nrow(fdataobj)
  m <- ncol(fdataobj)
  if (is.null(n) && is.null(m)) {
    stop("ERROR IN THE DATA DIMENSIONS")
  }
  if (is.null(row.names(fdataobj[["data"]]))) {
    row.names(fdataobj[["data"]]) <- 1:n
  }

  # Calculate cutoff using symmetric window bootstrap
  qs <- boot(fdataobj,
    window_size = window_size,
    nb = nb, ns = ns, dfunc = dfunc, smo = smo
  )
  cutoff_avr <- quantile(qs$q_avr, probs = quan)
  cutoff_var <- quantile(qs$q_var, probs = quan)

  # Calculate half window size for symmetric windows
  half_window <- floor(window_size / 2)

  # Initialize vectors to store aggregated directional outlyingness for each observation
  d_avr_all <- numeric(n)
  d_var_all <- numeric(n)
  window_counts <- numeric(n) # Track how many windows each observation appears in
  dep.out_avr <- dep.out_var <- c()
  outliers <- c()

  # For each observation, create a symmetric window around it
  for (i in 1:n) {
    # Calculate symmetric window boundaries
    start_idx <- max(1, i - half_window)
    end_idx <- min(n, i + half_window)

    # Extract window data
    window_data <- fdataobj[start_idx:end_idx]

    # Calculate directional outlyingness measures for this window
    DirOut.Obj <- DirOut(window_data[["data"]], depth.dir = dfunc)
    d_avr <- DirOut.Obj$out_avr
    d_var <- DirOut.Obj$out_var

    # Find the position of observation i in the window
    obs_pos_in_window <- i - start_idx + 1

    # Aggregate the directional outlyingness values for observation i
    d_avr_all[i] <- d_avr_all[i] + d_avr[obs_pos_in_window]
    d_var_all[i] <- d_var_all[i] + d_var[obs_pos_in_window]
    window_counts[i] <- window_counts[i] + 1
  }

  # Average the aggregated values by the number of windows each observation appears in
  d_avr_all <- d_avr_all / window_counts
  d_var_all <- d_var_all / window_counts

  # Find outliers based on cutoffs
  outliers_avr <- which(d_avr_all > cutoff_avr)
  outliers_var <- which(d_var_all > cutoff_var)

  # Get unique outliers
  final_outliers <- unique(c(outliers_avr, outliers_var))

  # Store depth values for outliers
  if (length(outliers_avr) > 0) {
    dep.out_avr <- d_avr_all[outliers_avr]
  }
  if (length(outliers_var) > 0) {
    dep.out_var <- d_var_all[outliers_var]
  }

  outliers <- rownames(fdataobj[["data"]])[final_outliers]

  if (plot == TRUE) {
    # Plot directional outlyingness values
    par(mfrow = c(2, 1))
    plot(d_avr_all,
      main = "Average Directional Outlyingness",
      xlab = "Observation Index",
      ylab = "Directional Outlyingness",
      col = "steelblue", pch = 19
    )
    abline(h = cutoff_avr, col = "red", lty = 2)
    points(outliers_avr, d_avr_all[outliers_avr], col = "red", pch = 19, cex = 1.2)

    plot(d_var_all,
      main = "Variance of Directional Outlyingness",
      xlab = "Observation Index",
      ylab = "Directional Outlyingness",
      col = "steelblue", pch = 19
    )
    abline(h = cutoff_var, col = "red", lty = 2)
    points(outliers_var, d_var_all[outliers_var], col = "red", pch = 19, cex = 1.2)
    par(mfrow = c(1, 1))
  }

  # Return the outliers, the depths of the outliers, and the cutoffs
  return(list(
    outliers = outliers,
    dep.out_avr = dep.out_avr,
    dep.out_var = dep.out_var,
    q_avr = cutoff_avr,
    q_var = cutoff_var,
    d_avr_all = d_avr_all,
    d_var_all = d_var_all
  ))
}
