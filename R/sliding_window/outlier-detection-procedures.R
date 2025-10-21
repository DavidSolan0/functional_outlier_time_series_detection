sliding_window_outlier <- function(
    fdataobj,
    window_size = 8,
    dfunc = "RP",
    threshold = 0.5,
    plot = FALSE) {
  # This function implements outlier detection using sliding window with functional depth
  # and boxplot detection within each window
  # params:
  #   fdataobj: Functional data object to analyze
  #   window_size: Size of the symmetric window (default 8)
  #   dfunc: Depth function to use
  #   threshold: Threshold for determining outliers based on window count (default 0.5)
  # returns:
  #   list containing:
  #     outliers: Vector of detected outlier indices
  #     outlier_counts: Vector showing how many windows each observation was flagged as outlier
  #     window_outliers: List of outliers detected in each window

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

  # Calculate half window size for symmetric windows
  half_window <- floor(window_size / 2)

  # Initialize vector to count how many windows each observation is
  # flagged as outlier
  outlier_counts <- numeric(n)
  window_outliers <- list()

  # For each observation, create a symmetric window around it
  for (i in 1:n) {
    # Calculate symmetric window boundaries
    start_idx <- max(1, i - half_window)
    end_idx <- min(n, i + half_window)

    # Extract window data
    window_data <- fdataobj[start_idx:end_idx]

    # Calculate functional depth for this window
    depth_values <- dfunc(window_data[["data"]])

    # Use boxplot to detect outliers in depth values
    # Lower depth values indicate potential outliers
    boxplot_stats <- fda::fbplot(
      t(window_data[["data"]]),
      plot = FALSE, depth = depth_values
    )
    outlier_indices_in_window <- boxplot_stats$outpoint

    # Convert window indices to global indices
    global_outlier_indices <- start_idx + outlier_indices_in_window - 1

    # Store outliers for this window
    window_outliers[[i]] <- global_outlier_indices

    # Count how many times each observation appears as outlier
    for (outlier_idx in global_outlier_indices) {
      if (outlier_idx >= 1 && outlier_idx <= n) {
        outlier_counts[outlier_idx] <- outlier_counts[outlier_idx] + 1
      }
    }
  }

  # Determine final outliers based on threshold
  # An observation is considered an outlier if it appears as outlier in
  # at least (threshold * total_windows) windows
  max_windows <- 2 * half_window + 1 # Maximum number of windows an observation can appear in
  min_outlier_windows <- ceiling(threshold * max_windows)

  final_outliers <- which(outlier_counts >= min_outlier_windows)
  outliers <- rownames(fdataobj[["data"]])[final_outliers]

  if (plot == TRUE) {
    # Plot outlier counts
    par(mfrow = c(1, 1))
    plot(outlier_counts,
      main = "Number of Windows Each Observation is Flagged as Outlier",
      xlab = "Observation Index",
      ylab = "Number of Outlier Windows",
      col = "steelblue", pch = 19
    )
    abline(
      h = min_outlier_windows, col = "red", lty = 2,
      main = paste("Threshold:", min_outlier_windows, "windows")
    )
    points(final_outliers, outlier_counts[final_outliers],
      col = "red", pch = 19, cex = 1.2
    )
    legend("topright",
      legend = c("Observations", "Final Outliers", "Threshold"),
      col = c("steelblue", "red", "red"),
      pch = c(19, 19, NA),
      lty = c(NA, NA, 2)
    )
  }

  # Return the outliers and outlier counts
  return(list(
    outliers = outliers,
    outlier_counts = outlier_counts,
    window_outliers = window_outliers,
    threshold = threshold,
    min_outlier_windows = min_outlier_windows
  ))
}