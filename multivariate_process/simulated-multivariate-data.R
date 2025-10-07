multifdata <- function(
    rho, model = shape, k = 4, plot = TRUE, dim = 3) {
  obj <- model(rho = rho, k = k, plot = FALSE)

  # This function generates a multivariate functional data model
  # params:
  #   rho: Correlation parameter
  #   model: Function that generates the contaminated data
  #   k: Contamination level
  #   plot: Whether to plot the generated data
  #   dim: Dimension of the multivariate functional data
  # returns:
  #   list containing:
  #     mfdataobj: multivariate fdata object for contaminated model
  #     outliers: Vector of outlier indices

  # Original process
  fdataobj <- obj$fdataobj
  outliers <- obj$outliers

  # First derivative
  deriv1 <- fdata.deriv(fdataobj)
  row.names(deriv1[["data"]]) <- row.names(fdataobj[["data"]])

  # Second derivative
  deriv2 <- fdata.deriv(fdataobj, 2)
  row.names(deriv2[["data"]]) <- row.names(fdataobj[["data"]])

  # Multivariate functional object
  if (dim == 3) {
    mfdata <- list(
      original = fdataobj$data, firt = deriv1$data, second = deriv2$data
    )

    if (plot == TRUE) {
      x11()
      par(mfrow = c(1, 3))
      plot(fdataobj, main = "Original process", col = "gray")
      lines(fdataobj[outliers, ], col = "black", lty = 1, lwd = 2)
      plot(deriv1, main = "First derivative", col = "gray")
      lines(deriv1[outliers, ], col = "black", lty = 1, lwd = 2)
      plot(deriv2, main = "Second derivative", col = "gray")
      lines(deriv2[outliers, ], col = "black", lty = 1, lwd = 2)
    }
  } else if (dim == 2) {
    mfdata <- list(original = fdataobj$data, firt = deriv1$data)

    if (plot == TRUE) {
      x11()
      par(mfrow = c(1, 2))
      plot(fdataobj, main = "Original process")
      lines(fdataobj[outliers, ], col = "black", lty = 1, lwd = 2)
      plot(deriv1, main = "First derivative")
      lines(deriv1[outliers, ], col = "black", lty = 1, lwd = 2)
    }
  } else if (dim == 1) {
    mfdata <- list(original = fdataobj$data)

    if (plot == TRUE) {
      x11()
      plot(fdataobj, main = "Original process")
      lines(fdataobj[outliers, ], col = "black", lty = 1, lwd = 2)
    }
  } else {
    "Incorrect dimension"
  }

  # Return the list containing the multivariate functional
  # data object and the outliers indices
  list <- list(mfdataobj = mfdata, outliers = outliers)
  return(list)
}
