outlier_dirout <- function(
    fdataobj,
    nb = 200,
    quan = 0.5,
    l = 4,
    ns = 0.99,
    dfunc = "RP",
    smo = 0,
    boot = MBBo.DirOut) {
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
  outliers <- dep.out_avr <- dep.out_var <- ite <- c()
  curvasgood <- fdataobj

  # Calculate DirOut results
  DirOut.Obj <- DirOut(curvasgood[["data"]], depth.dir = dfunc)
  d_avr <- DirOut.Obj$out_avr
  d_var <- DirOut.Obj$out_var

  # Calculate outliers based on average directional outlyingness
  cutt <- cutoff_avr < d_avr
  fecha <- as.numeric(rownames(curvasgood[["data"]])[cutt])
  elim <- which(cutt)

  # If there are any outliers, add them to the list
  if (length(elim) > 0) {
    dep.out_avr <- c(dep.out_avr, d_avr[elim])
    outliers <- c(outliers, fecha)
  }

  # Calculate outliers based on variance of directional outlyingness
  cutt <- cutoff_var < d_var
  fecha <- as.numeric(rownames(curvasgood[["data"]])[cutt])
  elim <- which(cutt)

  # If there are any outliers, add them to the list
  if (length(elim) > 0) {
    dep.out_var <- c(dep.out_var, d_var[elim])
    outliers <- c(outliers, fecha)
  }

  # Get the names of the outliers
  outliers <- rownames(fdataobj[["data"]])[unique(outliers)]
  names(dep.out_var) <- names(dep.out_avr) <- NULL

  # Return the outliers, the depths of the outliers,
  # the indices of the iterations, the cutoff, and the depths
  return(list(
    outliers = as.numeric(outliers),
    dep.out_avr = dep.out_avr,
    dep.out_var = dep.out_var,
    q_avr = cutoff_avr,
    q_var = cutoff_var
  ))
}