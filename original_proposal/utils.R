library(fda)
library(roahd)
library(fda.usc)
library(mrfDepth)

#* uniform Discrete

runifdisc <- function(n, min = 0, max = 1) sample(min:max, n, replace = TRUE)

#* first step of the cut off estimation algorithm

clean_outliers <- function(fdataobj, depths) {
  # params:
  # fdataobj: functional data object
  # depths: depths of the functional data object

  # Check if fdataobj is a functional data object
  if (!is.fdata(fdataobj)) {
    fdataobj <- fdata(fdataobj)
  }

  # Extract data
  dat <- fdataobj[["data"]]

  # Calculate outliers
  o <- fda::fbplot(t(dat), plot = FALSE, depth = depths)$outpoint

  # Clean outliers
  if (length(o) != 0) {
    fdataobj[["data"]] <- fdataobj[["data"]][-o, ]
  }

  return(fdataobj)
}

#* clock's i curves

block_i_generator <- function(i, l) i:(i + l - 1)

#* module

block_StBo_generator <- function(i, n, l) {
  vector <- i:(i + l - 1) %% n
  mod <- vector %% n
  cam <- mod == 0
  vector[cam] <- n
  vector
}

#* cleaning

clean_outliers_multivariate <- function(fdata, depths) {
  # params:
  # fdata: multivariate functional data object
  # depths: depths of the multivariate functional data object

  # Check if fdata is a multivariate functional data object
  if (!is.list(fdata)) {
    fdata <- list(fdata)
  }

  # Extract data
  multivariate_fdata <- fdata
  dim <- length(multivariate_fdata)

  # Initialize vector for outliers
  o <- NULL
  for (i in 1:dim) {
    # Calculate outliers
    o0 <- fda::fbplot(t(multivariate_fdata[[i]]), plot = FALSE, depth = depths)
    o0 <- o0$outpoint

    # Update vector for outliers
    o <- unique(c(o0, o))
  }

  # Get unique outliers
  o <- unique(o)

  # Clean outliers
  if (length(o) != 0) {
    for (i in 1:dim) multivariate_fdata[[i]] <- multivariate_fdata[[i]][-o, ]
  }

  # Return multivariate functional data object
  return(multivariate_fdata)
}
