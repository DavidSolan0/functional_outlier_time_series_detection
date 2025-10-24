library(fda)
library(knitr)
library(roahd)
library(dplyr)
library(fda.usc)
library(mrfDepth)



# Load global functions
source("R/utils.R")
source("R/simulated-models.R")
source("R/depths.R")
source("R/calculate_rates_results.R")

# Load local functions
source("R/sliding_window/outlier-detection-procedures.R")
source("R/sliding_window/power-study-functions.R")

#** Univariate model **#

#* Parameters
M <- 100
seed <- 1234
window_size <- 8
dfunc <- MBD


#* Magnitude
K <- c(10, 15, 20, 25)
tabla <- run_simulation_sliding_window(
  K,
  rho = 0.8,
  model = magnitude,
  window_size = window_size,
  M = M,
  dfunc = dfunc,
  multivariate = FALSE,
  seed = seed
)
tabla

#* Shape
K <- c(4, 5, 6, 7)
tabla <- run_simulation_sliding_window(
  K,
  rho = 0.8,
  model = shape,
  window_size = window_size,
  M = M,
  dfunc = dfunc,
  multivariate = FALSE,
  seed = seed
)
tabla

#* Partial
K <- c(10, 15, 20, 25)
tabla <- run_simulation_sliding_window(
  K,
  rho = 0.8,
  model = partial,
  window_size = window_size,
  M = M,
  dfunc = dfunc,
  multivariate = FALSE,
  seed = seed
)
tabla


#** Multivariate model **#

#* Parameters
M <- 100
seed <- 1234
window_size <- 8
dfunc <- multiMBD
threshold <- 0.5

#* Magnitude
K <- c(10, 15, 20, 25)
tabla <- run_simulation_sliding_window(
  K,
  rho = 0.8,
  model = magnitude,
  window_size = window_size,
  M = M,
  dfunc = dfunc,
  multivariate = TRUE,
  seed = seed,
  threshold = threshold,
)
tabla

#* Shape
K <- c(4, 5, 6, 7)
tabla <- run_simulation_sliding_window(
  K,
  rho = 0.8,
  model = shape,
  window_size = window_size,
  M = M,
  dfunc = dfunc,
  multivariate = TRUE,
  seed = seed,
  threshold = threshold,
)
tabla

#* Partial
K <- c(10, 15, 20, 25)
tabla <- run_simulation_sliding_window(
  K,
  rho = 0.8,
  model = partial,
  window_size = window_size,
  M = M,
  dfunc = dfunc,
  multivariate = TRUE,
  seed = seed,
  threshold = threshold,
)
tabla
