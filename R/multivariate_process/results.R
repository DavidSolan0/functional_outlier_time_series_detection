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
source("R/multivariate_process/bootstrap-procedures.R")
source("R/multivariate_process/outlier-detection-procedures.R")
source("R/multivariate_process/power-study-functions.R")


#* Parameters
M <- 100
seed <- 1234
depths <- c(multiMBD)
depth_names <- c("multiMBD")
nn <- length(depths)
outlier_detection_method <- multivariate_functional_boxplot_outlier_detection_method
bootstrap_estimation_method <- NULL

#* Magnitude
K <- c(10, 15, 20, 25)
tabla <- run_simulation_multivariate(
    K,
    depths = depths,
    depth_names = depth_names,
    M = M,
    outlier_detection_method = outlier_detection_method,
    bootstrap_estimation_method = bootstrap_estimation_method,
    seed = seed,
    model = magnitude,
    model_name = " (Magnitude model)",
    weights = c(1, 0, 0),
)
tabla

#* Shape
K <- c(4, 5, 6, 7)
tabla <- run_simulation_multivariate(
    K,
    depths = depths,
    depth_names = depth_names,
    M = M,
    outlier_detection_method = outlier_detection_method,
    bootstrap_estimation_method = bootstrap_estimation_method,
    seed = seed,
    model = shape,
    model_name = " (Shape model)",
    weights = c(1 / 3, 1 / 3, 1 / 3),
)
tabla

#* Partial
K <- c(10, 15, 20, 25)
tabla <- run_simulation_multivariate(
    K,
    depths = depths,
    depth_names = depth_names,
    M = M,
    outlier_detection_method = outlier_detection_method,
    bootstrap_estimation_method = bootstrap_estimation_method,
    seed = seed,
    model = shape,
    model_name = " (Partial model)",
    weights = c(1 / 3, 1 / 3, 1 / 3)
)
tabla
