library(fda)
library(knitr)
# library(tcltk)  # Commented out - requires XQuartz on macOS
library(roahd)
library(dplyr)
library(fda.usc)
library(mrfDepth)

# workspace = 'C:/Users/David.Solano/OneDrive - Ipsos/David/functional_time_series_outlier_detection'
# setwd(workspace)

source("original_proposal/utils.R")
source("original_proposal/simulated-models.R")
source("original_proposal/depths.R")
source("original_proposal/bootstrap-procedures.R")
source("original_proposal/outlier-detection-procedures.R")
source("original_proposal/power-study-functions.R")
source("original_proposal/calculate_rates_results.R")

#* Parameters
M <- 100
seed <- 1234
depths <- c(BD)
depth_names <- c("MD")
nn <- length(depths)
outlier_detection_method <- outlier_bootstrap
bootstrap_estimation_method <- MBBo


#* Magnitude
K <- c(10, 15, 20, 25)
tabla <- run_simulation(
  K,
  depths = depths,
  depth_names = depth_names,
  M = M,
  outlier_detection_method = outlier_detection_method,
  bootstrap_estimation_method = bootstrap_estimation_method,
  seed = seed,
  model = magnitude,
  model_name = " (Magnitude model)"
)
tabla

#* Shape
K <- c(4, 5, 6, 7)
tabla <- run_simulation(
  K,
  depths = depths,
  depth_names = depth_names,
  M = M,
  outlier_detection_method = outlier_detection_method,
  bootstrap_estimation_method = bootstrap_estimation_method,
  seed = seed,
  model = shape,
  model_name = " (Shape model)"
)
tabla

#* Partial
K <- c(10, 15, 20, 25)
tabla <- run_simulation(
  K,
  depths = depths,
  depth_names = depth_names,
  M = M,
  outlier_detection_method = outlier_detection_method,
  bootstrap_estimation_method = bootstrap_estimation_method,
  seed = seed,
  model = shape,
  model_name = " (Partial model)"
)
tabla

# #* Mixed
# K <- list(c(10, 4, 10), c(15, 5, 15), c(20, 6, 20), c(25, 7, 25))
# tabla <- NULL

# set.seed(1234)
# for (i in 1:nn) {
#   iter <- 1
#   tabla_depth <- NULL
#   # Progress tracking without tcltk
#   cat(paste("Processing depth function", i, "of", nn, "(Mixed model)\n"))
#   for (k in 2:4) {
#     cat(paste("  Processing k =", k, "(", iter, "of", length(K), ")\n"))
#     rate <- rates(0.8,
#       k1 = K[[i]][1], k2 = K[[i]][2], k3 = K[[i]][3],
#       dfunc = depths[i][[1]], method = outlier_detection_method,
#       M = M, boot = bootstrap_estimation_method
#     )

#     vector <- c(rate$pf, rate$pc, rate$sd, rate$pdc)
#     names(vector) <- paste0(c("pf", "pc", "sd", "pdc"), "-", k)

#     tabla_K <- t(vector)

#     tabla_depth <- cbind(tabla_depth, t(vector))

#     iter <- iter + 1
#   }

#   tabla <- rbind(tabla, tabla_depth)
# }

# row.names(tabla) <- c("MBD", "MD")
# tabla
