# Test script for sliding window DirOut rates calculation
# This script demonstrates how to use the new calculate_sliding_window_dirout_rates function

# Load required libraries
library(fda.usc)

# Source the functions
source("R/utils.R")
source("R/dirout/DirOut.R")
source("R/simulated-models.R")
source("R/dirout/bootstrap-procedures.R")
source("R/dirout/outlier-detection-procedures.R")
source("R/dirout/power-study-functions.R")

cat("=== SLIDING WINDOW DIROUT RATES CALCULATION ===\n")

# Test parameters
M <- 10  # Number of simulations (reduced for faster testing)
rho <- 0.8
k <- 25
window_size <- 8

cat(sprintf("Running %d simulations with:\n", M))
cat(sprintf("  - Correlation parameter (rho): %.1f\n", rho))
cat(sprintf("  - Contamination level (k): %d\n", k))
cat(sprintf("  - Window size: %d\n", window_size))
cat("\n")

# Test with magnitude model
cat("Testing with magnitude model...\n")
magnitude_results <- calculate_sliding_window_dirout_rates(
  rho = rho,
  k = k,
  model = magnitude,
  method = sliding_window_outlier_dirout,
  M = M,
  window_size = window_size,
  dfunc = "RP",
  boot = SlidingWindow.DirOut,
  multivariate = FALSE
)

cat("Magnitude Model Results:\n")
cat(sprintf("  False Positive Rate: %.3f\n", magnitude_results$false_positive_rate))
cat(sprintf("  True Positive Rate: %.3f\n", magnitude_results$true_positive_rate))
cat(sprintf("  Precision: %.3f\n", magnitude_results$precision))
cat(sprintf("  Recall: %.3f\n", magnitude_results$recall))
cat(sprintf("  F1 Score: %.3f\n", magnitude_results$f1_score))
cat(sprintf("  SD True Positive Rate: %.3f\n", magnitude_results$sd_true_positive_rate))
cat(sprintf("  True Positive Rate (non-zero): %.3f\n", magnitude_results$true_positive_rate_zero_clean))

# Test with shape model
cat("\nTesting with shape model...\n")
shape_results <- calculate_sliding_window_dirout_rates(
  rho = rho,
  k = k,
  model = shape,
  method = sliding_window_outlier_dirout,
  M = M,
  window_size = window_size,
  dfunc = "RP",
  boot = SlidingWindow.DirOut,
  multivariate = FALSE
)

cat("Shape Model Results:\n")
cat(sprintf("  False Positive Rate: %.3f\n", shape_results$false_positive_rate))
cat(sprintf("  True Positive Rate: %.3f\n", shape_results$true_positive_rate))
cat(sprintf("  Precision: %.3f\n", shape_results$precision))
cat(sprintf("  Recall: %.3f\n", shape_results$recall))
cat(sprintf("  F1 Score: %.3f\n", shape_results$f1_score))
cat(sprintf("  SD True Positive Rate: %.3f\n", shape_results$sd_true_positive_rate))
cat(sprintf("  True Positive Rate (non-zero): %.3f\n", shape_results$true_positive_rate_zero_clean))

# Test with partial model
cat("\nTesting with partial model...\n")
partial_results <- calculate_sliding_window_dirout_rates(
  rho = rho,
  k = k,
  model = partial,
  method = sliding_window_outlier_dirout,
  M = M,
  window_size = window_size,
  dfunc = "RP",
  boot = SlidingWindow.DirOut,
  multivariate = FALSE
)

cat("Partial Model Results:\n")
cat(sprintf("  False Positive Rate: %.3f\n", partial_results$false_positive_rate))
cat(sprintf("  True Positive Rate: %.3f\n", partial_results$true_positive_rate))
cat(sprintf("  Precision: %.3f\n", partial_results$precision))
cat(sprintf("  Recall: %.3f\n", partial_results$recall))
cat(sprintf("  F1 Score: %.3f\n", partial_results$f1_score))
cat(sprintf("  SD True Positive Rate: %.3f\n", partial_results$sd_true_positive_rate))
cat(sprintf("  True Positive Rate (non-zero): %.3f\n", partial_results$true_positive_rate_zero_clean))

# Compare different window sizes
cat("\n=== WINDOW SIZE COMPARISON ===\n")
window_sizes <- c(5, 8, 12)
comparison_results <- data.frame(
  window_size = window_sizes,
  f1_score = numeric(length(window_sizes)),
  precision = numeric(length(window_sizes)),
  recall = numeric(length(window_sizes))
)

for (i in seq_along(window_sizes)) {
  ws <- window_sizes[i]
  cat(sprintf("Testing window size %d...\n", ws))
  
  results <- calculate_sliding_window_dirout_rates(
    rho = rho,
    k = k,
    model = magnitude,
    method = sliding_window_outlier_dirout,
    M = M,
    window_size = ws,
    dfunc = "RP",
    boot = SlidingWindow.DirOut,
    multivariate = FALSE
  )
  
  comparison_results$f1_score[i] <- results$f1_score
  comparison_results$precision[i] <- results$precision
  comparison_results$recall[i] <- results$recall
}

cat("\nWindow Size Comparison Results:\n")
print(comparison_results)

# Find best window size
best_idx <- which.max(comparison_results$f1_score)
cat(sprintf("\nBest window size: %d (F1 Score: %.3f)\n", 
            comparison_results$window_size[best_idx], 
            comparison_results$f1_score[best_idx]))

cat("\n=== TEST COMPLETED ===\n")
cat("The sliding window approach provides comprehensive performance metrics\n")
cat("including precision, recall, and F1 score for better evaluation.\n")
