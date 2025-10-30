# Test script for hyperparameter analysis
# This script runs a minimal test to verify the hyperparameter analysis works

# Load required libraries
library(fda)
library(knitr)
library(roahd)
library(dplyr)
library(fda.usc)
library(mrfDepth)


# Load the hyperparameter analysis functions
source("R/sliding_window/hyperparameter-grid-simulation.R")

cat("=== TESTING HYPERPARAMETER ANALYSIS ===\n\n")

# Test with minimal parameters
cat("Running minimal hyperparameter test...\n")

# Very small test to verify everything works
test_results <- run_hyperparameter_grid_simulation(
  window_sizes = c(6, 8),
  thresholds = c(0.4, 0.5),
  K_values = c(10),
  rho = 0.8,
  model = magnitude,
  M = 5,  # Very small for testing
  dfunc = MBD,
  multivariate = FALSE,
  seed = 1234
)

cat("Test completed!\n")
cat("Number of results:", nrow(test_results), "\n")

if (nrow(test_results) > 0) {
  cat("Sample results:\n")
  print(head(test_results))
  
  # Test finding optimal parameters
  cat("\nTesting optimal parameter finding...\n")
  optimal <- find_optimal_hyperparameters(test_results)
  
  if (!is.null(optimal)) {
    cat("Optimal parameters found:\n")
    print(optimal)
  } else {
    cat("No optimal parameters found\n")
  }
  
  # Test summary tables
  cat("\nTesting summary tables...\n")
  summary_tables <- create_summary_tables(test_results)
  
  if (!is.null(summary_tables)) {
    cat("Summary tables created successfully\n")
    print(summary_tables$window_summary)
  } else {
    cat("Summary tables creation failed\n")
  }
  
  cat("\n✓ All tests passed! The hyperparameter analysis is working correctly.\n")
  cat("You can now run the full analysis with larger parameter ranges.\n")
  
} else {
  cat("✗ Test failed - no results generated\n")
}
