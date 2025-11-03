# Quick test script for hyperparameter analysis
# This script runs a small test to verify everything works before running the full analysis

# Load required libraries
library(fda)
library(knitr)
library(roahd)
library(dplyr)
library(fda.usc)
library(mrfDepth)

# Load the hyperparameter analysis functions
source("R/sliding_window/hyperparameter-grid-simulation.R")

cat("=== QUICK TEST FOR HYPERPARAMETER ANALYSIS ===\n\n")

# Test 1: Univariate quick test
cat("Test 1: Univariate quick test...\n")
tryCatch({
  uni_test <- run_quick_test()
  if (!is.null(uni_test) && nrow(uni_test) > 0) {
    cat("✓ Univariate test passed!\n")
    cat("Sample results:\n")
    print(head(uni_test))
  } else {
    cat("✗ Univariate test failed\n")
  }
}, error = function(e) {
  cat("✗ Univariate test failed with error:", e$message, "\n")
})

cat("\n")

# Test 2: Multivariate quick test
cat("Test 2: Multivariate quick test...\n")
tryCatch({
  multi_test <- run_multivariate_quick_test()
  if (!is.null(multi_test) && nrow(multi_test) > 0) {
    cat("✓ Multivariate test passed!\n")
    cat("Sample results:\n")
    print(head(multi_test))
  } else {
    cat("✗ Multivariate test failed\n")
  }
}, error = function(e) {
  cat("✗ Multivariate test failed with error:", e$message, "\n")
})

cat("\n")

# Test 3: Check if all required functions are available
cat("Test 3: Checking function availability...\n")
required_functions <- c(
  "run_hyperparameter_grid_simulation",
  "create_performance_plots",
  "find_optimal_hyperparameters",
  "create_summary_tables",
  "run_quick_test",
  "run_multivariate_quick_test"
)

missing_functions <- c()
for (func in required_functions) {
  if (!exists(func)) {
    missing_functions <- c(missing_functions, func)
  }
}

if (length(missing_functions) == 0) {
  cat("✓ All required functions are available\n")
} else {
  cat("✗ Missing functions:", paste(missing_functions, collapse = ", "), "\n")
}

cat("\n")

# Test 4: Check if data generation functions work
cat("Test 4: Testing data generation...\n")
tryCatch({
  # Test magnitude model
  fit_mag <- magnitude(rho = 0.8, k = 10, plot = FALSE)
  if (!is.null(fit_mag$fdataobj)) {
    cat("✓ Magnitude model data generation works\n")
  } else {
    cat("✗ Magnitude model data generation failed\n")
  }
  
  # Test shape model
  fit_shape <- shape(rho = 0.8, k = 5, plot = FALSE)
  if (!is.null(fit_shape$fdataobj)) {
    cat("✓ Shape model data generation works\n")
  } else {
    cat("✗ Shape model data generation failed\n")
  }
  
  # Test partial model
  fit_partial <- partial(rho = 0.8, k = 10, plot = FALSE)
  if (!is.null(fit_partial$fdataobj)) {
    cat("✓ Partial model data generation works\n")
  } else {
    cat("✗ Partial model data generation failed\n")
  }
  
}, error = function(e) {
  cat("✗ Data generation test failed with error:", e$message, "\n")
})

cat("\n")

# Test 5: Check if depth functions work
cat("Test 5: Testing depth functions...\n")
tryCatch({
  # Create sample data
  fit <- magnitude(rho = 0.8, k = 10, plot = FALSE)
  fdataobj <- fit$fdataobj
  
  # Test MBD
  depth_values <- MBD(fdataobj$data)
  if (length(depth_values) == nrow(fdataobj)) {
    cat("✓ MBD depth function works\n")
  } else {
    cat("✗ MBD depth function failed\n")
  }
  
  # Test multivariate depth
  mfit <- multifdata(rho = 0.8, model = magnitude, k = 10, plot = FALSE)
  mfdata <- mfit$mfdataobj
  mdepth_values <- multiMBD(mfdata)
  if (length(mdepth_values) == nrow(mfdata[[1]])) {
    cat("✓ MultiMBD depth function works\n")
  } else {
    cat("✗ MultiMBD depth function failed\n")
  }
  
}, error = function(e) {
  cat("✗ Depth function test failed with error:", e$message, "\n")
})

cat("\n")

# Summary
cat("=== QUICK TEST SUMMARY ===\n")
cat("If all tests passed, you can proceed with the full analysis:\n")
cat("1. Run: source('R/sliding_window/run-hyperparameter-analysis.R')\n")
cat("2. Run: source('R/sliding_window/multivariate-hyperparameter-analysis.R')\n")
cat("3. Run: source('R/sliding_window/comprehensive-hyperparameter-analysis.R')\n")
cat("\nIf any tests failed, please check the error messages above and fix the issues.\n")
