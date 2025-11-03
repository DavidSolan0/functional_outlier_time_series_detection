# Script to run hyperparameter analysis for sliding window outlier detection
# This script will test different combinations of window_size and threshold parameters

# Load required libraries
library(fda)
library(knitr)
library(roahd)
library(dplyr)
library(fda.usc)
library(mrfDepth)
library(ggplot2)
library(gridExtra)

# Load the hyperparameter analysis functions
source("R/sliding_window/hyperparameter-grid-simulation.R")

# Run quick test first to verify everything works
cat("Running quick test...\n")
quick_results <- run_quick_test()

# If quick test works, run full analysis
if (!is.null(quick_results) && nrow(quick_results) > 0) {
  cat("\nQuick test successful! Running full analysis...\n")

  # Run complete analysis
  analysis_results <- run_complete_hyperparameter_analysis()

  # Print optimal parameters for each model
  cat("\n=== OPTIMAL HYPERPARAMETERS ===\n")

  cat("\nMagnitude Model:\n")
  if (!is.null(analysis_results$optimal$magnitude)) {
    print(analysis_results$optimal$magnitude)
  }

  cat("\nShape Model:\n")
  if (!is.null(analysis_results$optimal$shape)) {
    print(analysis_results$optimal$shape)
  }

  cat("\nPartial Model:\n")
  if (!is.null(analysis_results$optimal$partial)) {
    print(analysis_results$optimal$partial)
  }

  # Print summary tables
  cat("\n=== SUMMARY TABLES ===\n")

  cat("\nMagnitude Model - Window Size Summary:\n")
  if (!is.null(analysis_results$summary$magnitude$window_summary)) {
    print(analysis_results$summary$magnitude$window_summary)
  }

  cat("\nMagnitude Model - Threshold Summary:\n")
  if (!is.null(analysis_results$summary$magnitude$threshold_summary)) {
    print(analysis_results$summary$magnitude$threshold_summary)
  }

  # Create and save plots
  if (!is.null(analysis_results$plots$magnitude)) {
    cat("\nSaving plots...\n")

    # Save magnitude plots
    pdf("R/sliding_window/magnitude_hyperparameter_plots.pdf", width = 12, height = 8)
    print(analysis_results$plots$magnitude$fpr_vs_window)
    print(analysis_results$plots$magnitude$tpr_vs_window)
    print(analysis_results$plots$magnitude$fpr_heatmap)
    print(analysis_results$plots$magnitude$tpr_heatmap)
    dev.off()

    # Save shape plots
    if (!is.null(analysis_results$plots$shape)) {
      pdf("R/sliding_window/shape_hyperparameter_plots.pdf", width = 12, height = 8)
      print(analysis_results$plots$shape$fpr_vs_window)
      print(analysis_results$plots$shape$tpr_vs_window)
      print(analysis_results$plots$shape$fpr_heatmap)
      print(analysis_results$plots$shape$tpr_heatmap)
      dev.off()
    }

    # Save partial plots
    if (!is.null(analysis_results$plots$partial)) {
      pdf("R/sliding_window/partial_hyperparameter_plots.pdf", width = 12, height = 8)
      print(analysis_results$plots$partial$fpr_vs_window)
      print(analysis_results$plots$partial$tpr_vs_window)
      print(analysis_results$plots$partial$fpr_heatmap)
      print(analysis_results$plots$partial$tpr_heatmap)
      dev.off()
    }
  }

  cat("\nAnalysis complete! Check the generated files:\n")
  cat("- hyperparameter_analysis_results.RData\n")
  cat("- magnitude_hyperparameter_plots.pdf\n")
  cat("- shape_hyperparameter_plots.pdf\n")
  cat("- partial_hyperparameter_plots.pdf\n")
} else {
  cat("Quick test failed. Please check the error messages above.\n")
}
