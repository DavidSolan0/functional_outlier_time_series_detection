# Multivariate hyperparameter analysis for sliding window outlier detection
# This script tests different combinations of window_size and threshold parameters
# for multivariate functional data

# Load required libraries
library(fda)
library(knitr)
library(roahd)
library(dplyr)
library(fda.usc)
library(mrfDepth)
library(ggplot2)
library(gridExtra)

# Suppress dplyr warnings about global variables
utils::globalVariables(c("window_size", "threshold", "K", "false_positive_rate", 
                        "true_positive_rate", "score", "mean_fpr", "mean_tpr", 
                        "sd_fpr", "sd_tpr", "n_combinations", "type", "magnitude", 
                        "shape", "partial", "results_magnitude", "results_shape", 
                        "results_partial"))

# Load the hyperparameter analysis functions
source("R/sliding_window/hyperparameter-grid-simulation.R")

# Function to run multivariate hyperparameter grid simulation
run_multivariate_hyperparameter_analysis <- function() {
  
  cat("=== MULTIVARIATE SLIDING WINDOW HYPERPARAMETER ANALYSIS ===\n\n")
  
  # Define parameter ranges
  window_sizes <- c(4, 6, 8, 10, 12, 16)
  thresholds <- c(0.3, 0.4, 0.5, 0.6, 0.7)
  K_values <- c(10, 15, 20, 25)
  
  # Run simulation for multivariate magnitude model
  cat("Running simulation for multivariate magnitude model...\n")
  results_magnitude <- run_hyperparameter_grid_simulation(
    window_sizes = window_sizes,
    thresholds = thresholds,
    K_values = K_values,
    rho = 0.8,
    model = magnitude,
    M = 50,
    dfunc = multiMBD,
    multivariate = TRUE,
    seed = 1234
  )
  
  # Run simulation for multivariate shape model
  cat("\nRunning simulation for multivariate shape model...\n")
  results_shape <- run_hyperparameter_grid_simulation(
    window_sizes = window_sizes,
    thresholds = thresholds,
    K_values = K_values,
    rho = 0.8,
    model = shape,
    M = 50,
    dfunc = multiMBD,
    multivariate = TRUE,
    seed = 1234
  )
  
  # Run simulation for multivariate partial model
  cat("\nRunning simulation for multivariate partial model...\n")
  results_partial <- run_hyperparameter_grid_simulation(
    window_sizes = window_sizes,
    thresholds = thresholds,
    K_values = K_values,
    rho = 0.8,
    model = partial,
    M = 50,
    dfunc = multiMBD,
    multivariate = TRUE,
    seed = 1234
  )
  
  # Create visualizations
  cat("\nCreating visualizations...\n")
  plots_magnitude <- create_performance_plots(results_magnitude, "Multivariate Magnitude")
  plots_shape <- create_performance_plots(results_shape, "Multivariate Shape")
  plots_partial <- create_performance_plots(results_partial, "Multivariate Partial")
  
  # Find optimal hyperparameters
  cat("\nFinding optimal hyperparameters...\n")
  optimal_magnitude <- find_optimal_hyperparameters(results_magnitude)
  optimal_shape <- find_optimal_hyperparameters(results_shape)
  optimal_partial <- find_optimal_hyperparameters(results_partial)
  
  # Create summary tables
  cat("\nCreating summary tables...\n")
  summary_magnitude <- create_summary_tables(results_magnitude)
  summary_shape <- create_summary_tables(results_shape)
  summary_partial <- create_summary_tables(results_partial)
  
  # Save results
  save(results_magnitude, results_shape, results_partial,
       plots_magnitude, plots_shape, plots_partial,
       optimal_magnitude, optimal_shape, optimal_partial,
       summary_magnitude, summary_shape, summary_partial,
       file = "R/sliding_window/multivariate_hyperparameter_analysis_results.RData")
  
  cat("\nMultivariate analysis complete! Results saved to multivariate_hyperparameter_analysis_results.RData\n")
  
  return(list(
    results = list(
      magnitude = results_magnitude,
      shape = results_shape,
      partial = results_partial
    ),
    plots = list(
      magnitude = plots_magnitude,
      shape = plots_shape,
      partial = plots_partial
    ),
    optimal = list(
      magnitude = optimal_magnitude,
      shape = optimal_shape,
      partial = optimal_partial
    ),
    summary = list(
      magnitude = summary_magnitude,
      shape = summary_shape,
      partial = summary_partial
    )
  ))
}

# Function to compare univariate vs multivariate performance
compare_univariate_multivariate <- function() {
  
  cat("=== COMPARING UNIVARIATE VS MULTIVARIATE PERFORMANCE ===\n")
  
  # Load univariate results if they exist
  if (file.exists("R/sliding_window/hyperparameter_analysis_results.RData")) {
    load("R/sliding_window/hyperparameter_analysis_results.RData")
    univariate_results <- list(
      magnitude = results_magnitude,
      shape = results_shape,
      partial = results_partial
    )
  } else {
    cat("Univariate results not found. Please run univariate analysis first.\n")
    return(NULL)
  }
  
  # Load multivariate results if they exist
  if (file.exists("R/sliding_window/multivariate_hyperparameter_analysis_results.RData")) {
    load("R/sliding_window/multivariate_hyperparameter_analysis_results.RData")
    multivariate_results <- list(
      magnitude = results_magnitude,
      shape = results_shape,
      partial = results_partial
    )
  } else {
    cat("Multivariate results not found. Please run multivariate analysis first.\n")
    return(NULL)
  }
  
  # Create comparison plots
  create_comparison_plots <- function(uni_results, multi_results, model_name) {
    
    # Combine results with type indicator
    uni_results$type <- "Univariate"
    multi_results$type <- "Multivariate"
    
    combined_results <- rbind(uni_results, multi_results)
    combined_results <- combined_results[!is.na(combined_results$false_positive_rate), ]
    
    if (nrow(combined_results) == 0) {
      return(NULL)
    }
    
    # Plot 1: FPR comparison
    p1 <- ggplot(combined_results, aes(x = window_size, y = false_positive_rate, 
                                      color = type, linetype = factor(threshold))) +
      geom_line(aes(group = interaction(type, threshold, K)), alpha = 0.7) +
      geom_point(aes(size = K), alpha = 0.8) +
      facet_wrap(~K, scales = "free_y") +
      labs(title = paste("False Positive Rate Comparison -", model_name),
           x = "Window Size",
           y = "False Positive Rate",
           color = "Type",
           linetype = "Threshold",
           size = "K") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Plot 2: TPR comparison
    p2 <- ggplot(combined_results, aes(x = window_size, y = true_positive_rate, 
                                      color = type, linetype = factor(threshold))) +
      geom_line(aes(group = interaction(type, threshold, K)), alpha = 0.7) +
      geom_point(aes(size = K), alpha = 0.8) +
      facet_wrap(~K, scales = "free_y") +
      labs(title = paste("True Positive Rate Comparison -", model_name),
           x = "Window Size",
           y = "True Positive Rate",
           color = "Type",
           linetype = "Threshold",
           size = "K") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    return(list(fpr_comparison = p1, tpr_comparison = p2))
  }
  
  # Create comparison plots for each model
  magnitude_comparison <- create_comparison_plots(
    univariate_results$magnitude, 
    multivariate_results$magnitude, 
    "Magnitude"
  )
  
  shape_comparison <- create_comparison_plots(
    univariate_results$shape, 
    multivariate_results$shape, 
    "Shape"
  )
  
  partial_comparison <- create_comparison_plots(
    univariate_results$partial, 
    multivariate_results$partial, 
    "Partial"
  )
  
  # Save comparison plots
  if (!is.null(magnitude_comparison)) {
    pdf("R/sliding_window/magnitude_univariate_vs_multivariate_comparison.pdf", 
        width = 14, height = 10)
    print(magnitude_comparison$fpr_comparison)
    print(magnitude_comparison$tpr_comparison)
    dev.off()
  }
  
  if (!is.null(shape_comparison)) {
    pdf("R/sliding_window/shape_univariate_vs_multivariate_comparison.pdf", 
        width = 14, height = 10)
    print(shape_comparison$fpr_comparison)
    print(shape_comparison$tpr_comparison)
    dev.off()
  }
  
  if (!is.null(partial_comparison)) {
    pdf("R/sliding_window/partial_univariate_vs_multivariate_comparison.pdf", 
        width = 14, height = 10)
    print(partial_comparison$fpr_comparison)
    print(partial_comparison$tpr_comparison)
    dev.off()
  }
  
  cat("Comparison plots saved!\n")
  
  return(list(
    magnitude_comparison = magnitude_comparison,
    shape_comparison = shape_comparison,
    partial_comparison = partial_comparison
  ))
}

# Quick multivariate test
run_multivariate_quick_test <- function() {
  cat("=== QUICK MULTIVARIATE HYPERPARAMETER TEST ===\n")
  
  # Smaller parameter ranges for quick testing
  window_sizes <- c(6, 8, 10)
  thresholds <- c(0.4, 0.5, 0.6)
  K_values <- c(10, 20)
  
  # Test with magnitude model only
  results <- run_hyperparameter_grid_simulation(
    window_sizes = window_sizes,
    thresholds = thresholds,
    K_values = K_values,
    rho = 0.8,
    model = magnitude,
    M = 20,  # Very small for quick test
    dfunc = multiMBD,
    multivariate = TRUE,
    seed = 1234
  )
  
  # Print results
  print("Results:")
  print(results)
  
  # Find optimal parameters
  optimal <- find_optimal_hyperparameters(results)
  print("Optimal parameters:")
  print(optimal)
  
  return(results)
}

# Main execution
cat("Starting multivariate hyperparameter analysis...\n")

# Run quick test first
quick_results <- run_multivariate_quick_test()

# If quick test works, run full analysis
if (!is.null(quick_results) && nrow(quick_results) > 0) {
  cat("\nQuick test successful! Running full multivariate analysis...\n")
  
  # Run complete multivariate analysis
  multivariate_analysis <- run_multivariate_hyperparameter_analysis()
  
  # Print optimal parameters
  cat("\n=== OPTIMAL MULTIVARIATE HYPERPARAMETERS ===\n")
  
  cat("\nMultivariate Magnitude Model:\n")
  if (!is.null(multivariate_analysis$optimal$magnitude)) {
    print(multivariate_analysis$optimal$magnitude)
  }
  
  cat("\nMultivariate Shape Model:\n")
  if (!is.null(multivariate_analysis$optimal$shape)) {
    print(multivariate_analysis$optimal$shape)
  }
  
  cat("\nMultivariate Partial Model:\n")
  if (!is.null(multivariate_analysis$optimal$partial)) {
    print(multivariate_analysis$optimal$partial)
  }
  
  # Create comparison plots if univariate results exist
  if (file.exists("R/sliding_window/hyperparameter_analysis_results.RData")) {
    cat("\nCreating univariate vs multivariate comparison...\n")
    comparison_results <- compare_univariate_multivariate()
  }
  
  cat("\nMultivariate analysis complete!\n")
  
} else {
  cat("Quick test failed. Please check the error messages above.\n")
}
