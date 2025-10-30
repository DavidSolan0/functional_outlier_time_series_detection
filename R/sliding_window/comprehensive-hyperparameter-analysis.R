# Comprehensive hyperparameter analysis for sliding window outlier detection
# This script runs both univariate and multivariate analyses and generates a comprehensive report

# Load required libraries
library(fda)
library(knitr)
library(roahd)
library(dplyr)
library(fda.usc)
library(mrfDepth)
library(ggplot2)
library(gridExtra)
library(kableExtra)

# Load the hyperparameter analysis functions
source("R/sliding_window/hyperparameter-grid-simulation.R")

# Function to generate comprehensive report
generate_comprehensive_report <- function() {
  
  cat("=== COMPREHENSIVE HYPERPARAMETER ANALYSIS REPORT ===\n\n")
  
  # Check if results exist
  uni_file <- "R/sliding_window/hyperparameter_analysis_results.RData"
  multi_file <- "R/sliding_window/multivariate_hyperparameter_analysis_results.RData"
  
  if (!file.exists(uni_file)) {
    cat("Univariate results not found. Please run univariate analysis first.\n")
    return(NULL)
  }
  
  if (!file.exists(multi_file)) {
    cat("Multivariate results not found. Please run multivariate analysis first.\n")
    return(NULL)
  }
  
  # Load results
  load(uni_file)
  uni_results <- list(
    magnitude = results_magnitude,
    shape = results_shape,
    partial = results_partial,
    optimal = list(
      magnitude = optimal_magnitude,
      shape = optimal_shape,
      partial = optimal_partial
    )
  )
  
  load(multi_file)
  multi_results <- list(
    magnitude = results_magnitude,
    shape = results_shape,
    partial = results_partial,
    optimal = list(
      magnitude = optimal_magnitude,
      shape = optimal_shape,
      partial = optimal_partial
    )
  )
  
  # Create comprehensive summary table
  create_comprehensive_summary <- function(uni_opt, multi_opt, model_name) {
    
    if (is.null(uni_opt) || is.null(multi_opt)) {
      return(NULL)
    }
    
    # Combine optimal parameters
    uni_opt$type <- "Univariate"
    multi_opt$type <- "Multivariate"
    
    combined <- rbind(uni_opt, multi_opt)
    
    # Create summary
    summary_table <- combined %>%
      select(type, K, window_size, threshold, 
             false_positive_rate, true_positive_rate) %>%
      arrange(K, type)
    
    return(summary_table)
  }
  
  # Generate summary tables for each model
  magnitude_summary <- create_comprehensive_summary(
    uni_results$optimal$magnitude, 
    multi_results$optimal$magnitude, 
    "Magnitude"
  )
  
  shape_summary <- create_comprehensive_summary(
    uni_results$optimal$shape, 
    multi_results$optimal$shape, 
    "Shape"
  )
  
  partial_summary <- create_comprehensive_summary(
    uni_results$optimal$partial, 
    multi_results$optimal$partial, 
    "Partial"
  )
  
  # Print comprehensive results
  cat("=== OPTIMAL HYPERPARAMETERS SUMMARY ===\n\n")
  
  if (!is.null(magnitude_summary)) {
    cat("MAGNITUDE MODEL:\n")
    print(magnitude_summary)
    cat("\n")
  }
  
  if (!is.null(shape_summary)) {
    cat("SHAPE MODEL:\n")
    print(shape_summary)
    cat("\n")
  }
  
  if (!is.null(partial_summary)) {
    cat("PARTIAL MODEL:\n")
    print(partial_summary)
    cat("\n")
  }
  
  # Create performance comparison plots
  create_performance_comparison <- function(uni_data, multi_data, model_name) {
    
    # Combine data
    uni_data$type <- "Univariate"
    multi_data$type <- "Multivariate"
    
    combined_data <- rbind(uni_data, multi_data)
    combined_data <- combined_data[!is.na(combined_data$false_positive_rate), ]
    
    if (nrow(combined_data) == 0) {
      return(NULL)
    }
    
    # Calculate average performance by window size and threshold
    avg_performance <- combined_data %>%
      group_by(type, window_size, threshold) %>%
      summarise(
        avg_fpr = mean(false_positive_rate, na.rm = TRUE),
        avg_tpr = mean(true_positive_rate, na.rm = TRUE),
        n_combinations = n(),
        .groups = 'drop'
      )
    
    # Plot 1: Average FPR vs Window Size
    p1 <- ggplot(avg_performance, aes(x = window_size, y = avg_fpr, 
                                     color = type, linetype = factor(threshold))) +
      geom_line(aes(group = interaction(type, threshold)), size = 1) +
      geom_point(size = 2) +
      labs(title = paste("Average False Positive Rate -", model_name),
           x = "Window Size",
           y = "Average False Positive Rate",
           color = "Type",
           linetype = "Threshold") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Plot 2: Average TPR vs Window Size
    p2 <- ggplot(avg_performance, aes(x = window_size, y = avg_tpr, 
                                     color = type, linetype = factor(threshold))) +
      geom_line(aes(group = interaction(type, threshold)), size = 1) +
      geom_point(size = 2) +
      labs(title = paste("Average True Positive Rate -", model_name),
           x = "Window Size",
           y = "Average True Positive Rate",
           color = "Type",
           linetype = "Threshold") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Plot 3: FPR vs TPR scatter plot
    p3 <- ggplot(avg_performance, aes(x = avg_fpr, y = avg_tpr, 
                                     color = type, shape = factor(window_size))) +
      geom_point(size = 3, alpha = 0.8) +
      labs(title = paste("False Positive Rate vs True Positive Rate -", model_name),
           x = "Average False Positive Rate",
           y = "Average True Positive Rate",
           color = "Type",
           shape = "Window Size") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    return(list(fpr_plot = p1, tpr_plot = p2, scatter_plot = p3))
  }
  
  # Create comparison plots
  cat("Creating performance comparison plots...\n")
  
  magnitude_comparison <- create_performance_comparison(
    uni_results$magnitude, multi_results$magnitude, "Magnitude"
  )
  
  shape_comparison <- create_performance_comparison(
    uni_results$shape, multi_results$shape, "Shape"
  )
  
  partial_comparison <- create_performance_comparison(
    uni_results$partial, multi_results$partial, "Partial"
  )
  
  # Save comparison plots
  if (!is.null(magnitude_comparison)) {
    pdf("R/sliding_window/comprehensive_magnitude_comparison.pdf", 
        width = 15, height = 5)
    grid.arrange(
      magnitude_comparison$fpr_plot,
      magnitude_comparison$tpr_plot,
      magnitude_comparison$scatter_plot,
      ncol = 3
    )
    dev.off()
  }
  
  if (!is.null(shape_comparison)) {
    pdf("R/sliding_window/comprehensive_shape_comparison.pdf", 
        width = 15, height = 5)
    grid.arrange(
      shape_comparison$fpr_plot,
      shape_comparison$tpr_plot,
      shape_comparison$scatter_plot,
      ncol = 3
    )
    dev.off()
  }
  
  if (!is.null(partial_comparison)) {
    pdf("R/sliding_window/comprehensive_partial_comparison.pdf", 
        width = 15, height = 5)
    grid.arrange(
      partial_comparison$fpr_plot,
      partial_comparison$tpr_plot,
      partial_comparison$scatter_plot,
      ncol = 3
    )
    dev.off()
  }
  
  # Generate recommendations
  cat("\n=== HYPERPARAMETER RECOMMENDATIONS ===\n\n")
  
  generate_recommendations <- function(uni_opt, multi_opt, model_name) {
    
    if (is.null(uni_opt) || is.null(multi_opt)) {
      return(NULL)
    }
    
    cat(paste("RECOMMENDATIONS FOR", toupper(model_name), "MODEL:\n"))
    
    # Find best overall parameters for univariate
    uni_best <- uni_opt %>%
      mutate(score = true_positive_rate - false_positive_rate) %>%
      slice_max(score, n = 1)
    
    # Find best overall parameters for multivariate
    multi_best <- multi_opt %>%
      mutate(score = true_positive_rate - false_positive_rate) %>%
      slice_max(score, n = 1)
    
    cat("Univariate - Best overall performance:\n")
    cat("  Window size:", uni_best$window_size, "\n")
    cat("  Threshold:", uni_best$threshold, "\n")
    cat("  FPR:", round(uni_best$false_positive_rate, 4), "\n")
    cat("  TPR:", round(uni_best$true_positive_rate, 4), "\n")
    cat("  Score (TPR-FPR):", round(uni_best$score, 4), "\n\n")
    
    cat("Multivariate - Best overall performance:\n")
    cat("  Window size:", multi_best$window_size, "\n")
    cat("  Threshold:", multi_best$threshold, "\n")
    cat("  FPR:", round(multi_best$false_positive_rate, 4), "\n")
    cat("  TPR:", round(multi_best$true_positive_rate, 4), "\n")
    cat("  Score (TPR-FPR):", round(multi_best$score, 4), "\n\n")
    
    # General recommendations
    cat("General recommendations:\n")
    
    # Window size recommendations
    uni_window_mode <- names(sort(table(uni_opt$window_size), decreasing = TRUE))[1]
    multi_window_mode <- names(sort(table(multi_opt$window_size), decreasing = TRUE))[1]
    
    cat("  - Most common optimal window size (univariate):", uni_window_mode, "\n")
    cat("  - Most common optimal window size (multivariate):", multi_window_mode, "\n")
    
    # Threshold recommendations
    uni_threshold_mode <- names(sort(table(uni_opt$threshold), decreasing = TRUE))[1]
    multi_threshold_mode <- names(sort(table(multi_opt$threshold), decreasing = TRUE))[1]
    
    cat("  - Most common optimal threshold (univariate):", uni_threshold_mode, "\n")
    cat("  - Most common optimal threshold (multivariate):", multi_threshold_mode, "\n")
    
    cat("\n")
  }
  
  # Generate recommendations for each model
  generate_recommendations(uni_results$optimal$magnitude, multi_results$optimal$magnitude, "Magnitude")
  generate_recommendations(uni_results$optimal$shape, multi_results$optimal$shape, "Shape")
  generate_recommendations(uni_results$optimal$partial, multi_results$optimal$partial, "Partial")
  
  cat("Comprehensive analysis complete!\n")
  cat("Check the generated files:\n")
  cat("- comprehensive_magnitude_comparison.pdf\n")
  cat("- comprehensive_shape_comparison.pdf\n")
  cat("- comprehensive_partial_comparison.pdf\n")
  
  return(list(
    magnitude_summary = magnitude_summary,
    shape_summary = shape_summary,
    partial_summary = partial_summary,
    magnitude_comparison = magnitude_comparison,
    shape_comparison = shape_comparison,
    partial_comparison = partial_comparison
  ))
}

# Main execution
cat("Starting comprehensive hyperparameter analysis...\n")

# Check if both univariate and multivariate results exist
if (file.exists("R/sliding_window/hyperparameter_analysis_results.RData") && 
    file.exists("R/sliding_window/multivariate_hyperparameter_analysis_results.RData")) {
  
  cat("Both univariate and multivariate results found. Generating comprehensive report...\n")
  comprehensive_results <- generate_comprehensive_report()
  
} else {
  cat("Missing results files. Please run both univariate and multivariate analyses first.\n")
  cat("Run: source('R/sliding_window/run-hyperparameter-analysis.R')\n")
  cat("Then: source('R/sliding_window/multivariate-hyperparameter-analysis.R')\n")
}
