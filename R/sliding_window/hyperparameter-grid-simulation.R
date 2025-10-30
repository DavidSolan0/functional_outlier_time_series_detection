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
                        "shape", "partial"))

# Load global functions
source("R/utils.R")
source("R/simulated-models.R")
source("R/depths.R")
source("R/calculate_rates_results.R")

# Load local functions
source("R/sliding_window/outlier-detection-procedures.R")
source("R/sliding_window/power-study-functions.R")

# Define the missing function if it doesn't exist
if (!exists("calculate_sliding_window_rates")) {
  calculate_sliding_window_rates <- function(...) {
    # This function should be defined in power-study-functions.R
    # If it's missing, we'll define a placeholder
    stop("calculate_sliding_window_rates function not found. Please check power-study-functions.R")
  }
}

# Function to run hyperparameter grid simulation
run_hyperparameter_grid_simulation <- function(
    window_sizes = c(4, 6, 8, 10, 12, 16),
    thresholds = c(0.3, 0.4, 0.5, 0.6, 0.7),
    K_values = c(10, 15, 20, 25),
    rho = 0.8,
    model = magnitude,
    M = 50,  # Reduced for faster computation
    dfunc = MBD,
    multivariate = FALSE,
    seed = 1234) {
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Create grid of hyperparameters
  param_grid <- expand.grid(
    window_size = window_sizes,
    threshold = thresholds,
    K = K_values,
    stringsAsFactors = FALSE
  )
  
  # Initialize results data frame
  results <- data.frame()
  
  # Progress tracking
  total_combinations <- nrow(param_grid)
  cat("Running", total_combinations, "hyperparameter combinations...\n")
  
  for (i in seq_len(nrow(param_grid))) {
    window_size <- param_grid$window_size[i]
    threshold <- param_grid$threshold[i]
    K <- param_grid$K[i]
    
    cat("Progress:", i, "/", total_combinations, 
        "- Window size:", window_size, 
        ", Threshold:", threshold, 
        ", K:", K, "\n")
    
    # Run simulation for this parameter combination
    tryCatch({
      rates <- calculate_sliding_window_rates(
        rho = rho,
        k = K,
        model = model,
        window_size = window_size,
        M = M,
        dfunc = dfunc,
        multivariate = multivariate,
        threshold = threshold
      )
      
      # Add results to data frame
      results <- rbind(results, data.frame(
        window_size = window_size,
        threshold = threshold,
        K = K,
        false_positive_rate = rates$false_positive_rate,
        true_positive_rate = rates$true_positive_rate,
        sd_true_positive_rate = rates$sd_true_positive_rate,
        true_positive_rate_zero_clean = rates$true_positive_rate_zero_clean,
        stringsAsFactors = FALSE
      ))
      
    }, error = function(e) {
      cat("Error for window_size =", window_size, 
          ", threshold =", threshold, 
          ", K =", K, ":", e$message, "\n")
      
      # Add NA results for failed combinations
      results <<- rbind(results, data.frame(
        window_size = window_size,
        threshold = threshold,
        K = K,
        false_positive_rate = NA,
        true_positive_rate = NA,
        sd_true_positive_rate = NA,
        true_positive_rate_zero_clean = NA,
        stringsAsFactors = FALSE
      ))
    })
  }
  
  return(results)
}

# Function to create performance visualizations
create_performance_plots <- function(results, model_name = "Magnitude") {
  
  # Remove rows with NA values
  results_clean <- results[!is.na(results$false_positive_rate), ]
  
  if (nrow(results_clean) == 0) {
    cat("No valid results to plot\n")
    return(NULL)
  }
  
  # Plot 1: False Positive Rate vs Window Size by Threshold
  p1 <- ggplot(results_clean, aes(x = window_size, y = false_positive_rate, 
                                  color = factor(threshold))) +
    geom_line(aes(group = interaction(threshold, K)), alpha = 0.7) +
    geom_point(aes(size = K), alpha = 0.8) +
    facet_wrap(~K, scales = "free_y") +
    labs(title = paste("False Positive Rate vs Window Size -", model_name),
         x = "Window Size",
         y = "False Positive Rate",
         color = "Threshold",
         size = "K") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Plot 2: True Positive Rate vs Window Size by Threshold
  p2 <- ggplot(results_clean, aes(x = window_size, y = true_positive_rate, 
                                  color = factor(threshold))) +
    geom_line(aes(group = interaction(threshold, K)), alpha = 0.7) +
    geom_point(aes(size = K), alpha = 0.8) +
    facet_wrap(~K, scales = "free_y") +
    labs(title = paste("True Positive Rate vs Window Size -", model_name),
         x = "Window Size",
         y = "True Positive Rate",
         color = "Threshold",
         size = "K") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Plot 3: Heatmap of False Positive Rate
  p3 <- ggplot(results_clean, aes(x = factor(window_size), y = factor(threshold), 
                                  fill = false_positive_rate)) +
    geom_tile() +
    facet_wrap(~K) +
    scale_fill_viridis_c(name = "FPR") +
    labs(title = paste("False Positive Rate Heatmap -", model_name),
         x = "Window Size",
         y = "Threshold") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 4: Heatmap of True Positive Rate
  p4 <- ggplot(results_clean, aes(x = factor(window_size), y = factor(threshold), 
                                  fill = true_positive_rate)) +
    geom_tile() +
    facet_wrap(~K) +
    scale_fill_viridis_c(name = "TPR") +
    labs(title = paste("True Positive Rate Heatmap -", model_name),
         x = "Window Size",
         y = "Threshold") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(
    fpr_vs_window = p1,
    tpr_vs_window = p2,
    fpr_heatmap = p3,
    tpr_heatmap = p4
  ))
}

# Function to find optimal hyperparameters
find_optimal_hyperparameters <- function(results, metric = "balanced") {
  
  # Remove rows with NA values
  results_clean <- results[!is.na(results$false_positive_rate), ]
  
  if (nrow(results_clean) == 0) {
    return(NULL)
  }
  
  if (metric == "balanced") {
    # Balance between high TPR and low FPR
    results_clean$score <- results_clean$true_positive_rate - results_clean$false_positive_rate
  } else if (metric == "fpr_focused") {
    # Focus on minimizing FPR
    results_clean$score <- -results_clean$false_positive_rate
  } else if (metric == "tpr_focused") {
    # Focus on maximizing TPR
    results_clean$score <- results_clean$true_positive_rate
  }
  
  # Find optimal parameters for each K
  optimal_params <- results_clean %>%
    group_by(K) %>%
    slice_max(score, n = 1) %>%
    ungroup()
  
  return(optimal_params)
}

# Function to create summary tables
create_summary_tables <- function(results) {
  
  # Remove rows with NA values
  results_clean <- results[!is.na(results$false_positive_rate), ]
  
  if (nrow(results_clean) == 0) {
    return(NULL)
  }
  
  # Summary by window size
  window_summary <- results_clean %>%
    group_by(window_size) %>%
    summarise(
      mean_fpr = mean(false_positive_rate, na.rm = TRUE),
      mean_tpr = mean(true_positive_rate, na.rm = TRUE),
      sd_fpr = sd(false_positive_rate, na.rm = TRUE),
      sd_tpr = sd(true_positive_rate, na.rm = TRUE),
      n_combinations = n(),
      .groups = 'drop'
    ) %>%
    arrange(mean_fpr)
  
  # Summary by threshold
  threshold_summary <- results_clean %>%
    group_by(threshold) %>%
    summarise(
      mean_fpr = mean(false_positive_rate, na.rm = TRUE),
      mean_tpr = mean(true_positive_rate, na.rm = TRUE),
      sd_fpr = sd(false_positive_rate, na.rm = TRUE),
      sd_tpr = sd(true_positive_rate, na.rm = TRUE),
      n_combinations = n(),
      .groups = 'drop'
    ) %>%
    arrange(mean_fpr)
  
  # Summary by K
  k_summary <- results_clean %>%
    group_by(K) %>%
    summarise(
      mean_fpr = mean(false_positive_rate, na.rm = TRUE),
      mean_tpr = mean(true_positive_rate, na.rm = TRUE),
      sd_fpr = sd(false_positive_rate, na.rm = TRUE),
      sd_tpr = sd(true_positive_rate, na.rm = TRUE),
      n_combinations = n(),
      .groups = 'drop'
    ) %>%
    arrange(K)
  
  return(list(
    window_summary = window_summary,
    threshold_summary = threshold_summary,
    k_summary = k_summary
  ))
}

# Main execution function
run_complete_hyperparameter_analysis <- function() {
  
  cat("=== SLIDING WINDOW HYPERPARAMETER ANALYSIS ===\n\n")
  
  # Define parameter ranges
  window_sizes <- c(4, 6, 8, 10, 12, 16)
  thresholds <- c(0.3, 0.4, 0.5, 0.6, 0.7)
  K_values <- c(10, 15, 20, 25)
  
  # Run simulation for univariate magnitude model
  cat("Running simulation for univariate magnitude model...\n")
  results_magnitude <- run_hyperparameter_grid_simulation(
    window_sizes = window_sizes,
    thresholds = thresholds,
    K_values = K_values,
    rho = 0.8,
    model = magnitude,
    M = 50,
    dfunc = MBD,
    multivariate = FALSE,
    seed = 1234
  )
  
  # Run simulation for univariate shape model
  cat("\nRunning simulation for univariate shape model...\n")
  results_shape <- run_hyperparameter_grid_simulation(
    window_sizes = window_sizes,
    thresholds = thresholds,
    K_values = K_values,
    rho = 0.8,
    model = shape,
    M = 50,
    dfunc = MBD,
    multivariate = FALSE,
    seed = 1234
  )
  
  # Run simulation for univariate partial model
  cat("\nRunning simulation for univariate partial model...\n")
  results_partial <- run_hyperparameter_grid_simulation(
    window_sizes = window_sizes,
    thresholds = thresholds,
    K_values = K_values,
    rho = 0.8,
    model = partial,
    M = 50,
    dfunc = MBD,
    multivariate = FALSE,
    seed = 1234
  )
  
  # Create visualizations
  cat("\nCreating visualizations...\n")
  plots_magnitude <- create_performance_plots(results_magnitude, "Magnitude")
  plots_shape <- create_performance_plots(results_shape, "Shape")
  plots_partial <- create_performance_plots(results_partial, "Partial")
  
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
       file = "R/sliding_window/hyperparameter_analysis_results.RData")
  
  cat("\nAnalysis complete! Results saved to hyperparameter_analysis_results.RData\n")
  
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

# Quick test function for smaller grid
run_quick_test <- function() {
  cat("=== QUICK HYPERPARAMETER TEST ===\n")
  
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
    dfunc = MBD,
    multivariate = FALSE,
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
