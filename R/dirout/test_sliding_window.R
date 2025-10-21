# Test script for sliding window DirOut outlier detection
# This script demonstrates the new sliding window approach

# Load required libraries
library(fda.usc)
# library(DepthProc)

# Source the functions
source("R/utils.R")
source("R/dirout/DirOut.R")
source("R/dirout/bootstrap-procedures.R")
source("R/dirout/outlier-detection-procedures.R")


# Load the simulated models
source("R/simulated-models.R")

# Generate data using the magnitude model with known outliers
# set.seed(1234)
cat("Generating functional time series with magnitude outliers...\n")
model_result <- magnitude(rho = 0.8, k = 25, plot = TRUE, t = c(-0.5, 1.5))

# Extract the functional data object and true outliers
fdataobj <- model_result$fdataobj
true_outliers <- model_result$outliers
true_outliers

cat("True outliers in the data:", true_outliers, "\n")
cat("Data dimensions:", nrow(fdataobj), "curves,", ncol(fdataobj), "time points\n")

# Test the new sliding window approach
cat("Testing sliding window DirOut outlier detection...\n")

# Run sliding window outlier detection
result_sliding <- sliding_window_outlier_dirout(
  fdataobj = fdataobj,
  window_size = 8,
  nb = 50, # Reduced for faster testing
  quan = 0.5,
  ns = 0.99,
  dfunc = "RP",
  plot = TRUE
)

# Print results
cat("Sliding Window Results:\n")
detected_sliding <- as.numeric(result_sliding$outliers)
cat("Detected outliers:", detected_sliding, "\n")
cat("Cutoff for average directional outlyingness:", result_sliding$q_avr, "\n")
cat("Cutoff for variance of directional outlyingness:", result_sliding$q_var, "\n")

# Compare with traditional approach
cat("\nComparing with traditional MBBo approach...\n")
result_traditional <- outlier_dirout(
  fdataobj = fdataobj,
  nb = 50, # Reduced for faster testing
  quan = 0.5,
  l = 4,
  ns = 0.99,
  dfunc = "RP",
  plot = TRUE
)

cat("Traditional Results:\n")
detected_traditional <- as.numeric(result_traditional$outliers)
cat("Detected outliers:", detected_traditional, "\n")

# Calculate performance metrics
cat("\n=== PERFORMANCE EVALUATION ===\n")

# Function to calculate precision, recall, and F1 score
calculate_metrics <- function(detected, true_outliers, total_obs) {
  if (length(detected) == 0) {
    return(list(precision = 0, recall = 0, f1 = 0, false_positives = 0, false_negatives = length(true_outliers)))
  }

  true_positives <- sum(detected %in% true_outliers)
  false_positives <- sum(!(detected %in% true_outliers))
  false_negatives <- sum(!(true_outliers %in% detected))

  precision <- true_positives / (true_positives + false_positives)
  recall <- true_positives / (true_positives + false_negatives)
  f1 <- if (precision + recall > 0) 2 * precision * recall / (precision + recall) else 0

  return(list(
    precision = precision,
    recall = recall,
    f1 = f1,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives
  ))
}

# Calculate metrics for both methods
metrics_sliding <- calculate_metrics(detected_sliding, true_outliers, nrow(fdataobj))
metrics_traditional <- calculate_metrics(detected_traditional, true_outliers, nrow(fdataobj))

cat("Sliding Window Method:\n")
cat(sprintf("  Precision: %.3f\n", metrics_sliding$precision))
cat(sprintf("  Recall: %.3f\n", metrics_sliding$recall))
cat(sprintf("  F1 Score: %.3f\n", metrics_sliding$f1))
cat(sprintf("  True Positives: %d\n", metrics_sliding$true_positives))
cat(sprintf("  False Positives: %d\n", metrics_sliding$false_positives))
cat(sprintf("  False Negatives: %d\n", metrics_sliding$false_negatives))

cat("\nTraditional MBBo Method:\n")
cat(sprintf("  Precision: %.3f\n", metrics_traditional$precision))
cat(sprintf("  Recall: %.3f\n", metrics_traditional$recall))
cat(sprintf("  F1 Score: %.3f\n", metrics_traditional$f1))
cat(sprintf("  True Positives: %d\n", metrics_traditional$true_positives))
cat(sprintf("  False Positives: %d\n", metrics_traditional$false_positives))
cat(sprintf("  False Negatives: %d\n", metrics_traditional$false_negatives))

# Analyze directional outlyingness values
if (length(result_sliding$d_avr_all) > 0) {
  cat("\nDirectional outlyingness analysis:\n")
  cat("Observations with highest directional outlyingness:\n")

  # Find top 5 observations with highest directional outlyingness
  top_outliers_avr <- order(result_sliding$d_avr_all, decreasing = TRUE)[1:min(5, length(result_sliding$d_avr_all))]
  top_outliers_var <- order(result_sliding$d_var_all, decreasing = TRUE)[1:min(5, length(result_sliding$d_var_all))]

  cat("Top outliers by average directional outlyingness:\n")
  for (i in top_outliers_avr) {
    cat(sprintf(
      "  Observation %d: %.3f (cutoff: %.3f)\n",
      i, result_sliding$d_avr_all[i], result_sliding$q_avr
    ))
  }

  cat("Top outliers by variance of directional outlyingness:\n")
  for (i in top_outliers_var) {
    cat(sprintf(
      "  Observation %d: %.3f (cutoff: %.3f)\n",
      i, result_sliding$d_var_all[i], result_sliding$q_var
    ))
  }
}

# Create a comparison plot
par(mfrow = c(2, 2))

# Plot 1: Original data with highlighted true outliers
plot(fdataobj, col = "grey", main = "Magnitude Model Data")
lines(fdataobj[true_outliers], col = "black", lwd = 2)
for (i in true_outliers) {
  points(i, fdataobj[i, 1], col = "red", pch = 19, cex = 1.5)
}

# Plot 2: Average directional outlyingness
plot(result_sliding$d_avr_all,
  main = "Average Directional Outlyingness",
  xlab = "Observation Index",
  ylab = "Directional Outlyingness",
  col = "steelblue", pch = 19
)
abline(h = result_sliding$q_avr, col = "red", lty = 2)
# Highlight true outliers
for (i in true_outliers) {
  points(i, result_sliding$d_avr_all[i],
    col = "red", pch = 19, cex = 1.5
  )
}
# Highlight detected outliers
if (length(detected_sliding) > 0) {
  points(detected_sliding, result_sliding$d_avr_all[detected_sliding],
    col = "blue", pch = 17, cex = 1.2
  )
}

# Plot 3: Variance of directional outlyingness
plot(result_sliding$d_var_all,
  main = "Variance of Directional Outlyingness",
  xlab = "Observation Index",
  ylab = "Directional Outlyingness",
  col = "steelblue", pch = 19
)
abline(h = result_sliding$q_var, col = "red", lty = 2)
# Highlight true outliers
for (i in true_outliers) {
  points(i, result_sliding$d_var_all[i],
    col = "red", pch = 19, cex = 1.5
  )
}
# Highlight detected outliers
if (length(detected_sliding) > 0) {
  points(detected_sliding, result_sliding$d_var_all[detected_sliding],
    col = "blue", pch = 17, cex = 1.2
  )
}

# Plot 4: Comparison of detection results
plot(1:nrow(fdataobj), rep(0, nrow(fdataobj)),
  ylim = c(-0.5, 2.5),
  main = "Detection Results Comparison",
  xlab = "Observation Index",
  ylab = "Method",
  yaxt = "n"
)
axis(2, at = c(0, 1, 2), labels = c("None", "Traditional", "Sliding Window"))
# Mark true outliers
for (i in true_outliers) {
  points(i, 0, col = "red", pch = 19, cex = 1.5)
}
# Mark traditional detections
if (length(detected_traditional) > 0) {
  points(detected_traditional, rep(1, length(detected_traditional)),
    col = "blue", pch = 17, cex = 1.2
  )
}
# Mark sliding window detections
if (length(detected_sliding) > 0) {
  points(detected_sliding, rep(2, length(detected_sliding)),
    col = "green", pch = 15, cex = 1.2
  )
}

par(mfrow = c(1, 1))

cat("\n=== FINAL SUMMARY ===\n")
cat("True outliers:", true_outliers, "\n")
cat("Sliding window detected:", detected_sliding, "\n")
cat("Traditional MBBo detected:", detected_traditional, "\n")

# Determine which method performed better
if (metrics_sliding$f1 > metrics_traditional$f1) {
  cat("\n✓ Sliding window method performed better (F1:", metrics_sliding$f1, "vs", metrics_traditional$f1, ")\n")
} else if (metrics_traditional$f1 > metrics_sliding$f1) {
  cat("\n✓ Traditional MBBo method performed better (F1:", metrics_traditional$f1, "vs", metrics_sliding$f1, ")\n")
} else {
  cat("\n= Both methods performed equally well (F1:", metrics_sliding$f1, ")\n")
}

cat("\nTest completed successfully!\n")
cat("The sliding window approach should better capture outliers in functional time series\n")
cat("by considering the temporal structure and avoiding the masking effect of concatenated blocks.\n")
cat("\nKey advantages of sliding window approach:\n")
cat("- Preserves temporal structure of functional time series\n")
cat("- Reduces masking effect of concatenated blocks\n")
cat("- Provides frequency-based outlier detection\n")
cat("- More robust to temporal dependencies\n")
