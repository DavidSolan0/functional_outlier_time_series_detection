library(fda)
library(roahd)
library(fda.usc)
library(fdaoutlier)

# Load global functions
source("R/simulated-models.R")

set.seed(1234)
model <- shape
K <- c(4, 5, 6, 7)
M <- 100

# Store results for each K and each simulation
results <- list()

for (k in K) {
  false_positive_vector <- numeric(M)
  true_positive_vector <- numeric(M)

  for (m in 1:M) {
    fit <- model(rho = 0.8, k = k, plot = FALSE)
    fdata <- fit$fdataobj$data
    ndata <- nrow(fdata)

    # Generated outliers (indexes)
    generated_outliers <- fit$outliers

    # Apply the outlier detection (msplot in this case)
    msplot_object <- msplot(fdata, plot = FALSE)
    detected_outliers <- msplot_object$outliers

    # Ensure detected_outliers are numeric indexes
    detected_outliers <- as.numeric(detected_outliers)

    # If no outliers detected, set rates to 0
    if (length(detected_outliers) == 0) {
      false_positive_vector[m] <- 0
      true_positive_vector[m] <- 0
    } else {
      is_true_positive <- detected_outliers %in% generated_outliers
      true_positive_count <- sum(is_true_positive)
      false_positive_count <- sum(!is_true_positive)

      denominator <- ndata - length(generated_outliers)
      # Handle 0 denominator case robustly
      false_positive_rate <- if (denominator > 0) false_positive_count / denominator else NA
      true_positive_rate <- true_positive_count / length(generated_outliers)

      false_positive_vector[m] <- false_positive_rate
      true_positive_vector[m] <- true_positive_rate
    }
  }

  # Store summary statistics for current k
  mean_false_positive <- mean(false_positive_vector, na.rm = TRUE)
  mean_true_positive <- mean(true_positive_vector, na.rm = TRUE)
  sd_true_positive <- sd(true_positive_vector, na.rm = TRUE)
  true_positive_zero_clean <- mean(true_positive_vector != 0, na.rm = TRUE)

  results[[as.character(k)]] <- list(
    false_positive_rate = mean_false_positive,
    true_positive_rate = mean_true_positive,
    sd_true_positive_rate = sd_true_positive,
    true_positive_rate_zero_clean = true_positive_zero_clean,
    false_positive_vector = false_positive_vector,
    true_positive_vector = true_positive_vector
  )
}

# To view summary table, e.g.
summary_table <- do.call(rbind, lapply(results, function(x)
  unlist(x[c("false_positive_rate", "true_positive_rate", "sd_true_positive_rate", "true_positive_rate_zero_clean")])
))
rownames(summary_table) <- paste0("k=", K)
print(summary_table)
