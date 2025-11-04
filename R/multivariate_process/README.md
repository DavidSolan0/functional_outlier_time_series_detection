# Detection of outliers in multivariate functional time series

This directory contains the implementation of outlier detection methods for **multivariate functional time series data** using bootstrap procedures. This is the multivariate extension of the methods described in the [original_proposal](../original_proposal/) directory.

## Overview

The multivariate functional outlier detection methods extend the univariate bootstrap-based approaches to handle multiple functional variables simultaneously. These methods:

1. Use multivariate functional depth measures (e.g., `multiMBD`) that combine information across all variables
2. Support weighted combinations of variables to emphasize certain components
3. Apply moving block bootstrap (MBBo) to preserve temporal dependence structure
4. Implement iterative outlier removal procedures

**Key difference from univariate methods**: The multivariate approach considers the joint behavior across all variables when detecting outliers, which can identify outliers that might not be apparent when analyzing variables separately.

## Prerequisites

Before using these functions, ensure you have the required packages loaded:

```r
library(fda)
library(roahd)
library(fda.usc)
library(mrfDepth)
```

Also, make sure to source the necessary files:

```r
source("R/utils.R")
source("R/simulated-models.R")  # Required for data generation
source("R/depths.R")             # Required for multiMBD depth function
source("R/multivariate_process/bootstrap-procedures.R")
source("R/multivariate_process/outlier-detection-procedures.R")
```

## Data Structure

Multivariate functional data should be provided as a **list**, where each element is a functional data object (or matrix) representing one variable. All variables must have:
- The same number of observations (rows)
- Consistent row names (if provided)
- The same time grid (columns)

### Example Data Structure

```r
# Create multivariate functional data
n_obs <- 100
n_time <- 50
n_vars <- 3  # Number of variables

# Option 1: List of fdata objects (recommended)
mfdataobj <- list()
for (v in 1:n_vars) {
  data_matrix <- matrix(rnorm(n_obs * n_time), nrow = n_obs, ncol = n_time)
  mfdataobj[[v]] <- fdata(data_matrix)
}

# Option 2: List of matrices (also works)
mfdataobj <- list(
  variable1 = matrix(rnorm(n_obs * n_time), nrow = n_obs, ncol = n_time),
  variable2 = matrix(rnorm(n_obs * n_time), nrow = n_obs, ncol = n_time),
  variable3 = matrix(rnorm(n_obs * n_time), nrow = n_obs, ncol = n_time)
)
```

## Main Functions

### 1. `outlier_multivariate_bootstrap()` - Bootstrap-based Outlier Detection

This function implements outlier detection for multivariate functional time series using bootstrap procedures, following the same iterative algorithm as the univariate version.

#### Function Signature

```r
outlier_multivariate_bootstrap(
    mfdataobj,
    nb = 200,
    smo = 0.05,
    quan = 0.5,
    dfunc = multiMBD,
    l = 4,
    p = 0.1,
    ns = 0.01,
    boot = multiMBBo,
    weights = c(1/3, 1/3, 1/3)
)
```

#### Parameters

* **`mfdataobj`**: List containing multivariate functional data objects. Each element should be an `fdata` object or matrix representing one variable. All variables must have the same number of observations (rows).
* **`nb`**: Number of bootstrap samples to generate (default: 200). More samples provide more stable cutoff estimates but increase computation time.
* **`smo`**: Smoothness parameter (default: 0.05). Currently not actively used but kept for compatibility with function signature.
* **`quan`**: Quantile used for cutoff estimation from bootstrap distribution (default: 0.5, i.e., median). Values between 0 and 1.
* **`dfunc`**: Depth function to use for outlier detection (default: `multiMBD`). Must be a multivariate depth function that:
    - Accepts a list of functional data objects
    - Accepts a `weights` parameter
    - Returns a vector of depth values (one per observation)
    - Common options: `multiMBD` (Multivariate Modified Band Depth)
* **`l`**: Block size for moving block bootstrap (default: 4). Should be chosen based on the temporal dependence structure of your data.
* **`p`**: Success probability for geometric distribution (default: 0.1). Currently only `multiMBBo` is implemented, so this parameter is not actively used.
* **`ns`**: Quantile to calculate in each bootstrap iteration (default: 0.01). This is the quantile of depth values used within each bootstrap sample to estimate the cutoff.
* **`boot`**: Bootstrap procedure to use (default: `multiMBBo`). Currently only `multiMBBo` (Moving Block Bootstrap for multivariate data) is implemented.
* **`weights`**: Vector of weights for each univariate process/variable (default: `c(1/3, 1/3, 1/3)` for equal weights). 
    - Must have length equal to the number of variables
    - Weights should sum to 1 (or will be normalized)
    - Higher weights emphasize certain variables in the depth calculation
    - Example: `c(0.5, 0.3, 0.2)` gives more weight to the first variable

#### Return Value

The function returns a list with the following components:

* **`outliers`**: Vector of detected outlier indices (as row names from the original data)
* **`outlier_depths`**: Vector of depth values for the detected outliers
* **`iteration`**: Vector indicating the iteration when each outlier was found
* **`quantile`**: Estimated cutoff value used for outlier detection
* **`Dep`**: Vector of depth values for all curves (after iterative process)

#### Example Usage

```r
# Prepare multivariate functional data
n_obs <- 100
n_time <- 50
n_vars <- 3

mfdataobj <- list()
for (v in 1:n_vars) {
  data_matrix <- matrix(rnorm(n_obs * n_time), nrow = n_obs, ncol = n_time)
  mfdataobj[[v]] <- fdata(data_matrix)
}

# Run outlier detection with equal weights
result <- outlier_multivariate_bootstrap(
    mfdataobj = mfdataobj,
    nb = 200,
    quan = 0.5,
    dfunc = multiMBD,
    l = 4,
    ns = 0.01,
    boot = multiMBBo,
    weights = c(1/3, 1/3, 1/3)  # Equal weights
)

# Access results
print(paste("Number of outliers detected:", length(result$outliers)))
print(paste("Outlier indices:", result$outliers))
print(paste("Cutoff value:", result$quantile))
```

### 2. `multivariate_functional_boxplot()` - Functional Boxplot Method

This function implements outlier detection using functional boxplots for multivariate functional data.

#### Function Signature

```r
multivariate_functional_boxplot(
    mfdataobj,
    dfunc = multiMBD,
    weights = NULL,
    ...
)
```

#### Parameters

* **`mfdataobj`**: List containing multivariate functional data objects
* **`dfunc`**: Depth function to use (default: `multiMBD`)
* **`weights`**: Vector of weights for each variable (default: `NULL`, which sets equal weights automatically)

#### Return Value

The function returns a list with:
* **`outliers`**: Vector of detected outlier indices

#### Example Usage

```r
# Run functional boxplot outlier detection
result <- multivariate_functional_boxplot(
    mfdataobj = mfdataobj,
    dfunc = multiMBD,
    weights = c(1/3, 1/3, 1/3)
)

print(paste("Number of outliers detected:", length(result$outliers)))
```

## Bootstrap Methods

### multiMBBo (Multivariate Moving Block Bootstrap)

**When to use**: Recommended for multivariate functional time series with temporal dependence.

**How it works**:
1. Calculates multivariate functional depth for all observations
2. Removes outliers using a naive approach (functional boxplot) to get a trimmed sample
3. Divides the time series into overlapping blocks of size `l`
4. Samples blocks with replacement to create bootstrap samples (preserving temporal structure across all variables)
5. Calculates depth quantiles from bootstrap samples to estimate cutoff

**Key features**:
- Preserves temporal dependence structure across all variables
- Uses the same block indices for all variables, maintaining multivariate structure
- Supports weighted depth calculations

**Example usage**:

```r
# Prepare multivariate functional time series data
mfdataobj <- list(
  var1 = fdata(matrix(rnorm(100*50), nrow = 100, ncol = 50)),
  var2 = fdata(matrix(rnorm(100*50), nrow = 100, ncol = 50)),
  var3 = fdata(matrix(rnorm(100*50), nrow = 100, ncol = 50))
)

# Run outlier detection with multiMBBo
result <- outlier_multivariate_bootstrap(
    mfdataobj = mfdataobj,
    nb = 200,
    quan = 0.5,
    dfunc = multiMBD,
    l = 4,
    ns = 0.01,
    boot = multiMBBo,
    weights = c(1/3, 1/3, 1/3)
)
```

**Key parameters for multiMBBo**:
* `l`: Block size. Should reflect the temporal dependence structure:
    - Small values (2-4): For weak dependence
    - Medium values (5-10): For moderate dependence
    - Large values (10+): For strong dependence
* `nb`: Number of bootstrap samples. Increase for more stable estimates.
* `weights`: Variable weights. Use equal weights if all variables are equally important, or adjust based on domain knowledge.

## Understanding Weights

The `weights` parameter allows you to control the relative importance of each variable in the multivariate depth calculation:

### Equal Weights (Default)

```r
# All variables equally important
weights = c(1/3, 1/3, 1/3)  # For 3 variables
weights = c(0.25, 0.25, 0.25, 0.25)  # For 4 variables
```

### Unequal Weights

```r
# Emphasize first variable
weights = c(0.5, 0.3, 0.2)

# Emphasize last variable
weights = c(0.2, 0.3, 0.5)

# Single variable focus (weight one variable heavily)
weights = c(0.8, 0.1, 0.1)
```

### When to Adjust Weights

- **Domain knowledge**: If certain variables are more important for outlier detection
- **Variable quality**: If some variables are more reliable or have less noise
- **Outlier patterns**: If outliers tend to manifest more strongly in certain variables
- **Experimental tuning**: Try different weight combinations and compare results

**Note**: Weights should generally sum to 1, though the function may normalize them automatically.

## Complete Examples

### Example 1: Basic Multivariate Outlier Detection

```r
# Load required libraries
library(fda)
library(roahd)
library(fda.usc)
library(mrfDepth)

# Source required files
source("R/utils.R")
source("R/simulated-models.R")
source("R/depths.R")
source("R/multivariate_process/bootstrap-procedures.R")
source("R/multivariate_process/outlier-detection-procedures.R")

# Create multivariate functional data
set.seed(123)
n_obs <- 100
n_time <- 50
n_vars <- 3

mfdataobj <- list()
for (v in 1:n_vars) {
  data_matrix <- matrix(rnorm(n_obs * n_time), 
                       nrow = n_obs, 
                       ncol = n_time)
  mfdataobj[[v]] <- fdata(data_matrix)
}

# Add row names
for (v in 1:n_vars) {
  row.names(mfdataobj[[v]]) <- 1:n_obs
}

# Run outlier detection
result <- outlier_multivariate_bootstrap(
    mfdataobj = mfdataobj,
    nb = 200,
    quan = 0.5,
    dfunc = multiMBD,
    l = 4,
    ns = 0.01,
    boot = multiMBBo,
    weights = c(1/3, 1/3, 1/3)
)

# Analyze results
cat("Total outliers detected:", length(result$outliers), "\n")
cat("Outlier indices:", result$outliers, "\n")
cat("Cutoff value:", result$quantile, "\n")
cat("Outlier depths:", result$outlier_depths, "\n")
```

### Example 2: Using Simulated Models

```r
# Use built-in simulation models
set.seed(1234)

# Generate multivariate functional data with contamination
fit <- multifdata(
    rho = 0.8,
    k = 10,  # Contamination level
    model = magnitude,  # Magnitude outliers
    plot = FALSE,
    dim = 3  # 3 variables
)

mfdataobj <- fit$mfdataobj
generated_outliers <- fit$outliers

# Run outlier detection
result <- outlier_multivariate_bootstrap(
    mfdataobj = mfdataobj,
    nb = 200,
    quan = 0.5,
    dfunc = multiMBD,
    l = 4,
    ns = 0.01,
    boot = multiMBBo,
    weights = c(1/3, 1/3, 1/3)
)

# Compare with known outliers
detected_outliers <- as.numeric(result$outliers)
true_positives <- sum(detected_outliers %in% generated_outliers)
false_positives <- sum(!(detected_outliers %in% generated_outliers))

cat("True positives:", true_positives, "out of", length(generated_outliers), "\n")
cat("False positives:", false_positives, "\n")
```

### Example 3: Comparing Different Weight Schemes

```r
# Test different weight schemes
weight_schemes <- list(
  equal = c(1/3, 1/3, 1/3),
  emphasize_first = c(0.6, 0.2, 0.2),
  emphasize_last = c(0.2, 0.2, 0.6)
)

results_list <- list()

for (scheme_name in names(weight_schemes)) {
  result <- outlier_multivariate_bootstrap(
      mfdataobj = mfdataobj,
      nb = 200,
      quan = 0.5,
      dfunc = multiMBD,
      l = 4,
      ns = 0.01,
      boot = multiMBBo,
      weights = weight_schemes[[scheme_name]]
  )
  
  results_list[[scheme_name]] <- result
  cat(scheme_name, "weights detected", length(result$outliers), "outliers\n")
}

# Compare outlier sets
if (length(results_list) > 1) {
  outlier_sets <- lapply(results_list, function(r) r$outliers)
  common_outliers <- Reduce(intersect, outlier_sets)
  cat("Common outliers across all weight schemes:", length(common_outliers), "\n")
}
```

### Example 4: Functional Boxplot Method

```r
# Run functional boxplot outlier detection
result_fbplot <- multivariate_functional_boxplot(
    mfdataobj = mfdataobj,
    dfunc = multiMBD,
    weights = c(1/3, 1/3, 1/3)
)

# Compare with bootstrap method
result_bootstrap <- outlier_multivariate_bootstrap(
    mfdataobj = mfdataobj,
    nb = 200,
    quan = 0.5,
    dfunc = multiMBD,
    l = 4,
    boot = multiMBBo,
    weights = c(1/3, 1/3, 1/3)
)

cat("Functional boxplot detected", length(result_fbplot$outliers), "outliers\n")
cat("Bootstrap method detected", length(result_bootstrap$outliers), "outliers\n")
```

## Parameter Tuning Guidelines

* **`nb`**: Start with 200. Increase to 500-1000 for more stable results, especially with small sample sizes.
* **`quan`**: 
    - 0.5 (median): Balanced approach
    - Lower values (0.25): More conservative (fewer outliers)
    - Higher values (0.75): More liberal (more outliers)
* **`ns`**: Typically kept at 0.01. This is the quantile used within each bootstrap iteration.
* **`l`**: Block size for multiMBBo. Choose based on temporal dependence:
    - Weak dependence: `l = 2-4`
    - Moderate dependence: `l = 5-10`
    - Strong dependence: `l = 10+`
* **`weights`**: 
    - Start with equal weights: `rep(1/n_vars, n_vars)`
    - Adjust based on domain knowledge or experimental results
    - Consider the relative importance and reliability of each variable
* **`dfunc`**: Currently `multiMBD` is the default and recommended choice.

## Differences from Univariate Methods

| Aspect | Univariate | Multivariate |
|--------|------------|--------------|
| **Data structure** | Single `fdata` object or matrix | List of `fdata` objects/matrices |
| **Depth function** | `MBD`, `MD`, etc. | `multiMBD` (multivariate depth) |
| **Weights** | Not applicable | Can weight variables |
| **Bootstrap** | `MBBo`, `SmBoD`, `StBo` | `multiMBBo` (only) |
| **Outlier detection** | Based on single variable | Based on joint behavior across variables |

## Advantages of Multivariate Approach

1. **Joint detection**: Can identify outliers that are not apparent in individual variables
2. **Correlation structure**: Considers relationships between variables
3. **Weighted emphasis**: Allows prioritizing certain variables
4. **Temporal coherence**: Preserves temporal structure across all variables simultaneously

## Limitations

1. **Computational cost**: More expensive than univariate methods (multiple variables)
2. **Weight selection**: Requires careful consideration or experimentation
3. **Data requirements**: All variables must have same number of observations
4. **Limited bootstrap options**: Currently only `multiMBBo` is implemented (no SmBoD or StBo variants)

## Troubleshooting

* **Error: "object must be a list"**: Ensure `mfdataobj` is a list, not a single object.
* **Error: "ERROR IN THE DATA DIMENSIONS"**: Ensure all variables have the same number of rows (observations).
* **Error related to weights**: Ensure `weights` vector has length equal to the number of variables.
* **No outliers detected**: Try adjusting `quan` to a higher value or check if your data actually contains outliers.
* **Too many outliers detected**: Try adjusting `quan` to a lower value or increase `nb` for more stable cutoff estimation.
* **Error related to multiMBD**: Ensure the `multiMBD` function is available (source `R/depths.R`).

## Algorithm Details

### Multivariate Bootstrap Algorithm

1. **Bootstrap cutoff estimation**: 
   - Uses `multiMBBo` to generate bootstrap samples
   - Calculates multivariate depth for each bootstrap sample
   - Estimates cutoff from the distribution of depth quantiles

2. **Iterative outlier removal**:
   - Calculates multivariate depths for all observations
   - Identifies observations with depth < cutoff
   - Removes identified outliers from all variables simultaneously
   - Recalculates depths on remaining data
   - Repeats until no more outliers are found or more than 20% of data is removed

3. **Stopping criteria**: The algorithm stops when:
   - No outliers are found in an iteration, OR
   - More than 20% of the original data has been identified as outliers

### Functional Boxplot Algorithm

1. **Iterative process**:
   - Calculates multivariate depths for all observations
   - For each variable, uses functional boxplot to detect outliers
   - Combines outliers detected across all variables
   - Removes identified outliers from all variables
   - Repeats until no more outliers are found

## Best Practices

1. **Data preparation**: Ensure all variables have consistent structure and row names
2. **Weight selection**: Start with equal weights, then adjust based on results or domain knowledge
3. **Block size**: Choose `l` based on temporal dependence structure
4. **Validation**: Test on data with known outliers if available
5. **Compare methods**: Try both bootstrap and functional boxplot methods
6. **Check results**: Examine outlier depths and iteration information to understand detection process

## References

For the original univariate methods, see:
- Raña, P., Aneiros, G. and Vilar, J. (2015) Detection of outliers in functional time series. Environmetrics, 26, 178–191.

For multivariate functional depth, see:
- General literature on multivariate functional data analysis and depth measures
