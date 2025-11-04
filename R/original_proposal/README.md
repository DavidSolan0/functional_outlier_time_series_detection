# Detection of outliers in functional time series

This proposal was elaborated by Raña et al. [2015](https://onlinelibrary.wiley.com/doi/abs/10.1002/env.2327). Here I dispose the codes to replicate their work as follows:

* `utils.R`: code with utils functions 
* `simulated-models.R`: here there are the simulated models described in section 4.1
* `bootstrap-procedures.R`: here you will find three functions. **SmBoD** for standard smoothed bootstrap on the data. **MBBo** for moving block bootstrap (Section 3.1). **StBo** for Standard smoothed bootstrap on residuals (Section 3.1).
* `outlier-detection-procedures.R`: here you will find the iterative process described in section 2.3 and 3.1 for the cutoff estimation.
* `depths.R`: functional depth to implement
* `power-study-functions.R`: here you will find customised functions to replicate tables on section 4.3.
* `results.R`: here you will find the calculus error for the different scenarios. 

## Overview

This directory contains the implementation of outlier detection methods for functional time series data using bootstrap procedures. The main function `outlier_bootstrap()` implements an iterative algorithm that detects outliers by comparing depth values against a bootstrap-estimated cutoff threshold.

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
source("R/depths.R")
source("R/original_proposal/bootstrap-procedures.R")
source("R/original_proposal/outlier-detection-procedures.R")
```

## Main Function: `outlier_bootstrap()`

The `outlier_bootstrap()` function is the main entry point for outlier detection. It uses one of three bootstrap methods to estimate a cutoff value and then iteratively identifies outliers.

### Function Signature

```r
outlier_bootstrap(
    fdataobj,
    nb = 200,
    smo = 0.05,
    quan = 0.5,
    dfunc = MBD,
    l = 4,
    p = 0.1,
    ns = 0.01,
    boot = SmBoD
)
```

### Parameters

* **`fdataobj`**: Functional data object (from `fda.usc` package) or matrix/data frame that can be converted to `fdata`. Each row represents a curve, and each column represents a time point.
* **`nb`**: Number of bootstrap samples to generate (default: 200). More samples provide more stable cutoff estimates but increase computation time.
* **`smo`**: Smoothness parameter for adding noise (default: 0.05). **Only used for `SmBoD` bootstrap method**. Controls the variance of the noise added to bootstrap samples.
* **`quan`**: Quantile used for cutoff estimation from bootstrap distribution (default: 0.5, i.e., median). Values between 0 and 1. Lower values make the method more conservative (fewer outliers detected).
* **`dfunc`**: Depth function to use for outlier detection (default: `MBD` from `roahd` package). Common options include:
    - `MBD`: Modified Band Depth (from `roahd` package)
    - `MD`: Mode Depth (from `depths.R`)
    - Any custom depth function that takes a matrix and returns a vector of depth values
* **`l`**: Block size for bootstrap sampling (default: 4). **Only used for `MBBo` bootstrap method**. Should be chosen based on the temporal dependence structure of your data.
* **`p`**: Success probability for geometric distribution (default: 0.1). **Only used for `StBo` bootstrap method**. Controls the expected block length in stationary bootstrap. Lower values yield longer blocks.
* **`ns`**: Quantile to calculate in each bootstrap iteration (default: 0.01). This is the quantile of depth values used within each bootstrap sample to estimate the cutoff.
* **`boot`**: Bootstrap procedure to use (default: `SmBoD`). Options:
    - `SmBoD`: Standard smoothed bootstrap on the data
    - `MBBo`: Moving block bootstrap
    - `StBo`: Standard smoothed bootstrap on residuals (stationary bootstrap)

### Return Value

The function returns a list with the following components:

* **`outliers`**: Vector of detected outlier indices (as row names from the original data)
* **`dep.out`**: Vector of depth values for the detected outliers
* **`iteration`**: Vector indicating the iteration when each outlier was found (useful for understanding the iterative process)
* **`quantile`**: Estimated cutoff value used for outlier detection
* **`depths`**: Vector of depth values for all remaining (non-outlier) curves after the iterative process

## Bootstrap Methods

### 1. SmBoD (Standard Smoothed Bootstrap on Data)

**When to use**: Suitable for independent functional data or when temporal dependence is weak.

**How it works**:
1. Removes outliers using a naive approach (functional boxplot)
2. Generates bootstrap samples by resampling curves with replacement
3. Adds Gaussian noise to each bootstrap sample based on the variance of the data
4. Calculates depth quantiles from bootstrap samples to estimate cutoff

**Example usage**:

```r
# Prepare your functional data
data_matrix <- matrix(rnorm(100*50), nrow = 100, ncol = 50)
fdataobj <- fdata(data_matrix)

# Run outlier detection with SmBoD
result <- outlier_bootstrap(
    fdataobj = fdataobj,
    nb = 200,
    smo = 0.05,
    quan = 0.5,
    dfunc = MBD,
    ns = 0.01,
    boot = SmBoD
)

# Access results
print(paste("Number of outliers detected:", length(result$outliers)))
print(paste("Outlier indices:", result$outliers))
print(paste("Cutoff value:", result$quantile))
```

**Key parameters for SmBoD**:
* `smo`: Controls the amount of noise added. Typical values: 0.01-0.1. Larger values add more variability.
* `nb`: Number of bootstrap samples. Increase for more stable estimates.

### 2. MBBo (Moving Block Bootstrap)

**When to use**: Recommended for functional time series with temporal dependence. Preserves the temporal structure by sampling blocks of consecutive curves.

**How it works**:
1. Removes outliers using a naive approach (functional boxplot)
2. Divides the time series into overlapping blocks of size `l`
3. Samples blocks with replacement to create bootstrap samples
4. Calculates depth quantiles from bootstrap samples to estimate cutoff

**Example usage**:

```r
# Prepare your functional time series data
# Note: Rows should be ordered by time
data_matrix <- matrix(rnorm(100*50), nrow = 100, ncol = 50)
fdataobj <- fdata(data_matrix)

# Run outlier detection with MBBo
result <- outlier_bootstrap(
    fdataobj = fdataobj,
    nb = 200,
    quan = 0.5,
    dfunc = MBD,
    l = 4,        # Block size - adjust based on temporal dependence
    ns = 0.01,
    boot = MBBo
)

# Access results
print(paste("Number of outliers detected:", length(result$outliers)))
print(paste("Outlier indices:", result$outliers))
```

**Key parameters for MBBo**:
* `l`: Block size. Should reflect the temporal dependence structure:
    - Small values (2-4): For weak dependence
    - Medium values (5-10): For moderate dependence
    - Large values (10+): For strong dependence
    - Rule of thumb: Choose `l` such that observations more than `l` time units apart are approximately independent
* `nb`: Number of bootstrap samples. Increase for more stable estimates.

### 3. StBo (Stationary Bootstrap / Standard Smoothed Bootstrap on Residuals)

**When to use**: Suitable for stationary functional time series. Uses random block lengths following a geometric distribution.

**How it works**:
1. Removes outliers using a naive approach (functional boxplot)
2. Generates blocks with random lengths following a geometric distribution
3. Samples starting points uniformly and wraps around (circular) to handle boundaries
4. Calculates depth quantiles from bootstrap samples to estimate cutoff

**Example usage**:

```r
# Prepare your functional time series data
data_matrix <- matrix(rnorm(100*50), nrow = 100, ncol = 50)
fdataobj <- fdata(data_matrix)

# Run outlier detection with StBo
result <- outlier_bootstrap(
    fdataobj = fdataobj,
    nb = 200,
    quan = 0.5,
    dfunc = MBD,
    p = 0.1,      # Geometric distribution parameter
    ns = 0.01,
    boot = StBo
)

# Access results
print(paste("Number of outliers detected:", length(result$outliers)))
print(paste("Outlier indices:", result$outliers))
```

**Key parameters for StBo**:
* `p`: Success probability for geometric distribution. Controls expected block length:
    - Lower values (0.05-0.1): Longer blocks, more dependence preserved
    - Higher values (0.2-0.5): Shorter blocks, less dependence
    - Expected block length = 1/p
* `nb`: Number of bootstrap samples. Increase for more stable estimates.

## Complete Example

Here's a complete example showing how to use all three methods:

```r
# Load required libraries
library(fda)
library(roahd)
library(fda.usc)
library(mrfDepth)

# Source required files
source("R/utils.R")
source("R/depths.R")
source("R/original_proposal/bootstrap-procedures.R")
source("R/original_proposal/outlier-detection-procedures.R")

# Create or load your functional data
# Example: Create synthetic functional data
set.seed(123)
n_curves <- 100
n_timepoints <- 50
data_matrix <- matrix(rnorm(n_curves * n_timepoints), 
                     nrow = n_curves, 
                     ncol = n_timepoints)
fdataobj <- fdata(data_matrix)

# Method 1: SmBoD
result_smbod <- outlier_bootstrap(
    fdataobj = fdataobj,
    nb = 200,
    smo = 0.05,
    quan = 0.5,
    dfunc = MBD,
    ns = 0.01,
    boot = SmBoD
)

# Method 2: MBBo
result_mbbo <- outlier_bootstrap(
    fdataobj = fdataobj,
    nb = 200,
    quan = 0.5,
    dfunc = MBD,
    l = 4,
    ns = 0.01,
    boot = MBBo
)

# Method 3: StBo
result_stbo <- outlier_bootstrap(
    fdataobj = fdataobj,
    nb = 200,
    quan = 0.5,
    dfunc = MBD,
    p = 0.1,
    ns = 0.01,
    boot = StBo
)

# Compare results
cat("SmBoD detected", length(result_smbod$outliers), "outliers\n")
cat("MBBo detected", length(result_mbbo$outliers), "outliers\n")
cat("StBo detected", length(result_stbo$outliers), "outliers\n")
```

## Tips for Choosing the Right Method

1. **For independent data**: Use `SmBoD`
2. **For time series with clear temporal dependence**: Use `MBBo` or `StBo`
3. **If you need to preserve exact temporal structure**: Use `MBBo` with appropriate block size
4. **If you want adaptive block lengths**: Use `StBo`

## Parameter Tuning Guidelines

* **`nb`**: Start with 200. Increase to 500-1000 for more stable results, especially with small sample sizes.
* **`quan`**: 
    - 0.5 (median): Balanced approach
    - Lower values (0.25): More conservative (fewer outliers)
    - Higher values (0.75): More liberal (more outliers)
* **`ns`**: Typically kept at 0.01. This is the quantile used within each bootstrap iteration.
* **`dfunc`**: `MBD` is the default and works well for most cases. Try `MD` if you need a different depth measure.

## Troubleshooting

* **Error: "ERROR IN THE DATA DIMENSIONS"**: Ensure your data is a matrix or `fdata` object with proper dimensions.
* **Error: "fdataobj contain X curves with some NA value"**: Remove or impute missing values before running the function.
* **No outliers detected**: Try adjusting `quan` to a higher value or check if your data actually contains outliers.
* **Too many outliers detected**: Try adjusting `quan` to a lower value or increase `nb` for more stable cutoff estimation.

## Algorithm Details

The `outlier_bootstrap()` function implements an iterative algorithm:

1. **Bootstrap cutoff estimation**: Uses the specified bootstrap method to generate bootstrap samples and estimate a cutoff value from the distribution of depth quantiles.

2. **Iterative outlier removal**:
   - Calculates depths for all curves
   - Identifies curves with depth < cutoff
   - Removes identified outliers
   - Recalculates depths on remaining data
   - Repeats until no more outliers are found or more than 20% of data is removed

3. **Stopping criteria**: The algorithm stops when:
   - No outliers are found in an iteration, OR
   - More than 20% of the original data has been identified as outliers

## References

Raña, P., Aneiros, G. and Vilar, J. (2015) Detection of outliers in functional time series. Environmetrics, 26, 178–191.
