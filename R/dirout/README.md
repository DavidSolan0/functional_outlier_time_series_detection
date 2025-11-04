# Detection of outliers in functional time series using DirOut

This directory contains the implementation of outlier detection methods for functional time series data using **Directional Outlyingness (DirOut)**. DirOut is a robust outlier detection method that characterizes outlyingness using two measures: average directional outlyingness and variance of directional outlyingness.

## Overview

DirOut (Directional Outlyingness) is a method that extends traditional depth-based outlier detection by considering both the magnitude and the variability of outlyingness across different directions. Unlike methods that use a single depth measure, DirOut provides two complementary measures:
- **Average directional outlyingness** (`d_avr`): Measures the average magnitude of outlyingness
- **Variance of directional outlyingness** (`d_var`): Measures the variability/consistency of outlyingness across directions

This directory implements:
- Bootstrap procedures for cutoff estimation using DirOut
- Univariate and multivariate functional outlier detection
- Integration with moving block bootstrap (MBBo) and stationary bootstrap (StBo) methods

## Prerequisites

Before using these functions, ensure you have the required packages loaded:

```r
library(fda)
library(roahd)
library(fda.usc)
library(mrfDepth)
library(fdaoutlier)  # Required for dir_out() function
```

Also, make sure to source the necessary files:

```r
source("R/utils.R")
source("R/dirout/bootstrap-procedures.R")
source("R/dirout/outlier-detection-procedures.R")
```

## Main Functions

### 1. `outlier_dirout()` - Univariate Functional Data

This function implements outlier detection for univariate functional data using DirOut.

#### Function Signature

```r
outlier_dirout(
    fdataobj,
    nb = 200,
    quan = 0.5,
    l = 4,
    ns = 0.99,
    dfunc = "random_projections",
    smo = 0,
    boot = MBBo.DirOut,
    plot = FALSE
)
```

#### Parameters

* **`fdataobj`**: Functional data object (from `fda.usc` package) or matrix/data frame that can be converted to `fdata`. Each row represents a curve, and each column represents a time point.
* **`nb`**: Number of bootstrap samples to generate (default: 200). More samples provide more stable cutoff estimates but increase computation time.
* **`quan`**: Quantile used for cutoff estimation from bootstrap distribution (default: 0.5, i.e., median). Values between 0 and 1.
* **`l`**: Block size for bootstrap sampling (default: 4). **Only used for `MBBo.DirOut` bootstrap method**. Should be chosen based on the temporal dependence structure.
* **`ns`**: Quantile to calculate in each bootstrap iteration (default: 0.99). This is the quantile of directional outlyingness values used within each bootstrap sample. **Note**: Unlike the original proposal, DirOut typically uses a high quantile (0.99) since outliers have high outlyingness values.
* **`dfunc`**: Depth function to use for directional outlyingness calculation (default: `"random_projections"`). Options include:
    - `"random_projections"` or `"RP"`: Random projections depth
    - `"MhD"`: Mahalanobis depth
    - `"SD"`: Spatial depth
    - `"HS"`: Halfspace depth
* **`smo`**: Smoothness parameter (default: 0). Currently not actively used in DirOut methods but kept for compatibility.
* **`boot`**: Bootstrap procedure to use (default: `MBBo.DirOut`). Options:
    - `MBBo.DirOut`: Moving block bootstrap for DirOut
    - `StBo_DirOut`: Stationary bootstrap for DirOut
* **`plot`**: Logical indicating whether to plot the directional outlyingness measures (default: `FALSE`). If `TRUE`, creates a scatter plot of average vs. variance of directional outlyingness.

#### Return Value

The function returns a list with the following components:

* **`outliers`**: Vector of detected outlier indices (as row names from the original data)
* **`dep.out_avr`**: Vector of average directional outlyingness values for the detected outliers
* **`dep.out_var`**: Vector of variance of directional outlyingness values for the detected outliers
* **`q_avr`**: Estimated cutoff value for average directional outlyingness
* **`q_var`**: Estimated cutoff value for variance of directional outlyingness
* **`iteration`**: Vector indicating the iteration when each outlier was found

#### Example Usage

```r
# Prepare your functional data
data_matrix <- matrix(rnorm(100*50), nrow = 100, ncol = 50)
fdataobj <- fdata(data_matrix)

# Run outlier detection with DirOut using moving block bootstrap
result <- outlier_dirout(
    fdataobj = fdataobj,
    nb = 200,
    quan = 0.5,
    l = 4,
    ns = 0.99,
    dfunc = "random_projections",
    boot = MBBo.DirOut,
    plot = TRUE  # Visualize directional outlyingness
)

# Access results
print(paste("Number of outliers detected:", length(result$outliers)))
print(paste("Outlier indices:", result$outliers))
print(paste("Cutoff for average outlyingness:", result$q_avr))
print(paste("Cutoff for variance outlyingness:", result$q_var))
```

### 2. `multivariate_outlier_dirout()` - Multivariate Functional Data

This function implements outlier detection for multivariate functional data using DirOut.

#### Function Signature

```r
multivariate_outlier_dirout(
    fdataobj,
    nb = 200,
    quan = 0.5,
    l = 4,
    ns = 0.99,
    dfunc = "random_projections",
    smo = 0,
    boot = multiMBBo.DirOut,
    plot = FALSE
)
```

#### Parameters

* **`fdataobj`**: Three-dimensional array containing multivariate functional data. Dimensions should be `[n, m, p]` where:
    - `n`: Number of observations (curves)
    - `m`: Number of time points
    - `p`: Number of variables/components
* **`nb`**: Number of bootstrap samples to generate (default: 200)
* **`quan`**: Quantile used for cutoff estimation (default: 0.5)
* **`l`**: Block size for bootstrap sampling (default: 4). **Only used for `multiMBBo.DirOut`**
* **`ns`**: Quantile to calculate in each bootstrap iteration (default: 0.99)
* **`dfunc`**: Depth function to use (default: `"random_projections"`)
* **`smo`**: Smoothness parameter (default: 0)
* **`boot`**: Bootstrap procedure to use (default: `multiMBBo.DirOut`)
* **`plot`**: Logical indicating whether to plot directional outlyingness for each variable (default: `FALSE`)

#### Return Value

The function returns a list with:
* **`outliers`**: Vector of detected outlier indices
* **`dep.out_avr`**: Vector of average directional outlyingness values for outliers
* **`dep.out_var`**: Vector of variance of directional outlyingness values for outliers
* **`q_avr`**: List of cutoff values for average directional outlyingness (one per variable)
* **`q_var`**: Cutoff value for variance of directional outlyingness

#### Example Usage

```r
# Prepare multivariate functional data as a 3D array
# Example: 100 observations, 50 time points, 2 variables
n_obs <- 100
n_time <- 50
n_vars <- 2
fdataobj <- array(rnorm(n_obs * n_time * n_vars), 
                  dim = c(n_obs, n_time, n_vars))

# Run multivariate outlier detection
result <- multivariate_outlier_dirout(
    fdataobj = fdataobj,
    nb = 200,
    quan = 0.5,
    l = 4,
    ns = 0.99,
    dfunc = "random_projections",
    boot = multiMBBo.DirOut,
    plot = TRUE
)

# Access results
print(paste("Number of outliers detected:", length(result$outliers)))
print(paste("Outlier indices:", result$outliers))
```

## Bootstrap Methods

### 1. MBBo.DirOut (Moving Block Bootstrap for DirOut)

**When to use**: Recommended for functional time series with temporal dependence.

**How it works**:
1. Divides the time series into overlapping blocks of size `l`
2. Samples blocks with replacement to create bootstrap samples
3. Calculates directional outlyingness measures for each bootstrap sample
4. Estimates cutoffs from the distribution of quantiles

**Example usage**:

```r
# Prepare functional time series data
data_matrix <- matrix(rnorm(100*50), nrow = 100, ncol = 50)
fdataobj <- fdata(data_matrix)

# Run outlier detection with MBBo.DirOut
result <- outlier_dirout(
    fdataobj = fdataobj,
    nb = 200,
    quan = 0.5,
    l = 4,        # Block size - adjust based on temporal dependence
    ns = 0.99,
    dfunc = "random_projections",
    boot = MBBo.DirOut
)
```

**Key parameters for MBBo.DirOut**:
* `l`: Block size. Should reflect the temporal dependence structure:
    - Small values (2-4): For weak dependence
    - Medium values (5-10): For moderate dependence
    - Large values (10+): For strong dependence
* `nb`: Number of bootstrap samples. Increase for more stable estimates.
* `ns`: Typically set to 0.99 for DirOut (high quantile since outliers have high outlyingness)

### 2. StBo_DirOut (Stationary Bootstrap for DirOut)

**When to use**: Suitable for stationary functional time series. Uses random block lengths following a geometric distribution.

**How it works**:
1. Generates blocks with random lengths following a geometric distribution
2. Samples starting points uniformly and wraps around (circular) to handle boundaries
3. Calculates directional outlyingness measures for each bootstrap sample
4. Estimates cutoffs from the distribution of quantiles

**Example usage**:

```r
# Prepare functional time series data
data_matrix <- matrix(rnorm(100*50), nrow = 100, ncol = 50)
fdataobj <- fdata(data_matrix)

# Run outlier detection with StBo_DirOut
result <- outlier_dirout(
    fdataobj = fdataobj,
    nb = 200,
    quan = 0.5,
    ns = 0.99,
    dfunc = "random_projections",
    boot = StBo_DirOut
)
```

**Key parameters for StBo_DirOut**:
* The function uses a geometric distribution parameter internally, but it's not directly exposed in the current implementation
* `nb`: Number of bootstrap samples
* `ns`: Typically set to 0.99

### 3. multiMBBo.DirOut (Multivariate Moving Block Bootstrap)

**When to use**: For multivariate functional time series with temporal dependence.

**How it works**:
1. Similar to MBBo.DirOut but handles multivariate data
2. Calculates separate cutoffs for average directional outlyingness for each variable
3. Uses a single cutoff for variance of directional outlyingness (shared across variables)

**Example usage**:

```r
# Prepare multivariate functional data
fdataobj <- array(rnorm(100 * 50 * 2), dim = c(100, 50, 2))

# Run multivariate outlier detection
result <- multivariate_outlier_dirout(
    fdataobj = fdataobj,
    nb = 200,
    quan = 0.5,
    l = 4,
    ns = 0.99,
    dfunc = "random_projections",
    boot = multiMBBo.DirOut
)
```

## Complete Examples

### Example 1: Univariate Functional Data with MBBo.DirOut

```r
# Load required libraries
library(fda)
library(roahd)
library(fda.usc)
library(mrfDepth)
library(fdaoutlier)

# Source required files
source("R/utils.R")
source("R/dirout/bootstrap-procedures.R")
source("R/dirout/outlier-detection-procedures.R")

# Create or load your functional data
set.seed(123)
n_curves <- 100
n_timepoints <- 50
data_matrix <- matrix(rnorm(n_curves * n_timepoints), 
                     nrow = n_curves, 
                     ncol = n_timepoints)
fdataobj <- fdata(data_matrix)

# Method 1: MBBo.DirOut
result_mbbo <- outlier_dirout(
    fdataobj = fdataobj,
    nb = 200,
    quan = 0.5,
    l = 4,
    ns = 0.99,
    dfunc = "random_projections",
    boot = MBBo.DirOut,
    plot = TRUE
)

# Method 2: StBo_DirOut
result_stbo <- outlier_dirout(
    fdataobj = fdataobj,
    nb = 200,
    quan = 0.5,
    ns = 0.99,
    dfunc = "random_projections",
    boot = StBo_DirOut,
    plot = TRUE
)

# Compare results
cat("MBBo.DirOut detected", length(result_mbbo$outliers), "outliers\n")
cat("StBo_DirOut detected", length(result_stbo$outliers), "outliers\n")
```

### Example 2: Multivariate Functional Data

```r
# Load required libraries
library(fda)
library(roahd)
library(fda.usc)
library(mrfDepth)
library(fdaoutlier)

# Source required files
source("R/utils.R")
source("R/dirout/bootstrap-procedures.R")
source("R/dirout/outlier-detection-procedures.R")

# Create multivariate functional data
set.seed(123)
n_obs <- 100
n_time <- 50
n_vars <- 2
fdataobj <- array(rnorm(n_obs * n_time * n_vars), 
                  dim = c(n_obs, n_time, n_vars))

# Run multivariate outlier detection
result <- multivariate_outlier_dirout(
    fdataobj = fdataobj,
    nb = 200,
    quan = 0.5,
    l = 4,
    ns = 0.99,
    dfunc = "random_projections",
    boot = multiMBBo.DirOut,
    plot = TRUE
)

# Access results
print(paste("Number of outliers detected:", length(result$outliers)))
print(paste("Outlier indices:", result$outliers))
```

## Understanding DirOut Results

DirOut provides two complementary measures of outlyingness:

1. **Average Directional Outlyingness (`d_avr`)**: 
   - Measures how far a curve is from the center on average across all directions
   - High values indicate magnitude outliers (curves that are consistently far from the center)
   
2. **Variance of Directional Outlyingness (`d_var`)**:
   - Measures the variability of outlyingness across different directions
   - High values indicate shape outliers (curves that are outlying in some directions but not others)

The visualization plot (when `plot = TRUE`) shows the relationship between these two measures:
- **Magnitude outliers**: High `d_avr`, moderate `d_var`
- **Shape outliers**: Moderate `d_avr`, high `d_var`
- **Severe outliers**: High `d_avr`, high `d_var`

## Tips for Choosing the Right Method

1. **For univariate functional data**: Use `outlier_dirout()` with either `MBBo.DirOut` or `StBo_DirOut`
2. **For multivariate functional data**: Use `multivariate_outlier_dirout()` with `multiMBBo.DirOut`
3. **For time series with temporal dependence**: Prefer `MBBo.DirOut` or `multiMBBo.DirOut`
4. **For independent or weakly dependent data**: Either method works, but `MBBo.DirOut` with small `l` is often sufficient

## Parameter Tuning Guidelines

* **`nb`**: Start with 200. Increase to 500-1000 for more stable results, especially with small sample sizes.
* **`quan`**: 
    - 0.5 (median): Balanced approach
    - Lower values (0.25): More conservative (fewer outliers)
    - Higher values (0.75): More liberal (more outliers)
* **`ns`**: 
    - **Important**: For DirOut, this should typically be set to **0.99** (not 0.01 as in the original proposal)
    - This is because outliers have HIGH outlyingness values, so we want the 99th percentile
    - Lower values (0.95) would be more conservative
* **`l`**: Block size for MBBo methods. Choose based on temporal dependence:
    - Weak dependence: `l = 2-4`
    - Moderate dependence: `l = 5-10`
    - Strong dependence: `l = 10+`
* **`dfunc`**: Depth function for directional outlyingness:
    - `"random_projections"`: Default, works well for most cases
    - `"MhD"`: Mahalanobis depth, good for elliptical distributions
    - `"SD"`: Spatial depth, robust to outliers
    - `"HS"`: Halfspace depth, computationally intensive

## Differences from Original Proposal

The DirOut implementation differs from the original proposal in several key ways:

1. **Two-component outlyingness**: DirOut uses both average and variance of directional outlyingness, while the original proposal uses a single depth measure.

2. **Cutoff values**: DirOut estimates separate cutoffs for average (`q_avr`) and variance (`q_var`) of directional outlyingness.

3. **Quantile parameter (`ns`)**: 
   - Original proposal: Typically uses `ns = 0.01` (low quantile, since outliers have low depth)
   - DirOut: Typically uses `ns = 0.99` (high quantile, since outliers have high outlyingness)

4. **Outlier detection logic**: 
   - Original proposal: Outliers have depth < cutoff
   - DirOut: Outliers have outlyingness > cutoff (for both `d_avr` and `d_var`)

5. **Multivariate support**: DirOut includes dedicated multivariate methods that handle multiple functional variables simultaneously.

## Troubleshooting

* **Error: "ERROR IN THE DATA DIMENSIONS"**: Ensure your data is a matrix, `fdata` object, or 3D array with proper dimensions.
* **Error: "fdataobj contain X curves with some NA value"**: Remove or impute missing values before running the function.
* **No outliers detected**: Try adjusting `quan` to a higher value or check if your data actually contains outliers. Also verify that `ns` is set appropriately (0.99 for DirOut).
* **Too many outliers detected**: Try adjusting `quan` to a lower value or increase `nb` for more stable cutoff estimation.
* **Error related to `dir_out()`**: Ensure the `fdaoutlier` package is installed and loaded.

## Algorithm Details

### Univariate DirOut Algorithm

1. **Bootstrap cutoff estimation**: 
   - Uses the specified bootstrap method to generate bootstrap samples
   - Calculates directional outlyingness for each bootstrap sample
   - Estimates two cutoffs: one for average outlyingness (`q_avr`) and one for variance (`q_var`)

2. **Iterative outlier removal**:
   - Calculates directional outlyingness for all curves
   - Identifies curves with `d_avr > q_avr` OR `d_var > q_var`
   - Removes identified outliers
   - Recalculates outlyingness on remaining data
   - Repeats until no more outliers are found

3. **Stopping criteria**: The algorithm stops when:
   - No outliers are found in an iteration

### Multivariate DirOut Algorithm

1. **Bootstrap cutoff estimation**:
   - Similar to univariate, but calculates separate cutoffs for average outlyingness for each variable
   - Uses a single cutoff for variance outlyingness (shared across variables)

2. **Outlier detection**:
   - For each variable, identifies curves with `d_avr[, v] > q_avr[[v]]`
   - Identifies curves with `d_var > q_var`
   - Combines outliers from all criteria

## Visualization

Setting `plot = TRUE` creates informative visualizations:

- **Univariate**: Scatter plot of average vs. variance of directional outlyingness
- **Multivariate**: Separate plots for each variable showing average vs. variance

These plots help identify:
- **Magnitude outliers**: Points in the upper-left region (high average, low variance)
- **Shape outliers**: Points in the lower-right region (low average, high variance)
- **Severe outliers**: Points in the upper-right region (high average, high variance)

## References

For more information on DirOut (Directional Outlyingness), see:
- Dai, W., & Genton, M. G. (2019). Directional outlyingness for multivariate functional data. Computational Statistics & Data Analysis, 131, 50-65.

For the bootstrap procedures used here, see:
- Raña, P., Aneiros, G. and Vilar, J. (2015) Detection of outliers in functional time series. Environmetrics, 26, 178–191.

