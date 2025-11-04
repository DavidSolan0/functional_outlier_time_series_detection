# Detection of outliers in functional time series using Sliding Window Method

This directory contains the implementation of outlier detection methods for functional time series data using a **sliding window approach**. This method detects outliers by analyzing local neighborhoods of observations using functional depth and boxplot detection.

## Overview

The sliding window method is a local approach to outlier detection that:
1. Creates symmetric windows around each observation in the time series
2. Calculates functional depth within each window
3. Uses functional boxplot to identify outliers within each window
4. Aggregates outlier flags across multiple windows
5. Classifies observations as outliers based on a threshold of window counts

This approach is particularly effective for:
- **Time series data** where temporal structure matters
- **Local outlier detection** where global methods might miss contextual outliers
- **Non-stationary data** where the distribution changes over time

## Key Advantages

- **Local context**: Considers temporal neighbors, making it sensitive to local patterns
- **Robust to non-stationarity**: Adapts to changes in the data distribution over time
- **Multiple confirmations**: An observation must be flagged as outlier in multiple windows, reducing false positives
- **No bootstrap required**: Faster than bootstrap-based methods
- **Interpretable results**: Provides window-level outlier information

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
source("R/sliding_window/outlier-detection-procedures.R")
```

## Main Functions

### 1. `sliding_window_outlier()` - Univariate Functional Data

This function implements outlier detection for univariate functional time series using a sliding window approach.

#### Function Signature

```r
sliding_window_outlier(
    fdataobj,
    window_size = 8,
    dfunc = "RP",
    threshold = 0.5,
    plot = FALSE
)
```

#### Parameters

* **`fdataobj`**: Functional data object (from `fda.usc` package) or matrix/data frame that can be converted to `fdata`. Each row represents a curve, and each column represents a time point. **Observations should be ordered by time**.
* **`window_size`**: Size of the symmetric window around each observation (default: 8). This determines how many neighbors are considered on each side:
    - Window includes `floor(window_size/2)` observations before and after
    - For `window_size = 8`, the window includes 4 observations before, the center, and 4 observations after (total of 9 observations)
    - Larger windows provide more context but may miss local outliers
    - Smaller windows are more sensitive to local patterns but may be less stable
* **`dfunc`**: Depth function to use for calculating functional depth within each window (default: `"RP"`). Options include:
    - **Function objects**: `MBD` (Modified Band Depth from `roahd`), `MD` (Mode Depth), or custom depth functions
    - **String identifiers**: `"RP"` (if using a function that accepts string identifiers)
    - The function should accept a matrix (rows = observations, columns = time points) and return a vector of depth values
* **`threshold`**: Threshold for determining outliers based on window count (default: 0.5). 
    - An observation is considered an outlier if it appears as outlier in at least `threshold * max_windows` windows
    - `max_windows` is the maximum number of windows an observation can appear in (typically `window_size`)
    - Values between 0 and 1:
        - Lower values (0.3-0.4): More sensitive, detects more outliers
        - Default (0.5): Balanced approach
        - Higher values (0.6-0.7): More conservative, fewer outliers
* **`plot`**: Logical indicating whether to plot the outlier counts (default: `FALSE`). If `TRUE`, creates a plot showing:
    - Number of windows each observation was flagged as outlier
    - Threshold line
    - Final detected outliers highlighted

#### Return Value

The function returns a list with the following components:

* **`outliers`**: Vector of detected outlier indices (as row names from the original data)
* **`outlier_counts`**: Vector of length `n` showing how many windows each observation was flagged as outlier
* **`window_outliers`**: List of length `n` containing the outlier indices detected in each window
* **`threshold`**: The threshold value used
* **`min_outlier_windows`**: Minimum number of windows an observation must appear as outlier to be classified as final outlier

#### Example Usage

```r
# Prepare your functional time series data
# Note: Observations should be ordered by time
data_matrix <- matrix(rnorm(100*50), nrow = 100, ncol = 50)
fdataobj <- fdata(data_matrix)

# Run outlier detection with sliding window
result <- sliding_window_outlier(
    fdataobj = fdataobj,
    window_size = 8,
    dfunc = MBD,  # Using Modified Band Depth
    threshold = 0.5,
    plot = TRUE  # Visualize outlier counts
)

# Access results
print(paste("Number of outliers detected:", length(result$outliers)))
print(paste("Outlier indices:", result$outliers))
print(paste("Minimum windows threshold:", result$min_outlier_windows))

# View outlier counts for each observation
head(result$outlier_counts)
```

### 2. `multivariate_sliding_window_outlier()` - Multivariate Functional Data

This function implements outlier detection for multivariate functional time series using a sliding window approach.

#### Function Signature

```r
multivariate_sliding_window_outlier(
    fdataobj,
    window_size = 8,
    dfunc = "RP",
    threshold = 0.5,
    plot = FALSE
)
```

#### Parameters

* **`fdataobj`**: List containing multivariate functional data objects. Each element should be an `fdata` object or matrix representing one variable. All variables must have the same number of observations (rows).
* **`window_size`**: Size of the symmetric window (default: 8). Same as univariate version.
* **`dfunc`**: Depth function to use (default: `"RP"`). For multivariate data, use:
    - `multiMBD`: Multivariate Modified Band Depth (recommended)
    - Other multivariate depth functions that accept a list of functional data objects
* **`threshold`**: Threshold for determining outliers (default: 0.5). Same as univariate version.
* **`plot`**: Logical indicating whether to plot outlier counts (default: `FALSE`)

#### Return Value

The function returns a list with the same structure as the univariate version:
* **`outliers`**: Vector of detected outlier indices
* **`outlier_counts`**: Vector showing window counts for each observation
* **`window_outliers`**: List of outliers detected in each window
* **`threshold`**: The threshold value used
* **`min_outlier_windows`**: Minimum number of windows required

#### Example Usage

```r
# Prepare multivariate functional data as a list
# Example: 100 observations, 50 time points, 2 variables
n_obs <- 100
n_time <- 50
n_vars <- 2

# Create list of functional data objects
fdata_list <- list()
for (v in 1:n_vars) {
  data_matrix <- matrix(rnorm(n_obs * n_time), nrow = n_obs, ncol = n_time)
  fdata_list[[v]] <- fdata(data_matrix)
}

# Run multivariate outlier detection
result <- multivariate_sliding_window_outlier(
    fdataobj = fdata_list,
    window_size = 8,
    dfunc = multiMBD,  # Using Multivariate Modified Band Depth
    threshold = 0.5,
    plot = TRUE
)

# Access results
print(paste("Number of outliers detected:", length(result$outliers)))
print(paste("Outlier indices:", result$outliers))
```

## How the Algorithm Works

### Step-by-Step Process

1. **Window Creation**: For each observation `i` (from 1 to `n`):
   - Creates a symmetric window: `[i - half_window, i + half_window]`
   - Handles boundaries by adjusting window size at the beginning and end of the series

2. **Depth Calculation**: Within each window:
   - Extracts the functional data for that window
   - Calculates functional depth for all observations in the window using the specified depth function

3. **Outlier Detection**: Within each window:
   - Uses functional boxplot (`fda::fbplot`) to detect outliers based on depth values
   - Lower depth values indicate potential outliers
   - Records which observations in the window are flagged as outliers

4. **Aggregation**: 
   - Counts how many windows each observation appears as outlier
   - Stores this in `outlier_counts` vector

5. **Final Classification**:
   - Calculates `min_outlier_windows = ceiling(threshold * max_windows)`
   - Classifies observations as outliers if `outlier_counts >= min_outlier_windows`

### Example: Window Size 8

For `window_size = 8`:
- `half_window = floor(8/2) = 4`
- Each window contains at most 9 observations (4 before + center + 4 after)
- An observation at position `i` appears in windows centered at positions `[i-4, i+4]` (if boundaries allow)
- Maximum windows an observation can appear in: `window_size = 8`
- With `threshold = 0.5`: An observation needs to appear as outlier in at least `ceiling(0.5 * 8) = 4` windows

## Complete Examples

### Example 1: Univariate Functional Data

```r
# Load required libraries
library(fda)
library(roahd)
library(fda.usc)
library(mrfDepth)

# Source required files
source("R/utils.R")
source("R/depths.R")
source("R/sliding_window/outlier-detection-procedures.R")

# Create functional time series data
set.seed(123)
n_curves <- 100
n_timepoints <- 50
data_matrix <- matrix(rnorm(n_curves * n_timepoints), 
                     nrow = n_curves, 
                     ncol = n_timepoints)
fdataobj <- fdata(data_matrix)

# Run sliding window outlier detection
result <- sliding_window_outlier(
    fdataobj = fdataobj,
    window_size = 8,
    dfunc = MBD,
    threshold = 0.5,
    plot = TRUE
)

# Analyze results
cat("Total outliers detected:", length(result$outliers), "\n")
cat("Outlier indices:", result$outliers, "\n")
cat("Outlier counts (first 10):", result$outlier_counts[1:10], "\n")
cat("Threshold (min windows):", result$min_outlier_windows, "\n")

# View which windows detected each outlier
for (outlier_idx in result$outliers[1:min(5, length(result$outliers))]) {
  cat("Outlier", outlier_idx, "detected in windows:\n")
  windows_with_outlier <- which(sapply(result$window_outliers, 
                                       function(w) as.numeric(outlier_idx) %in% w))
  cat("  Window centers:", windows_with_outlier, "\n")
}
```

### Example 2: Multivariate Functional Data

```r
# Load required libraries
library(fda)
library(roahd)
library(fda.usc)
library(mrfDepth)

# Source required files
source("R/utils.R")
source("R/depths.R")
source("R/sliding_window/outlier-detection-procedures.R")

# Create multivariate functional time series data
set.seed(123)
n_obs <- 100
n_time <- 50
n_vars <- 2

# Create list of functional data objects
fdata_list <- list()
for (v in 1:n_vars) {
  data_matrix <- matrix(rnorm(n_obs * n_time), nrow = n_obs, ncol = n_time)
  fdata_list[[v]] <- fdata(data_matrix)
}

# Run multivariate sliding window outlier detection
result <- multivariate_sliding_window_outlier(
    fdataobj = fdata_list,
    window_size = 8,
    dfunc = multiMBD,
    threshold = 0.5,
    plot = TRUE
)

# Analyze results
cat("Total outliers detected:", length(result$outliers), "\n")
cat("Outlier indices:", result$outliers, "\n")
```

### Example 3: Comparing Different Window Sizes

```r
# Compare results with different window sizes
window_sizes <- c(4, 8, 12, 16)
results_list <- list()

for (ws in window_sizes) {
  result <- sliding_window_outlier(
      fdataobj = fdataobj,
      window_size = ws,
      dfunc = MBD,
      threshold = 0.5,
      plot = FALSE
  )
  results_list[[as.character(ws)]] <- result
  
  cat("Window size", ws, "detected", length(result$outliers), "outliers\n")
}

# Compare outlier sets
if (length(window_sizes) > 1) {
  outlier_sets <- lapply(results_list, function(r) r$outliers)
  common_outliers <- Reduce(intersect, outlier_sets)
  cat("Common outliers across all window sizes:", length(common_outliers), "\n")
}
```

### Example 4: Comparing Different Thresholds

```r
# Compare results with different thresholds
thresholds <- c(0.3, 0.5, 0.7)
results_list <- list()

for (thresh in thresholds) {
  result <- sliding_window_outlier(
      fdataobj = fdataobj,
      window_size = 8,
      dfunc = MBD,
      threshold = thresh,
      plot = FALSE
  )
  results_list[[as.character(thresh)]] <- result
  
  cat("Threshold", thresh, "detected", length(result$outliers), "outliers\n")
}
```

## Parameter Tuning Guidelines

### Window Size (`window_size`)

The window size is a critical parameter that balances sensitivity and stability:

* **Small windows (4-6)**:
  - More sensitive to local patterns
  - Better for detecting short-term anomalies
  - May produce more false positives
  - Less stable (fewer observations per window)

* **Medium windows (8-12)**:
  - Balanced approach (recommended starting point)
  - Good general-purpose choice
  - Provides enough context while maintaining sensitivity

* **Large windows (14-20)**:
  - More stable (more observations per window)
  - Better for detecting persistent outliers
  - May miss local, short-term outliers
  - Less sensitive to local patterns

**Recommendation**: Start with `window_size = 8` and adjust based on:
- Data characteristics (temporal dependence structure)
- Desired sensitivity vs. stability trade-off
- Typical length of outlier patterns

### Threshold (`threshold`)

The threshold determines how many windows must flag an observation before it's considered an outlier:

* **Low threshold (0.3-0.4)**:
  - More sensitive, detects more outliers
  - Higher true positive rate
  - May increase false positives
  - Good when outliers are rare or subtle

* **Medium threshold (0.5)**:
  - Balanced approach (default)
  - Good general-purpose choice
  - Requires observation to be outlier in majority of relevant windows

* **High threshold (0.6-0.7)**:
  - More conservative, fewer outliers
  - Lower false positive rate
  - May miss some outliers
  - Good when false positives are costly

**Recommendation**: Start with `threshold = 0.5` and adjust based on:
- Desired sensitivity vs. specificity trade-off
- Consequences of false positives vs. false negatives
- Validation results on known data

### Depth Function (`dfunc`)

The choice of depth function affects how outliers are detected within each window:

* **MBD (Modified Band Depth)**:
  - Default choice for univariate data
  - Robust and computationally efficient
  - Good for most functional data types

* **MD (Mode Depth)**:
  - Alternative for univariate data
  - Different statistical properties
  - May perform better for specific data types

* **multiMBD (Multivariate Modified Band Depth)**:
  - Required for multivariate data
  - Extends MBD to multiple variables

**Recommendation**: 
- For univariate: Use `MBD` (default)
- For multivariate: Use `multiMBD` (required)
- Experiment with different depth functions if results are not satisfactory

## Visualization

Setting `plot = TRUE` creates informative visualizations:

### Univariate Plot
- X-axis: Observation index
- Y-axis: Number of windows where observation was flagged as outlier
- Blue points: All observations
- Red points: Final detected outliers
- Red dashed line: Threshold (`min_outlier_windows`)

### Interpretation
- Observations with high counts (above threshold) are final outliers
- Observations with moderate counts may be borderline cases
- Observations with low counts are likely not outliers

## Differences from Other Methods

### vs. Original Proposal (Bootstrap-based)

| Aspect | Sliding Window | Bootstrap-based |
|--------|----------------|-----------------|
| **Approach** | Local, window-based | Global, bootstrap-based |
| **Cutoff estimation** | Functional boxplot within windows | Bootstrap quantiles |
| **Computational cost** | Lower (no bootstrap) | Higher (multiple bootstrap samples) |
| **Temporal structure** | Explicitly considers via windows | Handled via block bootstrap |
| **Sensitivity** | Local patterns | Global patterns |
| **Parameters** | `window_size`, `threshold` | `nb`, `quan`, `l`, `p` |

### vs. DirOut Method

| Aspect | Sliding Window | DirOut |
|--------|----------------|--------|
| **Outlyingness measure** | Functional depth | Directional outlyingness |
| **Detection method** | Functional boxplot | Two-component outlyingness |
| **Cutoff** | Within-window boxplot | Bootstrap-estimated cutoffs |
| **Approach** | Local windows | Global with bootstrap |

## Advantages and Limitations

### Advantages

1. **Local context**: Considers temporal neighbors explicitly
2. **No bootstrap**: Faster computation
3. **Interpretable**: Easy to understand which windows flagged outliers
4. **Flexible**: Parameters are intuitive and easy to tune
5. **Robust to non-stationarity**: Adapts to local distribution changes

### Limitations

1. **Window size selection**: Requires careful tuning
2. **Boundary effects**: Observations near boundaries have fewer windows
3. **Multiple confirmations**: May miss outliers that appear in only a few windows
4. **Parameter sensitivity**: Results depend on `window_size` and `threshold`
5. **No statistical guarantees**: Unlike bootstrap methods, no formal statistical framework

## Troubleshooting

* **Error: "ERROR IN THE DATA DIMENSIONS"**: Ensure your data is a matrix or `fdata` object with proper dimensions.
* **Error: "fdataobj contain X curves with some NA value"**: Remove or impute missing values before running the function.
* **No outliers detected**: 
  - Try reducing `threshold` (e.g., 0.3 instead of 0.5)
  - Try reducing `window_size` (more sensitive to local patterns)
  - Check if your data actually contains outliers
* **Too many outliers detected**:
  - Try increasing `threshold` (e.g., 0.7 instead of 0.5)
  - Try increasing `window_size` (more stable, requires more confirmations)
  - Verify data quality
* **Inconsistent results across window sizes**:
  - This is expected; different window sizes capture different scales of patterns
  - Consider using multiple window sizes and combining results
* **Multivariate error: "All variables must have the same number of observations"**:
  - Ensure all elements in `fdataobj` list have the same number of rows
  - Check that row names are consistent across variables

## Best Practices

1. **Start with defaults**: Use `window_size = 8` and `threshold = 0.5` as starting points
2. **Visualize results**: Always use `plot = TRUE` to understand the outlier detection pattern
3. **Validate on known data**: Test on data with known outliers to tune parameters
4. **Compare window sizes**: Try multiple window sizes to understand scale of outliers
5. **Check outlier counts**: Examine `outlier_counts` to identify borderline cases
6. **Consider temporal structure**: Choose window size based on typical temporal dependence
7. **Handle boundaries carefully**: Be aware that observations near boundaries have fewer windows

## Advanced Usage

### Custom Depth Functions

You can use custom depth functions as long as they:
- Accept a matrix (rows = observations, columns = time points)
- Return a vector of depth values (one per observation)
- Lower depth values indicate potential outliers

Example:
```r
# Custom depth function
custom_depth <- function(data_matrix) {
  # Your custom depth calculation
  # Return vector of depth values
}

result <- sliding_window_outlier(
    fdataobj = fdataobj,
    window_size = 8,
    dfunc = custom_depth,
    threshold = 0.5
)
```

### Combining Multiple Window Sizes

You can combine results from multiple window sizes:
```r
window_sizes <- c(4, 8, 12)
all_outliers <- c()

for (ws in window_sizes) {
  result <- sliding_window_outlier(
      fdataobj = fdataobj,
      window_size = ws,
      dfunc = MBD,
      threshold = 0.5
  )
  all_outliers <- c(all_outliers, result$outliers)
}

# Get unique outliers across all window sizes
final_outliers <- unique(all_outliers)
```

## References

For more information on functional depth and boxplot methods, see:
- Sun, Y., & Genton, M. G. (2011). Functional boxplots. Journal of Computational and Graphical Statistics, 20(2), 316-334.

For the sliding window approach in time series, see general time series outlier detection literature.

For hyperparameter analysis of this method, see:
- `README-hyperparameter-analysis.md` in this directory

