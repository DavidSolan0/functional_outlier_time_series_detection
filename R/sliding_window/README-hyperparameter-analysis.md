# Hyperparameter Analysis for Sliding Window Outlier Detection

This directory contains scripts for comprehensive hyperparameter analysis of the sliding window outlier detection method. The method has two main hyperparameters:

1. **`window_size`**: Size of the symmetric window around each observation
2. **`threshold`**: Threshold for determining outliers based on window count

## Files Overview

### Core Analysis Scripts
- `hyperparameter-grid-simulation.R`: Main functions for running hyperparameter grid search
- `run-hyperparameter-analysis.R`: Script to run univariate hyperparameter analysis
- `multivariate-hyperparameter-analysis.R`: Script to run multivariate hyperparameter analysis
- `comprehensive-hyperparameter-analysis.R`: Script to generate comprehensive comparison report
- `quick-test.R`: Quick test script to verify everything works

### Generated Files (after running analysis)
- `hyperparameter_analysis_results.RData`: Univariate analysis results
- `multivariate_hyperparameter_analysis_results.RData`: Multivariate analysis results
- `*_hyperparameter_plots.pdf`: Performance plots for each model
- `*_comparison.pdf`: Comparison plots between univariate and multivariate
- `comprehensive_*_comparison.pdf`: Comprehensive performance comparison plots

## Quick Start

### 1. Run Quick Test
First, verify everything works by running the quick test:

```r
source("R/sliding_window/quick-test.R")
```

### 2. Run Univariate Analysis
```r
source("R/sliding_window/run-hyperparameter-analysis.R")
```

### 3. Run Multivariate Analysis
```r
source("R/sliding_window/multivariate-hyperparameter-analysis.R")
```

### 4. Generate Comprehensive Report
```r
source("R/sliding_window/comprehensive-hyperparameter-analysis.R")
```

## Hyperparameter Ranges

The analysis tests the following parameter combinations:

### Window Sizes
- 4, 6, 8, 10, 12, 16

### Thresholds
- 0.3, 0.4, 0.5, 0.6, 0.7

### Contamination Levels (K)
- 10, 15, 20, 25

### Models Tested
- **Magnitude**: Outliers with different magnitude
- **Shape**: Outliers with different shape
- **Partial**: Outliers affecting only part of the curve

## Performance Metrics

The analysis evaluates performance using:

1. **False Positive Rate (FPR)**: Proportion of clean observations incorrectly flagged as outliers
2. **True Positive Rate (TPR)**: Proportion of actual outliers correctly detected
3. **Balanced Score**: TPR - FPR (higher is better)

## Key Functions

### `run_hyperparameter_grid_simulation()`
Main function that runs the grid search simulation.

**Parameters:**
- `window_sizes`: Vector of window sizes to test
- `thresholds`: Vector of thresholds to test
- `K_values`: Vector of contamination levels to test
- `rho`: Correlation parameter (default: 0.8)
- `model`: Data generation model (magnitude, shape, partial)
- `M`: Number of simulations per combination (default: 50)
- `dfunc`: Depth function to use (MBD for univariate, multiMBD for multivariate)
- `multivariate`: Boolean flag for multivariate analysis

### `create_performance_plots()`
Creates visualization plots for the results.

### `find_optimal_hyperparameters()`
Finds the optimal hyperparameters based on different criteria:
- `"balanced"`: Maximizes TPR - FPR
- `"fpr_focused"`: Minimizes FPR
- `"tpr_focused"`: Maximizes TPR

### `create_summary_tables()`
Generates summary statistics by window size, threshold, and K.

## Expected Results

After running the complete analysis, you should see:

1. **Optimal hyperparameters** for each model and contamination level
2. **Performance plots** showing how FPR and TPR vary with hyperparameters
3. **Heatmaps** showing performance across the parameter grid
4. **Comparison plots** between univariate and multivariate approaches
5. **Summary tables** with average performance metrics

## Interpretation Guidelines

### Window Size
- **Smaller windows (4-6)**: More sensitive to local outliers, may have higher FPR
- **Larger windows (12-16)**: More robust, may miss subtle outliers
- **Medium windows (8-10)**: Often provide good balance

### Threshold
- **Lower thresholds (0.3-0.4)**: More conservative, lower FPR but may miss outliers
- **Higher thresholds (0.6-0.7)**: More aggressive, higher TPR but may increase FPR
- **Medium thresholds (0.5)**: Often provide good balance

### Model-Specific Insights
- **Magnitude outliers**: Usually easier to detect, work well with medium parameters
- **Shape outliers**: May require smaller windows to capture local shape differences
- **Partial outliers**: May benefit from larger windows to establish better context

## Troubleshooting

### Common Issues
1. **Memory issues**: Reduce `M` parameter for fewer simulations
2. **Long runtime**: Use smaller parameter grids or reduce `M`
3. **Missing dependencies**: Ensure all required packages are installed
4. **Data generation errors**: Check that model functions are properly loaded

### Quick Test
If you encounter issues, run the quick test first:
```r
source("R/sliding_window/quick-test.R")
```

This will verify that all components are working correctly before running the full analysis.

## Customization

To customize the analysis:

1. **Modify parameter ranges** in the main analysis scripts
2. **Change performance metrics** by modifying the scoring functions
3. **Add new models** by ensuring they follow the same interface as existing models
4. **Adjust visualization** by modifying the plotting functions

## Output Files

The analysis generates several types of output:

1. **RData files**: Complete results for further analysis
2. **PDF plots**: Performance visualizations
3. **Console output**: Optimal parameters and summary statistics
4. **Comparison plots**: Side-by-side univariate vs multivariate performance

All output files are saved in the `R/sliding_window/` directory.
