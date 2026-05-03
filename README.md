# Functional Outlier Time Series Detection

A comprehensive R implementation for detecting outliers in functional time series data using various depth-based and bootstrap methods.

## Overview

This repository provides multiple approaches for outlier detection in functional time series, based on and extending methods from the literature, and is the official research code repository for:

**A Hybrid Nonparametric Framework for Outlier Detection in Functional Time Series**

## Available Methods

### 1. [Original Proposal](R/original_proposal/README.md)
Bootstrap-based outlier detection methods following Raña et al. (2015):
- **SmBoD**: Standard smoothed bootstrap on data
- **MBBo**: Moving block bootstrap (for temporal dependence)
- **StBo**: Stationary bootstrap

### 2. [Multivariate Process](R/multivariate_process/README.md)
Multivariate extension using derivative information:
- Uses original process, first derivative, and second derivative
- Combines information across variables with weighted depth

> ⚠️ **Note**: May mask magnitude outliers. Use when you expect shape but not magnitude outliers.

### 3. [DirOut Methods](R/dirout/README.md)
Directional outlyingness-based detection:
- Two-component outlyingness (average and variance)
- Captures both magnitude and shape outliers

### 4. [Sliding Window](R/sliding_window/README.md)
Local outlier detection using sliding windows:
- Window-based functional depth and boxplot detection
- Aggregates outlier flags across multiple windows

## Quick Start

### Installation

```r
# Required packages
library(fda)
library(roahd)
library(fda.usc)
library(mrfDepth)
library(fdaoutlier)  # For DirOut methods
```

### Basic Example

```r
# Source required files
source("R/utils.R")
source("R/depths.R")

# Prepare your functional data
data_matrix <- matrix(rnorm(100*50), nrow = 100, ncol = 50)
fdataobj <- fdata(data_matrix)

# Method 1: Original proposal (MBBo)
source("R/original_proposal/bootstrap-procedures.R")
source("R/original_proposal/outlier-detection-procedures.R")

result <- outlier_bootstrap(
    fdataobj = fdataobj,
    boot = MBBo,
    dfunc = MBD
)

print(result$outliers)
```

## Documentation

Each method has detailed documentation with:
- Complete function signatures and parameter descriptions
- Working code examples
- Parameter tuning guidelines
- Troubleshooting tips

See the README files in each directory:
- [`R/original_proposal/README.md`](R/original_proposal/README.md)
- [`R/multivariate_process/README.md`](R/multivariate_process/README.md)
- [`R/dirout/README.md`](R/dirout/README.md)
- [`R/sliding_window/README.md`](R/sliding_window/README.md)

## Choosing a Method

| Method | Best For | Strengths |
|--------|----------|-----------|
| **Original Proposal** | General use, magnitude outliers | Robust, well-tested, multiple bootstrap options |
| **Multivariate Process** | Shape outliers, partial contamination | Excellent for shape detection, uses derivative info |
| **DirOut** | Comprehensive detection | Two-component outlyingness, captures both types |
| **Sliding Window** | Non-stationary data, local patterns | Local context, no bootstrap required |

## Repository Structure

```
R/
├── original_proposal/     # Original bootstrap methods
├── multivariate_process/  # Multivariate extension with derivatives
├── dirout/                # Directional outlyingness methods
├── sliding_window/        # Sliding window approach
├── real_data/            # Real data examples
├── depths.R              # Depth function implementations
├── utils.R               # Utility functions
└── simulated-models.R    # Simulation models for testing
```

## Reference

If you use this code, please cite:

**A Hybrid Nonparametric Framework for Outlier Detection in Functional Time Series**  
CC BY-NC-ND 4.0

[https://onlinelibrary.wiley.com/doi/10.1002/env.70099](https://onlinelibrary.wiley.com/doi/10.1002/env.70099)
