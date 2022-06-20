# Detection of outliers in functional time series

This proposal was elaborated by Raña et al. [2015](https://onlinelibrary.wiley.com/doi/abs/10.1002/env.2327). Here I dispose the codes to replicate their work as follows:

* utils.R: code with utils functions 
* simulated-models.R: here there are the simulated models described in section 4.1
* bootstrap-procedures: here you will find three functions. **SmBoD** for standard smoothed bootstrap on the data. **MBBo** for moving block bootstrap (Section 3.1). **StBo** for Standard smoothed bootstrap on residuals (Section 3.1).
* outlier-detection-procedures: here you will find the iterative process described in section 2.3 and 3.1 for the cutoff estimation.
* depth: functional depth to implement
* power-study: here you will find customised functions to replicate tables on section 4.3.
* results: here you will find the calculus error for the different scenarios. 



# REFERENCES

Raña, P., Aneiros, G. and Vilar, J. (2015) Detection of outliers in functional time series. Environmetrics, 26, 178–191.
