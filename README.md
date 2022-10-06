# Functional outlier time series detection

This repo contains a proposal for functional time series outlier detection based on multivariate functional processes and functional depths.

# [Orinigal proposal](https://github.com/DavidSolan0/functional_outlier_time_series_detection/tree/main/original_proposal)

The original proposal offers two alternatives to detect functional time series outliers. One of them is a non-parametric approach based on bootstrap procedures. However, this approach must be more robust in seeing the shape outliers and partially contaminated observations. From calculus, derivatives offer us information about a function's shape. I took this principle to modify the initial idea and extended it to a multivariate environment looking to capture more details about curves' shapes. The following image shows us that the masking effect that some depth functions could suffer to detect shape outliers is avoided using the original process's first and second derivatives information.

![image](https://user-images.githubusercontent.com/80591909/174551330-af6fa88f-4463-4483-ad27-a6677ef7e889.png)

# [Multivariate extension]()

From simulation studies and graphically it could be proof that derivatives information allows detection of shape outliers and partially contaminated observations with high precision and almost no error. However, this information could mask magnitude outliers. To sum up, this approach is strongly recommended if you are sure you do not have magnitude but shape outliers in your functional time series. 
