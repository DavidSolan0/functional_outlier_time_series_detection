library(fda)
library(dplyr)
library(roahd)
library(fda.usc)
library(mrfDepth)
library(fdaoutlier)


source("R/original_proposal/outlier-detection-procedures.R")
source("R/original_proposal/bootstrap-procedures.R")
source("R/multivariate_process/outlier-detection-procedures.R")
source("R/multivariate_process/bootstrap-procedures.R")
source("R/sliding_window/outlier-detection-procedures.R")

source("R/dirout/outlier-detection-procedures.R")
source("R/dirout/bootstrap-procedures.R")
source("R/utils.R")


# Data temperature2 tiene datos cada 10 minutos. CORRECTO
# Read data
temp_data <- read.csv("data/temperature2.csv", sep = ";", dec = ".")

# Extract date and time components
temp_data$datetime <- as.POSIXct(temp_data$Fecha, format = "%d/%m/%y %H:%M")
temp_data$date <- as.Date(temp_data$datetime)
temp_data$minute <- format(temp_data$datetime, "%H:%M")

# Get temperature values
temp_data$temp <- as.numeric(temp_data$Valor)

# Filter temp_data 2011-10-31
temp_data <- temp_data[temp_data$date != "2011-10-31", ]

# Sort temp_data by date and minute
# temp_data <- temp_data %>% arrange(date, minute)

# Reshape data into wide format with minutes as columns and days as rows
temp_wide <- reshape(
    temp_data[c("date", "minute", "temp")],
    idvar = "date",
    timevar = "minute",
    direction = "wide",
    v.names = "temp"
)

# TEMPORAL fix
columns_to_fix <- c(
    "temp.00:00",
    "temp.00:10",
    "temp.00:20",
    "temp.00:30",
    "temp.00:40",
    "temp.00:50"
)
temp_wide[nrow(temp_wide), columns_to_fix] <- 6.55

# Clean up column names
colnames(temp_wide) <- gsub("temp.", "", colnames(temp_wide))
dim(temp_wide)
head(temp_wide)
str(temp_wide)

# Set row names and remove date column
row.names(temp_wide) <- temp_wide$date
temp_wide <- temp_wide %>% dplyr::select(-date)

# Convert to fdata object
temp_fdata <- fdata(
    temp_wide,
    argvals = seq_len(ncol(temp_wide)),
    rangeval = c(1, ncol(temp_wide))
)

# Add row names
row.names(temp_fdata[["data"]]) <- row.names(temp_wide)

# Get first derivative
deriv1 <- fdata.deriv(temp_fdata)
row.names(deriv1[["data"]]) <- row.names(temp_wide)

# Get second derivative
deriv2 <- fdata.deriv(temp_fdata, 2)
row.names(deriv2[["data"]]) <- row.names(temp_wide)

# Multivariate functional object
mfdata <- list(
    original = temp_fdata$data, firt = deriv1$data, second = deriv2$data
)

#* Finded for Peña
original_outliers <- c(12, 13, 76, 96, 115, 116, 119)
row.names(temp_fdata[["data"]])[original_outliers]

plot(temp_fdata, col = "grey", ylab = "Xi(t)", xlab = "t")
lines(temp_fdata[original_outliers, ], col = "black", lty = 1, lwd = 2)


#* Original proposal outlier detection method
original_results <- outlier_bootstrap(
    temp_fdata,
    dfunc = MBD,
    boot = MBBo
)
original_results$quantile

# Number of outliers
original_results$outliers
original_results$depths["2011-11-13"]
length(original_results$outliers) / nrow(temp_fdata)

# Graphical identification of outliers
plot(temp_fdata, col = "grey", ylab = "Xi(t)", xlab = "t")
lines(temp_fdata[original_results$outliers, ], col = "black", lty = 1, lwd = 2)


#* Multivariate version of the original proposal
original_results_multivariate <- outlier_multivariate_bootstrap(
    mfdata,
    dfunc = multiMBD,
    boot = multiMBBo
)

# Number of outliers
original_results_multivariate$outliers
length(original_results_multivariate$outliers) / nrow(temp_fdata)


#* Functional boxplot outlier detection method on multivariate functional object

# Outlier detection
mfdata_outliers <- multivariate_functional_boxplot(
    mfdata,
    dfunc = multiMBD
)

# Number of outliers
row.names(temp_fdata[["data"]])[mfdata_outliers$outliers]
length(mfdata_outliers$outliers) / nrow(temp_fdata)

median_curve <- fdata(
    apply(temp_fdata$data, 2, median),
    argvals = temp_fdata$argvals
)

# Graphical identification of outliers
plot(temp_fdata,
    col = "grey",
    ylab = "Temperature",
    xlab = "t: Time (one unit=10 minutes)",
    main = "",
    cex.lab = 2,
    cex.axis = 2
)
lines(median_curve, col = "black", lwd = 2, lty = 1)
lines(temp_fdata[mfdata_outliers$outliers, ],
    col = "red",
    lty = 2,
    lwd = 2,
)


#* Outlier detection using dirout univatiare version
outlier_dirout_results <- outlier_dirout(
    temp_fdata,
    dfunc = "random_projections",
    boot = MBBo.DirOut,
    plot = TRUE
)

# See how many outliers were found in each iteration
outlier_dirout_results$iteration

# Number of outliers
outlier_dirout_results$outliers
length(outlier_dirout_results$outliers) / nrow(temp_fdata)

# Graphical identification of outliers
plot(temp_fdata, col = "grey", ylab = "Xi(t)", xlab = "t")
lines(temp_fdata[outlier_dirout_results$outliers, ],
    col = "black",
    lty = 1,
    lwd = 2
)


#* Outlier detection using dirout multivariate version
mfdata_array <- array(
    unlist(mfdata),
    dim = c(
        dim(mfdata[[1]])[1],
        dim(mfdata[[1]])[2],
        length(mfdata)
    )
)
rownames(mfdata_array) <- row.names(temp_fdata[["data"]])


multi_outlier_dirout_results <- multivariate_outlier_dirout(
    mfdata_array,
    dfunc = "random_projections",
    boot = multiMBBo.DirOut,
    plot = FALSE
)

# Number of outliers
sort(multi_outlier_dirout_results$outliers)
length(multi_outlier_dirout_results$outliers) / nrow(temp_fdata)

# Graphical identification of outliers
plot(temp_fdata, col = "grey", ylab = "Xi(t)", xlab = "t")
lines(
    temp_fdata[multi_outlier_dirout_results$outliers, ],
    col = "black",
    lty = 1,
    lwd = 2
)

#* Outlier detection using sliding window univariate version
sliding_window_outliers <- sliding_window_outlier(
    temp_fdata,
    dfunc = MBD,
    threshold = 0.5,
    plot = TRUE,
    window_size = 9
)

# Calculate median curve
median_curve <- fdata(
    apply(temp_fdata$data, 2, median),
    argvals = temp_fdata$argvals
)

# Number of outliers
sort(sliding_window_outliers$outliers)
length(sliding_window_outliers$outliers) / nrow(temp_fdata)


#* Graphical outlier detectionsummary

# Unique outliers detected by the proposal
proposal_detected_outliers <- unique(
    c(outlier_dirout_results$outliers, sliding_window_outliers$outliers)
)
proposal_detected_outliers <- sort(proposal_detected_outliers)

# Unique outliers detected by the originalproposal
original_ouliers <- row.names(temp_fdata[["data"]])[original_outliers]

# Mutual detected outliers
mutual_detected_outliers <- intersect(
    proposal_detected_outliers,
    original_ouliers
)
mutual_detected_outliers <- sort(mutual_detected_outliers)

# Graphical identification of outliers
opar <- par(no.readonly = TRUE)
par(mar = c(5, 6, 4, 2) + 0.1) # Increase left margin
plot(
    temp_fdata,
    col = "grey",
    ylab = "Temperature",
    xlab = "t: Time (one unit=10 minutes)",
    main = "",
    cex.lab = 2,
    cex.axis = 2
)
par(opar)
lines(median_curve, col = "black", lwd = 2, lty = 1)
lines(temp_fdata[mutual_detected_outliers, ], col = "red", lty = 2, lwd = 2)


# New outliers detected by the proposal
new_outliers <- setdiff(proposal_detected_outliers, original_ouliers)
new_outliers <- sort(new_outliers)

plot(
    temp_fdata,
    col = "grey",
    ylab = "Temperature",
    xlab = "t: Time (one unit=10 minutes)",
    main = "",
    cex.lab = 2,
    cex.axis = 2
)
lines(median_curve, col = "black", lwd = 2, lty = 1)

colors <- c(
    "#FF0000",
    "#FF8B00",
    "#00C853",
    "#0091FF",
    "#002EFF",
    "#5D00FF",
    "#E800FF",
    "#FF008B"
)

for (i in seq_along(new_outliers)) {
    lines(temp_fdata[new_outliers[i], ],
        col = colors[i],
        lty = 2,
        lwd = 2
    )
}

# Add legend with outlier indices
legend("topleft",
    legend = new_outliers,
    col = colors,
    lty = 2,
    lwd = 2,
    title = "",
    cex = 1,
    bty = "n" # No legend box
)

#* Outlier detection using sliding window multivariate version
# sliding_window_outliers_multivariate <- multivariate_sliding_window_outlier(
#     mfdata[1:2],
#     dfunc = multiMBD,
#     threshold = 0.7,
#     plot = TRUE,
#     window_size = 9
# )

# # Number of outliers
# sort(sliding_window_outliers_multivariate$outliers)
# length(sliding_window_outliers_multivariate$outliers) / nrow(temp_fdata)

# # Graphical identification of outliers
# plot(temp_fdata, col = "grey", ylab = "Xi(t)", xlab = "t")
# lines(temp_fdata[sliding_window_outliers_multivariate$outliers, ], col = "black", lty = 1, lwd = 2)

# # Graphical identification of outliers
# plot(deriv1, col = "grey", ylab = "Xi(t)", xlab = "t")
# lines(deriv1[sliding_window_outliers_multivariate$outliers, ], col = "black", lty = 1, lwd = 2)

# # Graphical identification of outliers
# plot(deriv2, col = "grey", ylab = "Xi(t)", xlab = "t")
# lines(deriv2[sliding_window_outliers_multivariate$outliers, ], col = "black", lty = 1, lwd = 2)


# dirout_msplot <- msplot(temp_fdata[["data"]], plot = TRUE)
# dirout_msplot$outliers
# length(dirout_msplot$outliers) / nrow(temp_fdata)
# row.names(temp_fdata[["data"]])[dirout_msplot$outliers]

# # Graphical identification of outliers
# plot(temp_fdata, col = "grey", ylab = "Xi(t)", xlab = "t")
# lines(temp_fdata[dirout_msplot$outliers, ], col = "black", lty = 1, lwd = 2)

