library(fda)
library(MVN)
library(roahd)
library(knitr)
library(dplyr)
library(fda.usc)
library(mrfDepth)
library(fdaoutlier)
library(CerioliOutlierDetection)

# Load global functions
source("R/utils.R")
source("R/simulated-models.R")
source("R/calculate_rates_results.R")

# Load local functions
source("R/dirout/bootstrap-procedures.R")
source("R/dirout/outlier-detection-procedures.R")
source("R/dirout/power-study-functions.R")


#** Univariate model **#

#* Parameters
M <- 100
seed <- 1234
rho <- 0.8
dfunc <- "random_projections"
boot <- MBBo.DirOut
method <- outlier_dirout

#* Uncontaminated
K <- 0
tabla <- run_simulation_dirout(
    K,
    rho = rho,
    method = method,
    M = M,
    dfunc = dfunc,
    boot = boot,
    seed = seed
)
tabla

#* Magnitude
K <- c(10, 15, 20, 25)
tabla <- run_simulation_dirout(
    K,
    rho = rho,
    method = method,
    M = M,
    dfunc = dfunc,
    boot = boot,
    seed = seed
)
tabla

#* Shape
K <- c(4, 5, 6, 7)
tabla <- run_simulation_dirout(
    K,
    rho = rho,
    method = method,
    M = M,
    dfunc = dfunc,
    boot = boot,
    model = shape,
    seed = seed
)
tabla

#* Partial
K <- c(10, 15, 20, 25)
tabla <- run_simulation_dirout(
    K,
    rho = rho,
    method = method,
    M = M,
    dfunc = dfunc,
    boot = boot,
    seed = seed,
    model = partial
)
tabla

#** Multivariate model **#

#* Parameters
boot <- multiMBBo.DirOut
method <- multivariate_outlier_dirout

#* Magnitude
K <- c(10, 15, 20, 25)
tabla <- run_simulation_dirout(
    K,
    rho = rho,
    method = method,
    M = M,
    dfunc = dfunc,
    boot = boot,
    seed = seed,
    model = magnitude,
    multivariate = TRUE
)
tabla

#* Shape
K <- c(4, 5, 6, 7)
tabla <- run_simulation_dirout(
    K,
    rho = rho,
    method = method,
    M = M,
    dfunc = dfunc,
    boot = boot,
    seed = seed,
    model = shape,
    multivariate = TRUE
)
tabla

#* Partial
K <- c(10, 15, 20, 25)
tabla <- run_simulation_dirout(
    K,
    rho = rho,
    method = method,
    M = M,
    dfunc = dfunc,
    boot = boot,
    seed = seed,
    model = partial,
    multivariate = TRUE
)
tabla

# #* Mixed

# K <- list(c(10, 4, 10), c(15, 5, 15), c(20, 6, 20), c(25, 7, 25))
# depths <- c(MBD, MD)

# set.seed(1234)
# for (k in 2:4) {
#   rate <- rates(0.8,
#     k1 = K[[i]][1], k2 = K[[i]][2], k3 = K[[i]][3],
#     dfunc = depths[i], method = Outlier.DirOut, M = 100, boot = MBBo.DirOut
#   )
#   vector <- c(rate$pf, rate$pc, rate$sd, rate$pdc)
#   names(vector) <- paste0(c("pf", "pc", "sd", "pdc"), "-", k)

#   if (k == 1) tabla_depth <- t(vector)
#   if (k != 1) tabla_depth <- cbind(tabla_depth, t(vector))
# }

# tabla <- tabla_depth

# row.names(tabla) <- "DirOut"
# tabla <- tabla %>% data.frame()

# # setwd(paste0(workspace,'/outputs'))

# write.csv2(tabla, "tabla-mixed-DirOut.csv", row.names = F)
# write.csv2(
#   tabla %>%
#     select(pf.10, pc.10, pf.15, pc.15, pf.20, pc.20, pf.25, pc.25),
#   "tabla-mixed-paper-DirOut.csv",
#   row.names = F
# )
