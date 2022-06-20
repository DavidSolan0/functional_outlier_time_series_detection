library(fda)
library(tcltk)
library(roahd)
library(dplyr)
library(fda.usc)
library(mrfDepth)

workspace = 'C:/Users/David.Solano/OneDrive - Ipsos/David/functional_time_series_outlier_detection'
setwd(workspace)

source('utils.R')
source('simulated-models.R')
source('depths.R')
source('bootstrap-procedures.R')
source('outlier-detection-procedures.R')
source('power-study.R')

#* Parameters

M = 100
depths = c(MBD)
nn = length(depths)
outlier_detection_method = outlier_bootstrap
bootstrap_estimation_method = MBBo

#* Magnitude 

K = c(10,15,20,25)
tabla = NULL

set.seed(1234)
for(i in 1:nn){

  iter = 1
  tabla_depth = NULL
  pb = tkProgressBar(title = "progress bar", min = 0, max = length(K), width = 300)
  for(k in K){
    
    rates = tasas(0.8, k=k, dfunc = depths[i][[1]], method = outlier_detection_method, 
                  M = 100, boot = bootstrap_estimation_method)
    
    vector = c(rates$pf,rates$pc,rates$sd,rates$pdc)
    names(vector) = paste0(c('pf','pc','sd','pdc'),'-',k)
    tabla_K = t(vector)
    
    tabla_depth = cbind(tabla_depth,t(vector))
    
    Sys.sleep(0.1)
    setTkProgressBar(pb, iter, label=paste(iter/length(K)*100,"% done"))
    iter = iter + 1
    
  }
  
  tabla = rbind(tabla, tabla_depth)
  
  Sys.sleep(0.5)
  close(pb)
  
}

row.names(tabla) = c('MBD','MD')
tabla

#* Shape

K = c(4,5,6,7)
tabla = NULL

set.seed(1234)
for(i in 1:nn){
  
  iter = 1
  tabla_depth = NULL
  pb = tkProgressBar(title = "progress bar", min = 0, max = length(K), width = 300)
  for(k in K){
    
    rates = tasas(0.8, model = shape, k=k, dfunc=depths[i][[1]], 
                  method = outlier_detection_method, M = M, 
                  boot = bootstrap_estimation_method)
    
    vector = c(rates$pf,rates$pc,rates$sd,rates$pdc)
    names(vector) = paste0(c('pf','pc','sd','pdc'),'-',k)
    tabla_K = t(vector)
    
    tabla_depth = cbind(tabla_depth,t(vector))
    
    Sys.sleep(0.1)
    setTkProgressBar(pb, iter, label=paste(iter/length(K)*100,"% done"))
    iter = iter + 1
    
  }
  
  tabla = rbind(tabla, tabla_depth)
  
  Sys.sleep(0.5)
  close(pb)
  
}

row.names(tabla) = c('MBD','MD')
tabla

#* Partial

K = c(10,15,20,25)
tabla = NULL

set.seed(1234)
for(i in 1:nn){
  
  iter = 1
  tabla_depth = NULL
  pb = tkProgressBar(title = "progress bar", min = 0, max = length(K), width = 300)
  for(k in K){
    
    rates = tasas(0.8, model = shape, k=k, dfunc=depths[i][[1]], 
                  method = outlier_detection_method, M = M, 
                  boot = bootstrap_estimation_method)
    
    vector = c(rates$pf, rates$pc, rates$sd, rates$pdc)
    names(vector) = paste0(c('pf','pc','sd','pdc'),'-',k)
    tabla_K = t(vector)
    
    tabla_depth = cbind(tabla_depth,t(vector))
    
    Sys.sleep(0.1)
    setTkProgressBar(pb, iter, label=paste(iter/length(K)*100,"% done"))
    iter = iter + 1
    
  }
  
  tabla = rbind(tabla, tabla_depth)
  
  Sys.sleep(0.5)
  close(pb)
  
}

row.names(tabla) = c('MBD','MD')
tabla 

#* Mixed

K = list(c(10,4,10),c(15,5,15),c(20,6,20),c(25,7,25))
tabla = NULL

set.seed(1234)
for(i in 1:nn){
  
  iter = 1
  tabla_depth = NULL
  pb = tkProgressBar(title = "progress bar", min = 0, max = length(K), width = 300)
  for(k in 2:4){
    
    rate = rates(0.8, k1=K[[i]][1], k2=K[[i]][2], k3=K[[i]][3],
                  dfunc=depths[i][[1]], method = outlier_detection_method, 
                 M = M, boot = bootstrap_estimation_method)
    
    vector = c(rate$pf, rate$pc, rate$sd, rate$pdc)
    names(vector) = paste0(c('pf','pc','sd','pdc'),'-',k)
    
    tabla_K = t(vector)
    
    tabla_depth = cbind(tabla_depth,t(vector))
    
    Sys.sleep(0.1)
    setTkProgressBar(pb, iter, label=paste(iter/length(K)*100,"% done"))
    iter = iter + 1
    
  }
  
  tabla = rbind(tabla, tabla_depth)
  
  Sys.sleep(0.5)
  close(pb)
  
}

row.names(tabla) = c('MBD','MD')
tabla
