outlier_multivariate_bootstrap <- function(mfdataobj ,nb = 200, smo = 0.05, quan=0.5,
                               dfunc = multiMBD,l=4,p=0.1,ns=0.01,boot=multiMBBo,
                               weights=c(1/3,1/3,1/3)) {  
  
  # params:
   # mfdataobj = a multivariate functional data object.  
   # nb = bootstrap sample.
   # smo = smoothness parameter.
   # quan = quantile to obtain cutoff estimation from the bootstrap procedure.
   # dfunc = depht to use.
   # l = Block size (MBBo).
   # p = success probability for geometric ditribution (StBo).
   # ns = quantile to calculate in each bootstrap iteration to cutoff estimation.
   # boot = bootstrap type. SmBoD to Standar. MBBo to moving block. StBo to Standard smoothed bootstrap on residuals.
   # weights = weights to assign for each univariate process in the multivariate process. its length will be equal to the multivariate process dimension.   
  
  if (!is.list(mfdataobj)) 
    "object must be a list"
  
  if (is.null(row.names(mfdataobj[[1]]))) 
    row.names(mfdataobj[[1]]) = 1:n
  
  dim <- length(mfdataobj)
  n <- nrow(mfdataobj[[1]])
  
  cutoff <- quantile(boot(mfdataobj, dfunc = dfunc,nb = nb,ns=ns,weights=weights),probs = quan)
  
  hay <- 1
  outliers <- dep.out <- ite <- c()
  ii <- 1
  curvasgood <- mfdataobj
  
  d <- dfunc(curvasgood,weights = weights)
  
  rwn = names(d) = rownames(curvasgood[[1]]) = 1:n
  
  while (hay == 1) {
    if (is.null(outliers)) {
      dtotal <- d
    }
    cutt <- d < cutoff
    fecha <- as.numeric(rownames(curvasgood[[1]])[cutt])
    elim <- which(cutt)
    if (length(elim) > 0) {
      dep.out <- c(dep.out, d[elim])
      
      for(i in 1:dim)
      {
        curvasgood[[i]] <- curvasgood[[i]][-elim,]
      }
      
      outliers <- c(outliers, fecha)
    }
    if (length(elim) == 0 || length(outliers) > n/5) {
      hay <- 0
    }
    else {
      d <- dfunc(curvasgood,weights = weights)
    }
    ite <- c(ite, rep(ii, length(elim)))
    ii <- ii + 1
  }
  outliers <- rownames(mfdataobj[[1]])[unique(outliers)]
  names(dep.out) <- NULL
  return(list(outliers = outliers,dep.out=dep.out ,iteration = ite, 
              quantile = cutoff, Dep = dtotal))
}