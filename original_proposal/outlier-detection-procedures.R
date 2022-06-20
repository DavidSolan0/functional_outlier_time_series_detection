
outlier_bootstrap <- function(fdataobj, nb = 200, smo = 0.05, quan=0.5,
                         dfunc = MBD, l=4, p=0.1, ns=0.01, boot=SmBoD) 
{
  
  # params:
   # fdataobj = a functional data object usually obtained from fda 
   # nb = bootstrap sample
   # smo = smoothness parameter 
   # quan = quantile to obtain cutoff estimation from the bootstrap procedure
   # dfunc = depht to use
   # l = Block size (MBBo)
   # p = success probability for geometric ditribution (StBo)
   # ns = quantile to calculate in each bootstrap iteration to cutoff estimation
   # boot = bootstrap type. SmBoD to Standar. MBBo to moving block. StBo to Standard smoothed bootstrap on residuals

    
  if (!is.fdata(fdataobj)) 
    fdataobj <- fdata(fdataobj)
  nas1 <- is.na(fdataobj)
  if (any(nas1)) 
    stop("fdataobj contain ", sum(nas1), " curves with some NA value \n")
  x <- fdataobj[["data"]]
  tt <- fdataobj[["argvals"]]
  rtt <- fdataobj[["rangeval"]]
  n <- nrow(fdataobj)
  m <- ncol(fdataobj)
  if (is.null(n) && is.null(m)) 
    stop("ERROR IN THE DATA DIMENSIONS")
  if (is.null(row.names(fdataobj[["data"]]))) 
    row.names(fdataobj[["data"]]) = 1:n
  
  cutoff <- quantile(boot(fdataobj, dfunc = dfunc, nb = nb, smo = smo, ns=ns),
                     probs = quan)
  
  hay <- 1
  outliers <- dep.out <- ite <- c()
  ii <- 1
  curvasgood <- fdataobj
  
  d <- dfunc(curvasgood[["data"]])
  
  rwn = names(d) = rownames(curvasgood[["data"]]) = 1:n
  while (hay == 1) {
    if (is.null(outliers)) {
      dtotal <- d
    }
    cutt <- d < cutoff
    fecha <- as.numeric(rownames(curvasgood[["data"]])[cutt])
    elim <- which(cutt)
    if (length(elim) > 0) {
      dep.out <- c(dep.out, d[elim])
      curvasgood <- curvasgood[-elim, ]
      outliers <- c(outliers, fecha)
    }
    if (length(elim) == 0 || length(outliers) > n/5) {
      hay <- 0
    }
    else {
      d <- dfunc(curvasgood[["data"]])
    }
    ite <- c(ite, rep(ii, length(elim)))
    ii <- ii + 1
  }
  outliers <- rownames(fdataobj[["data"]])[outliers]
  names(dep.out) <- NULL
  return(list(outliers = as.numeric(outliers),dep.out=dep.out ,iteration = ite, 
              quantile = cutoff, Dep = dtotal))
}

FBox <- function(fdataobj,dfunc=MBD,boot=boot){
  curvasgood  <- fdataobj 
  depths <- dfunc(curvasgood[["data"]])
  o <- outpoint <- fda::fbplot(t(curvasgood[["data"]]),plot=FALSE,depth = depths)$outpoint
  
  if(length(outpoint)!=0){
    curvasgood <- curvasgood[-outpoint]
    
    while(length(o)!=0){
      depths <- dfunc(curvasgood[["data"]])
      o <- fda::fbplot(t(curvasgood[["data"]]),plot=FALSE, depth = depths)$outpoint
      curvasgood <- curvasgood[-o]
      outpoint = c(outpoint,o)
    }
  }
  return(list(outliers=outpoint))
}