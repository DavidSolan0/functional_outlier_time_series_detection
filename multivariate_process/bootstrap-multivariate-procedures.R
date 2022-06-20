multiMBBo <- function(x,l=4,nb=200,dfunc=multiMBD,ns=0.01
                      ,weights=c(1/3,1/3,1/3)){
  
  # params:
   # x: multivariate functional data
   # l: block size 
   # dfunc: depth function
   # nb: number of bootstrap samples
   # smo: smoothness parameter
   # ns: quantile for cutoff estimation
   # weights = weights to assign for each univariate process in the multivariate process. its length will be equal to the multivariate process dimension.   
  
  
  dim <- length(x)
  n <- nrow(x[[1]]) 
  k <- ceiling(n/l)
  procesos <- x
  
  #* Construcción del SI
  
  depths <- dfunc(x,weights = weights)
  muestra.trim <- multistep1(x,depths)
  
  nn <- nrow(muestra.trim[[1]])
  
  i= 1:(nn-l+1)
  
  cuantiles <- numeric(nb)                                  
  
  for (j in 1:nb){ 
    
    I <- runifdisc(k,min=min(i),max=max(i))
    
    bmuestra.ind <- c()
    
    for(m in 1:k){
      bmuestra.ind <- c(bmuestra.ind,Bi(I[m],l=l))
    }
    
    bmuestra.ind <- bmuestra.ind[1:n]
    bmuestra <- list()
    
    for( i in 1:dim) bmuestra[[i]] <- muestra.trim[[i]][bmuestra.ind,]
    
    d <- dfunc(bmuestra,weights = weights)
    
    cuantiles[j] <- quantile(d, probs = ns,type=8)
  }
  
  return(cuantiles)
}