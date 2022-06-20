
tasas.nc <- function(rho,method=Outlier.boot,boot=SmBoD,M=100,dfunc=MBD,ns=0.01){
  
  # this function calculates the error ratios for the uncontaminated model 
  # params:
   # rho: correlation parameter. 
   # method: outlier detection procedure. 
   # m.boot: bootstrap procedure to estimate the cutoff
   # M: number of simulations 
   # dfunc: functional depth to use
   # ns: quantile to use in each bootstrap iteration to estimate the cutoff
  
  pf = NULL
  for(l in 1:M){
    
    x <- uncontaminated(rho=rho,plot=FALSE)
    
    depths <- dfunc(x[["data"]])
    fdataobj <- step1(x,depths)
    
    d <- method(fdataobj,dfunc = dfunc,boot=boot)$outliers
    
    pf[l] <- ifelse(length(d)!=0, length(d)/200, 0)
    
  }
  p.f = mean(pf)
  return(list(pf=p.f ,vf=pf))
}

tasas <- function(rho,k=10,model=magnitude,method=Outlier.boot,M=100,dfunc=MBD
                  ,boot=SmBoD){
  
  # this function calculates the error ratios for the contaminated models 
  # params:
   # rho: correlation parameter. 
   # method: outlier detection procedure. 
   # boot: bootstrap procedure to estimate the cutoff
   # M: number of simulations 
   # dfunc: functional depth to use
   # ns: quantile to use in each bootstrap iteration to estimate the cutoff
  
  
  pf = NULL;pc=NULL;cut = NULL
  for(l in 1:M){
    
    fit <- model(rho=rho,k=k,plot=FALSE)
    
    fdataobj = fit$fdataobj
    
    g <- fit$outliers # Outliers generado
    
    resultado <- method(fdataobj,dfunc = dfunc,boot=boot)
    
    d <-  resultado$outliers #Outliers detectados
    
    cut[l] <- resultado$quantile
    
    f=0;c=0
    
    if(length(d)!=0) {
      for(i in 1:length(d)){
        if((sum(d[i]==g)==1)==TRUE) c =  c + 1 else f = f + 1 
      }
      
      f<-f/197
      c<-c/length(g)
      pf[l]=f
      pc[l]=c
      
    } else {
      pf[l]=0
      pc[l]=0
    }
    
    #if(l %% 10 == 0)
     # cat("percentage = ", l, "% \n")
    
  }
  
  p.f = mean(pf)
  p.d.c = mean(pc!=0)
  p.c = mean(pc)
  sd = sd(pc)
  cutoff = mean(cut)
  
  if(!is.na(cutoff))
    return(list(pf=p.f, pc=p.c, sd = sd, pdc=p.d.c, cutoff = cutoff))
  
  if(is.na(cutoff))
    return(list(pf=p.f, pc=p.c, sd = sd, pdc=p.d.c))
  
}


rates <- function(rho,k1=10,k2=4,k3=10,method=Outlier.boot,M=100,dfunc=MBD
                  ,boot=MBBo){
  
  # this function calculates the error ratios for the contaminated model with all types of contamination
  # params:
   # rho: correlation parameter. 
   # method: outlier detection procedure. 
   # boot: bootstrap procedure to estimate the cutoff
   # M: number of simulations 
   # dfunc: functional depth to use
   # ns: quantile to use in each bootstrap iteration to estimate the cutoff
  
  pf = NULL;pc=NULL;cut = NULL
  for(l in 1:M){
    
    fit <- f_data(0.8,k1=k1,k2=k2,k3=k3,plot=FALSE)
    
    fdataobj = fit$fdataobj
    
    g <- fit$outliers # Outliers generado
    
    resultado <- method(fdataobj,dfunc = dfunc,boot=boot)
    
    d <-  resultado$outliers #Outliers detectados
    
    cut[l] <- resultado$quantile
    
    f=0;c=0
    
    if(length(d)!=0) {
      for(i in 1:length(d)){
        if((sum(d[i]==g)==1)==TRUE) c =  c + 1 else f = f + 1 
      }
      
      f<-f/(nrow.fdata(fdataobj)-length(g))
      c<-c/length(g)
      pf[l]=f
      pc[l]=c
      
    } else {
      pf[l]=0
      pc[l]=0
    }
    
  }
  p.f = mean(pf)
  p.d.c = mean(pc!=0)
  p.c = mean(pc)
  sd = sd(pc)
  cutoff = mean(cut)
  
  if(!is.na(cutoff))
    return(list(pf=p.f,pc=p.c,sd = sd,pdc=p.d.c,cutoff = cutoff))
  
  if(is.na(cutoff))
    return(list(pf=p.f,pc=p.c,sd = sd,pdc=p.d.c))
}

