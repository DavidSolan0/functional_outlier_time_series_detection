multi_tasas <- function(rho, k=4, model=shape, method=outlier_multivariate_bootstrap,
                        M=100, dfunc=multiMBD, boot=multiMBBo, weights=c(1/3,1/3,1/3), dim=3){
  
  pf = NULL;pc=NULL;cut = NULL
  for(l in 1:M){
    
    fit <- multifdata(rho=rho, k=k, model=model, plot=FALSE, dim=dim)
    
    fdataobj = fit$mfdataobj
    
    g <- fit$outliers #observed outliers
    
    resultado <- method(fdataobj, dfunc=dfunc, boot=boot, weights=weights)
    
    d <-  as.numeric(resultado$outliers) #detected outliers
    
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
    
  }
  p.f = mean(pf)
  p.d.c = mean(pc!=0)
  p.c = mean(pc)
  sd = sd(pc)
  cutoff = mean(cut)
  
  return(list(pf=p.f,pc=p.c,sd = sd,pdc=p.d.c,cutoff = cutoff))
}
