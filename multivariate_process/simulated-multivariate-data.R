multifdata<- function(rho,model=shape,k=4,plot=TRUE,dim = 3){
  
  obj <- model(rho=rho,k=k,plot=FALSE)
  
  #Proceso original
  fdataobj <- obj$fdataobj
  outliers <- obj$outliers
  
  #Primera derivada
  deriv1 <- fdata.deriv(fdataobj)
  row.names(deriv1[["data"]]) <- row.names(fdataobj[["data"]])
  
  #Segunda derivada 
  deriv2 <- fdata.deriv(fdataobj,2)
  row.names(deriv2[["data"]]) <- row.names(fdataobj[["data"]])
  
  
  #Objeto funcional multivariado 
  if(dim==3){
    mfdata <- list(original = fdataobj$data,firt = deriv1$data, second =deriv2$data)
    
    if(plot==TRUE){
      x11()
      par(mfrow=c(1,3))
      plot(fdataobj, main="proceso original",col="gray")
      lines(fdataobj[outliers,],col="black",lty=1,lwd=2)
      plot(deriv1, main="primera derivada",col="gray")
      lines(deriv1[outliers,],col="black",lty=1,lwd=2)
      plot(deriv2, main="segunda derivada",col="gray")
      lines(deriv2[outliers,],col="black",lty=1,lwd=2)
    }
  } else if (dim==2){
    
    mfdata <- list(original = fdataobj$data,firt = deriv1$data)
    
    if(plot==TRUE){
      x11()
      par(mfrow=c(1,2))
      plot(fdataobj, main="proceso original")
      lines(fdataobj[outliers,],col="black",lty=1,lwd=2)
      plot(deriv1, main="primera derivada")
      lines(deriv1[outliers,],col="black",lty=1,lwd=2)
    } 
  } else if(dim==1){
    
    mfdata <- list(original = fdataobj$data)
    
    if(plot==TRUE){
      x11()
      plot(fdataobj, main="proceso original")
      lines(fdataobj[outliers,],col="black",lty=1,lwd=2) 
    }
  } else {"Dimensión incorrecta" }
  
  
  list = list(mfdataobj=mfdata,outliers = outliers)
  return(list)
}
