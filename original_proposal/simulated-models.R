#* uncontaminated model

uncontaminated <- function(rho,m=30,n=200,t=c(-0.5,1.5),plot=TRUE)
{

  l = length((-n+1):n)
  b = arima.sim(list(order = c(1,0,0), ar = rho), n = l,innov = rnorm(l,0,1))
  TT = seq(from = t[1], to = t[2],length = m) #* tiempos en los que se observa
  
  xx = matrix(nrow=m,ncol=l) 
  xx[,1] = cos(pi*TT)
  for(i in 2:l)
  { 
    xx[,i] <- cos(pi*TT)*(1-rho)+rho*xx[,i-1] + rnorm(1,0,0.3)*sin(pi*TT) + b[i]
  }
  
  x = xx[,(l-(n-1)):l] 
  
  TT = seq(from = t[1], to = t[2],length = m) 
  fdataobj <- fdata(t(x),argvals = TT,rangeval = c(min(TT),max(TT)))
  row.names(fdataobj[["data"]]) <- 1:n
 
  
  if(plot==TRUE){
    x11()
    plot(fdataobj,col="grey",ylab = "Xi(t)", xlab = "t"
         ,ylim = c(-30,35),main ="Uncontaminated model")
    last=n
    x11()
    plot.ts(c(x[,(n-last+1):n]),ylim=c(min(x[,(n-last):n])-0.5,0.5
                                       +max(x[,(n-last):n])),axes=F,xlab="",ylab="",lwd=1
            , main ="no contaminated time serie")
    axis(2); axis(1,tick=F,labels=F); abline(h=0)
    box()
  }
  
  
  return(fdataobj)
}

#* model with magnitude outliers  

magnitude <- function(rho,k=10,plot=TRUE,t=c(-0.5,1.5)){
  
  fdataobj <- uncontaminated(rho=rho,plot=plot)
  
  m = ncol(fdataobj)
  n = nrow(fdataobj)
  o = runifdisc(n=3,max=n,min=1)
  
  
  Xi3 <- x <- fdataobj[["data"]]
  
  Xi3[o,] <- x[o,]+k
  
  TT = seq(from = t[1], to = t[2],length = m) 
  fdataobj <- fdata(Xi3,argvals = TT,rangeval = c(min(TT),max(TT)))
  
  if(plot==TRUE){
    x11()
    plot(fdataobj,col="grey",ylab = "Xi(t)", xlab = "t"
         ,ylim = c(-30,35),main= "magnitude outliers")
    lines(fdataobj[o,],col="black",lty=1,lwd=2)
    
    last=n
    x11()
    plot.ts(c(t(Xi3[(n-last+1):n,])),ylim=c(min(t(Xi3[(n-last):n,]))-0.5,0.5
                                            +max(t(Xi3[(n-last):n,]))),axes=F,xlab="",ylab="",lwd=1
            ,main= "time serie with magnitude ouliers")
    axis(2); axis(1,tick=F,labels=F); abline(h=0)
    abline(v=o*30, lty=2);box()
  }
  
  list = list(fdataobj=fdataobj,outliers=sort(o))
  return(list)
}

#* model with shape outliers

shape <- function(rho,k=4,plot=TRUE,t=c(-0.5,1.5)){
  
  fdataobj <- uncontaminated(rho=rho,plot=plot)
  
  m = ncol(fdataobj)
  n = nrow(fdataobj)
  I1 = sample(1:n,1);I2 = sample(1:n,1);I3 = sample(1:n,1)
  o = runifdisc(n=3,max=n,min=1)
  
  Xi4 <- x <- fdataobj[["data"]]
  
  TT = seq(from = t[1], to = t[2],length = m) 
  
  for(i in 1:length(o)){
    Xi4[o[i],] <- x[o[i],]+k*cos(3*pi*TT)
  }
  
  fdataobj <- fdata(Xi4,argvals = TT,rangeval = c(min(TT),max(TT)))
  
  if(plot==TRUE){
    x11()
    plot(fdataobj,col="grey",ylab = "Xi(t)", xlab = "t",ylim = c(-30,35))
    lines(fdataobj[o,],col="black",lty=1,lwd=2)
    x11()
    last=n
    plot.ts(c(t(Xi4[(n-last+1):n,])),ylim=c(min(t(Xi4[(n-last):n,]))-0.5,0.5
                                            +max(t(Xi4[(n-last):n,]))),axes=F,xlab="",ylab="",lwd=1)
    axis(2); axis(1,tick=F,labels=F); abline(h=0)
    abline(v=o*30, lty=2);box()
  }
  
  list = list(fdataobj=fdataobj,outliers=sort(o))
  return(list)
}

#* model partially contaminated  

partial <- function(rho,k=10,plot=TRUE,t=c(-0.5,1.5)){
  
  fdataobj <- uncontaminated(rho=rho,plot=plot)
  
  m = ncol(fdataobj)
  n = nrow(fdataobj)
  I1 = sample(1:n,1);I2 = sample(1:n,1);I3 = sample(1:n,1)
  o = runifdisc(n=3,max=n,min=1)
  
  Xi5 <- x <- fdataobj[["data"]]
  
  
  TT = seq(from = t[1], to = t[2],length = m) 
  
  for (i in 1:length(o)){
    Ti = runif(1, -0.5, 1.5)
    change = TT >= Ti
    Xi5[o[i], change] =  x[o[i],change] + k
  } 
  
  fdataobj <- fdata(Xi5,argvals = TT,rangeval = c(min(TT),max(TT)))
  
  if(plot==TRUE){
    x11()
    plot(fdataobj,col="grey",ylab = "Xi(t)", xlab = "t",ylim = c(-30,35))
    lines(fdataobj[o,],col="black",lty=1,lwd=2)
    x11()
    last=n
    plot.ts(c(t(Xi5[(n-last+1):n,])),ylim=c(min(t(Xi5[(n-last):n,]))-0.5,0.5
                                            +max(t(Xi5[(n-last):n,]))),axes=F,xlab="",ylab="",lwd=1)
    axis(2); axis(1,tick=F,labels=F); abline(h=0)
    abline(v=o*30, lty=2);box()
  }
  
  
  list = list(fdataobj=fdataobj,outliers=sort(o))
  return(list)
  
}

#* multivariate Model 

multifdata <- function(rho,model=shape,k=4,plot=TRUE,dim = 3){
  
  obj <- model(rho=rho,k=k,plot=FALSE)
  
  # original Process
  fdataobj <- obj$fdataobj
  outliers <- obj$outliers
  
  # firts Derivative 
  deriv1 <- fdata.deriv(fdataobj)
  row.names(deriv1[["data"]]) <- row.names(fdataobj[["data"]])
  
  # second Derivative 
  deriv2 <- fdata.deriv(fdataobj,2)
  row.names(deriv2[["data"]]) <- row.names(fdataobj[["data"]])
  
  
  # multivariate Functional Object 
  if(dim==3){
    mfdata <- list(original = fdataobj$data,firt = deriv1$data, second =deriv2$data)
    
    if(plot==TRUE){
      x11()
      par(mfrow=c(1,3))
      plot(fdataobj, main="original process",col="gray")
      lines(fdataobj[outliers,],col="black",lty=1,lwd=2)
      plot(deriv1, main="firts derivative",col="gray")
      lines(deriv1[outliers,],col="black",lty=1,lwd=2)
      plot(deriv2, main="second derivative",col="gray")
      lines(deriv2[outliers,],col="black",lty=1,lwd=2)
    }
  } else if (dim==2){
    
    mfdata <- list(original = fdataobj$data,firt = deriv1$data)
    
    if(plot==TRUE){
      x11()
      par(mfrow=c(1,2))
      plot(fdataobj, main="original process")
      lines(fdataobj[outliers,],col="black",lty=1,lwd=2)
      plot(deriv1, main="first derivative")
      lines(deriv1[outliers,],col="black",lty=1,lwd=2)
    } 
  } else if(dim==1){
    
    mfdata <- list(original = fdataobj$data)
    
    if(plot==TRUE){
      x11()
      plot(fdataobj, main="original process")
      lines(fdataobj[outliers,],col="black",lty=1,lwd=2) 
    }
  } else {"incorrect dimension" }
  
  
  list = list(mfdataobj=mfdata,outliers = outliers)
  return(list)
}
