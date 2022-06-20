library(fda)
library(roahd)
library(fda.usc)
library(mrfDepth)

#* uniform Discrete

runifdisc<-function(n, min=0, max=1) sample(min:max, n, replace=TRUE)

#* first step of the cut off estimation algorithm

step1 <- function(fdataobj,depths)
{
  o <- fda::fbplot(t(fdataobj$data),plot=FALSE,depth = depths)$outpoint
  
  if(length(o)!=0)
    fdataobj[["data"]] <- fdataobj[["data"]][-o,]
  
  return(fdataobj)
}

#* clock's i curves 

Bi <- function(i,l) i:(i+l-1)

#* module

B <- function(i,n,l){
  vector <-  i:(i+l-1)%%n
  mod <- vector%%n
  cam <- mod==0
  vector[cam] <- n
  vector
}
