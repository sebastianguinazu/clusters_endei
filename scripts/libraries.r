
library(tidyverse)
library(dplyr)
library(foreign)
library(glue)
library(ggplot2)
library(skimr)
library(stringi)

library(googlesheets4)
library(RCurl)

library(factoextra)
library(clValid)
library(NbClust)

library(amap)


# Hopkins con manhattan
hopkins = function(data,n,byrow=F,header=F) {
  if(is.data.frame(data))
    data<-as.matrix(data)
  if (!(is.matrix(data)))
    stop("data must be data.frame or matrix") 
  if(n>=nrow(data))
    stop("n must be no larger than num of samples")
  if(byrow==T) data<-t(data)
  if(header==T) data<-data[-1,]
  c<-apply(data,2,min)#minimum value per colume
  d<-apply(data,2,max)
  p<-matrix(0,ncol=ncol(data),nrow=n)#n vectors of space
  for(i in 1:ncol(data))
  {
    p[,i]<-runif(n,min=c[i],max=d[i])
  }
  k<-round(runif(n,1,nrow(data)))
  q<-as.matrix(data[k,])
  distp=rep(0,nrow(data))
  #distq=rep(0,nrow(data)-1)
  distq=0;
  minp=rep(0,n)
  minq=rep(0,n)
  for(i in 1:n)
  {
    distp[1]<-dist(rbind(p[i,],data[1,]), method = 'manhattan')
    minqi<-dist(rbind(q[i,],data[1,]), method = 'manhattan')
    for(j in 2:nrow(data))
    {
      distp[j]<-dist(rbind(p[i,],data[j,]), method = 'manhattan')
      error<-q[i,]-data[j,]
      if(sum(abs(error))!=0)
      {
        #distq[j]<-dist(rbind(q[i,],data[j,]))
        distq<-dist(rbind(q[i,],data[j,]), method = 'manhattan')
        if(distq<minqi)
          minqi<-distq;
      }
    }
    minp[i]<-min(distp)
    # minq[i]<-apply(distq,1,min)
    minq[i]<-minqi;
  }
  list(H= 1 - ((sum(minq)/(sum(minp)+sum(minq)))))
}
