#' False Nearset Neighbour
#'
#' False Nearest Neighbour a dimensionality estimation method.
#' This method uses delay value to reconstruct phase space and identifies dimension
#' based on true neighbours
#' Rossler system, Lorenz Data, and Mackey and Glass data into environment
#'
#' @docType package
#'
#' @usage fnn_algo(x,tau=1,mmax=5)
#'
#' @keywords FNN, Dimension, Phase-Space
#'
#' @examples
#' data(chaosdata)
#'
#' fnn_algo(chaosdata$Lorenz,tau=3,mmax=10)
#'
'fnn_algo'

# Start ----------

#The following code goes like this importing files
#plotting Time Series, plotting Auto Correlation
#Function/Average Mutual Information, plotting Phase Space,
#plotting FNN


# False Nearest Neighbour Algorithm FNN --------------

#x : time series

#mmax : embedding dimension

#tau : time delay

#npoint : total number of reconstructed vectors

#Y : M x m matrix

#rtol=15

#atol=2

#author:"Merve Kizilkaya"


#Input method


fnn_algo<-function(x,tau=1,mmax=10,rtol=15,atol=2,plot=F){

  AMI_func<-function(x){
    library(tseriesChaos)
    lagged_data<-tseriesChaos::mutual(x,lag.max=length(x),
                                      partitions = length(x),plot=F)
    lag_position<-diff(lagged_data,1)
    j<-which(lag_position>0)[1]-1 #-1 is for lag 0
    return(j)
  }

  tau=ifelse(tau=='ACF' #Condition
             ,(which(acf(x,length(x),plot=FALSE)$acf<=0)[1]-1) #Yes
             ,ifelse(tau=='AMI' ,AMI_func(x), tau))

  (N<-length(x))

  Ra<-sd(x)

  FNN<-matrix(data=NA,nrow=mmax)

  psr_deneme<-function(x,m,tau,npoint){

    N<-length(x)

    M=npoint

    Y<-matrix(data=0,M,m)

    for (i in 1:m) {

      Y[,i]<-t(x[(1:M)+(i-1)*tau])

    }

    return(Y)

  }

  for (m in 1:mmax) {

    M<-N-(m*tau)

    Y<-psr_deneme(x,m,tau,M)

    FNN[m,1]<-0

    for (n in 1:M) {

      Y0<-matrix(rep(Y[n,],each=M),nrow = M)

      distance<-sqrt(rowSums((Y-Y0)^2))

      near<-sort(distance,index.return=T)

      names(near)<-c("distance","position")

      near_position_index<-which(near$distance!=0)[1]

      D<-abs(x[n+m*tau]-x[near$position[near_position_index]+m*tau])

      R<-sqrt(D^2+near$distance[near_position_index]^2)

      if (((D/near$distance[near_position_index]) > rtol) || (R/Ra > atol)){

        FNN[m,1]<-FNN[m,1]+1

      }

    }

  }

  FNN=(FNN/FNN[1,1])*100

  if(plot){

    plot(1:mmax,FNN,y,xlim=c(0,10,20,30,50,75,100),"p",ylab="% FNN",xlab="Embedding Dimension",main="False Nearest Neighbour")

  }
  return(FNN)
}

#end ----
