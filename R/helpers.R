PlotCloud <- function(simdata){
  plot(c(1:dim(simdata)[1]),simdata[,1],col=gray(0.7),type="l",ylim=c(0,max(simdata)),xlab="Years",ylab="Abundance")
  
  for(r in 2:ncol(simdata)){
    lines(c(1:dim(simdata)[1]),simdata[,r],col=gray(0.7),type="l")
  }
}


Ricker <- function(prev_abund, R_max, SD_lambda, K){       # this is a function for computing next-year abundance -- includes env stochasticity
  prev_abund * exp(log(rnorm(1,R_max,SD_lambda))*(1-(prev_abund/K)))
}


PVAdemo <- function(nreps,nyears,Init_N,R_max,SD_lambda, K,Flood_prob,Flood_lambda){
  #browser()
  PopArray2 <- array(0,dim=c((nyears+1),nreps))
  
  ## start looping through replicates
  
  for(rep in 1:nreps){
    
    # set initial abundance
    PopArray2[1,rep] <- Init_N     # initial abundance
    
    ### loop through years
    for(y in 2:(nyears+1)){
      ### stochasticity and d-d
      nextyear <- max(0,trunc(Ricker(PopArray2[y-1,rep], R_max, SD_lambda, K)))
      
      ### catastrophe
      if(runif(1)<Flood_prob) nextyear <- nextyear*Flood_lambda
      PopArray2[y,rep] <- nextyear 
    }
  }
  
  return(PopArray2)
}