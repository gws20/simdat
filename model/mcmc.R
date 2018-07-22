#######################################################
##First set up the MCMC sampler

#number of draws sets to create
niter<-10001

#Create Uniform Prior function
prior = function(theta){
  if((theta<0) || (theta>1)){  
    return(0)
  }else{
    return(1)}
}

#Binomial distribution for y|theta (ignoring the constant wrt theta)
likelihood = function(theta, y,n){
  return(theta^(y) * (1-theta)^(n-y))
}

#Want to sample from p(theta|y)
#create a function that takes in an observed y, sample size n, number of iterations to run, a starting value for theta, and the "jumping distribution's" sd
thetasampler <- function(y, n, niter, thetastartval, thetasd){
  
  #vector to store info
  theta<-jump<-r<-rep(0,niter)
  #generate random uniforms for acceptance decision
  runiforms<-runif(niter)
  #use the starting value as the first theta
  theta[1] = thetastartval
  
  #loop through the desired number of iterations
  for(i in 2:niter){
    #temporary value for current theta is previous theta
    currenttheta = theta[i-1]
    
    #get the next random draw add in a random value from the jumping distr. to our theta value
    jump[i]<-rnorm(1,0,thetasd)
    newtheta = currenttheta + jump[i]
    #make sure it is in the appropriate range, else set it to the edges
    if (newtheta<0){ 
      newtheta<-0
    }else if (newtheta>1){
      newtheta<-1
    }
    
    #Find r ratio: 
    r[i]<- prior(theta=newtheta)*likelihood(theta=newtheta,y=y,n=n)/(prior(theta=currenttheta)*likelihood(theta=currenttheta,y=y,n=n))
    
    #accept this new value with prob min(1,r), leave value the same with prob 1-r
    if(runiforms[i]<r[i]){
      theta[i] = newtheta       
    } else {
      theta[i] = currenttheta 
    }
  } 
  
  #Set values for first theta and Keep a variable to determine if new value was kept
  jump[1]<-NA
  runiforms[1]<-NA
  r[1]<-NA
  accept<- r>runiforms
  accept[1]<-TRUE
  
  #return the vector of theta values	
  return(data.frame(theta,jump,runiforms,r,accept))
}