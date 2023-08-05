#####################################################################
# Description: This file includes the function to generate the true 
#              log power spectrum of three simulated data in the paper
##################################################################


#### Calculation of True power spectrum
ARspec <- function(phi,sigma,freq)
{
  dim = dim(phi)
  len = dim[2]
  phispect = array(0,c(2,2,length(freq)))
  spect = array(0,c(2,2,length(freq)))
  
  for (k in 1:length(freq))
  {
    phispect[,,k] = diag(2)
    for(j in 1:(len/2))
    {
      if(j==1)
      {
        bigmat = phi[,1:2]*exp(-2*pi*sqrt(-1+0i)*freq[k])
      }else{
        bigmat = phi[,(2*j-1):(2*j)]*exp(-2*j*pi*sqrt(-1+0i)*freq[k])
      }
      
      phispect[,,k] = phispect[,,k] - bigmat
      
    }
    spect[,,k] = sigma%*%solve(phispect[,,k])%*%Conj(solve(phispect[,,k]))
    
  }
  
  return(spect)
  
}


## true power spectrum for three simulations
spectrue=function(sim,Nobs,L)
{
  
  nfreq <- floor(Nobs/2)
  
  if(sim=="Abrupt+Smooth")
  {
    spec_true=matrix(0,(nfreq+1),L)
    ar1=diag(2)
    ar2=diag(2)
    sig=diag(2)
    freq_hat=(0:nfreq)/Nobs
    
    
    for(l in 1:L)
    {
      
      if(x[l,1]<0.5)
      {
        a1=-0.7+1.4*x[l,2]
        a2=0
      }else{
        a1=0.9-1.8*x[l,2]
        a2=0
      }
      spec=ARspec(cbind(a1*ar1,a2*ar2),sig,freq_hat)
      spec_true[,l]=log(Re(spec[1,1,]))
      
    }
    
  }else if(sim=="Adjusted-AdaptSPEC-X")
  {
    spec_true=matrix(0,(nfreq+1),L)
    ar1=diag(2)
    ar2=diag(2)
    sig=diag(2)
    freq_hat=(0:nfreq)/Nobs
    
    for(l in 1:L)
    {
      
      if((x[l,1]-0.75)^2+(x[l,2]-0.25)^2<=0.1875^2)
      {
        a1=1.5
        a2=-0.75
        
      }else if(x[l,1]>x[l,2])
      {
        
        a1=-1.5
        a2=-0.75
        
      }else if((x[l,1]-0.25)^2+(x[l,2]-0.75)^2<=0.125^2)
      {
        a1=-0.8
        a2=0
        
      }else{
        
        a1=0.2
        a2=0
      }
      
      spec=ARspec(cbind(a1*ar1,a2*ar2),sig,freq_hat)
      spec_true[,l]=log(Re(spec[1,1,]))
      
    }
  }else if(sim=="AR-Friedman")
  {
    
    spec_true=matrix(0,(nfreq+1),L)
    ar1=diag(2)
    ar2=diag(2)
    sig=diag(2)
    freq_hat=(0:nfreq)/Nobs
    
    
    for(l in 1:L)
    {
      a1=0.5*sin(pi*x[l,1]*x[l,2])-(x[l,3]-0.5)^2+0.35*sign(x[l,4]-0.5)-0.15*x[l,5]
      a2=0
      spec=ARspec(cbind(a1*ar1,a2*ar2),sig,freq_hat)
      spec_true[,l]=log(Re(spec[1,1,]))
      
    }
    
  }
  
  return(spec_true)
  
}



