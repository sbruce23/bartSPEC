#####################################################################
# Description: This file includes the function to generate the three 
#              simulated data in the paper
##################################################################

simulate=function(sim,L,Nobs,NumX,seed)
{
  if(sim=="Abrupt+Smooth")
  {
    set.seed(seed)
    
    x=matrix(runif(L*NumX),L,NumX)  # u
    ts.sim=matrix(0,L,Nobs)
    x=data.frame(x)
    
    
    for(l in 1:L)
    {
      if(x[l,1]<0.5)
      {
        ts.sim[l,]=arima.sim(list(order=c(1,0,0),ar=-0.7+1.4*x[l,2]),n=Nobs) 
      }else{
        ts.sim[l,]=arima.sim(list(order=c(1,0,0),ar=0.9-1.8*x[l,2]),n=Nobs) 
        
      }
      
    }
    
    x_t=t(ts.sim)
    
    # standardized
    for (i in 1:L)
    {
      xmat=cbind(matrix(1,dim(x_t)[1],1), matrix(seq(1,dim(x_t)[1],1),dim(x_t)[1],1))
      linfit=solve(t(xmat)%*%xmat)%*%t(xmat)%*%x_t[,i]
      x_t[,i]=x_t[,i]-xmat%*%linfit
    }
    
    
    VarType=rep("ORD",NumX)
    numcut=rep(100,NumX)
    
  }else if(sim=="Adjusted-AdaptSPEC-X")
  {
    
    set.seed(seed)
    
    x=matrix(runif(L*NumX),L,NumX)  # u
    ts.sim=matrix(0,L,Nobs)
    index=matrix(0,L,2)
    index[,1]=seq(1,L)
    x=data.frame(x)
    
    for(l in 1:L)
    {
      if((x[l,1]-0.75)^2+(x[l,2]-0.25)^2<=0.1875^2)
      {
        
        ts.sim[l,] <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -.75)), n=Nobs)
        index[l,2]=3
        
      }else if(x[l,1]>x[l,2])
      {
        
        ts.sim[l,] <- arima.sim(model = list(order = c(2, 0, 0), ar = c(-1.5, -.75)),  n=Nobs)
        ts.sim[l,]=ts.sim[l,]-1.5
        index[l,2]=1
        
      }else if((x[l,1]-0.25)^2+(x[l,2]-0.75)^2<=0.125^2)
      {
        
        ts.sim[l,] <- arima.sim(model = list(order = c(2, 0, 0), ar = c(-0.8, 0)), n=Nobs)
        ts.sim[l,]=ts.sim[l,]+1
        index[l,2]=4
        
      }else{
        
        ts.sim[l,] <- arima.sim(model = list(order = c(2, 0, 0), ar = c(0.2, 0)),  n=Nobs)
        ts.sim[l,]=ts.sim[l,]+1
        index[l,2]=2
        
      }
      
    }
    
    x_t=t(ts.sim)
    
    # standardized
    for (i in 1:L)
    {
      xmat=cbind(matrix(1,dim(x_t)[1],1), matrix(seq(1,dim(x_t)[1],1),dim(x_t)[1],1))
      linfit=solve(t(xmat)%*%xmat)%*%t(xmat)%*%x_t[,i]
      x_t[,i]=x_t[,i]-xmat%*%linfit
    }

    
    VarType=rep("ORD",NumX)
    numcut=rep(100,NumX)
    
  }else if(sim=="AR-Friedman")
  {
    set.seed(seed)
    
    x=matrix(0,L,NumX)
    x[,1:5]=matrix(runif(L*5),L,5)  # u
    
    ts.sim=matrix(0,L,Nobs)
    
    x=data.frame(x)
    
    for(l in 1:L)
    {
      
      ts.sim[l,]=arima.sim(list(order=c(1,0,0),ar= 0.5*sin(pi*x[l,1]*x[l,2])-(x[l,3]-0.5)^2+0.35*sign(x[l,4]-0.5)-0.15*x[l,5]),n=Nobs) # ar=2*x1
      
    }
    
    x_t=t(ts.sim)
    
    # standardized
    for (i in 1:L)
    {
      xmat=cbind(matrix(1,dim(x_t)[1],1), matrix(seq(1,dim(x_t)[1],1),dim(x_t)[1],1))
      linfit=solve(t(xmat)%*%xmat)%*%t(xmat)%*%x_t[,i]
      x_t[,i]=x_t[,i]-xmat%*%linfit
    }

    
    
    VarType=rep("ORD",NumX)
    numcut=rep(100,NumX)
    
    
  }
  
  list(x=x,x_t=x_t,VarType=VarType,numcut=numcut)
  
  
}





