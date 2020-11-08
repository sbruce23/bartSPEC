library(pracma)
library(trust)
library(mvtnorm)
library(MASS)
library(NPflow)
library(plotly)
library(invgamma)
library(forecast)
library(doParallel)
library(foreach)
library(ggplot2)
library(MCMCpack)
##### AdaptSPEC X paper
set.seed(1010)
L=200
Nobs=256
NumX=10
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
    index[l,2]=2
    
  }else{
    
    ts.sim[l,] <- arima.sim(model = list(order = c(2, 0, 0), ar = c(0.2, 0)),  n=Nobs)
    ts.sim[l,]=ts.sim[l,]+1
    index[l,2]=4
    
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
ts.plot(x_t[,5])

## scatter plot of data
x_data=cbind(x[,1:2],factor(index[,2]))
colnames(x_data)=c("X1","X2","Z")
ggplot(x_data,aes(X1,X2,color=Z)) + geom_point() 

###### Friedman
set.seed(1001)
L=200
Nobs=500
NumX=10
x=matrix(0,L,NumX)
x[,1:5]=matrix(runif(L*5),L,5)  # u
x[,6:NumX]=matrix(rnorm(L*(NumX-6+1)),L,(NumX-6+1))
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
ts.plot(x_t[,1])

#### phi value
a1=c()
for(l in 1:L)
{
  a1[l]=0.5*sin(pi*x[l,1]*x[l,2])-(x[l,3]-0.5)^2+0.35*sign(x[l,4]-0.5)-0.15*x[l,5]
}
range(a1)

##### abrupt_slowly_ordered
set.seed(1010)
L=100
Nobs=500
NumX=10
x=matrix(0,L,NumX) 
for(l in 1:L)
{
  x[l,1]=(l-1)/(L-1)
  x[l,2]=(l-1)/(L-1)
}

x[,3:NumX]=matrix(runif(L*(NumX-2)),L,NumX-2)
#x=matrix(runif(L*NumX),L,NumX)  # u
ts.sim=matrix(0,L,Nobs)
x=data.frame(x)

for(l in 1:L)
{
  if(x[l,1]<0.5)
  {
    ts.sim[l,]=arima.sim(list(order=c(1,0,0),ar=-0.7+1.4*x[l,2]),n=Nobs) 
  }else{
    ts.sim[l,]=arima.sim(list(order=c(1,0,0),ar=-0.9+1.8*x[l,2]),n=Nobs) 
    
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
ts.plot(x_t[,2])



##### abrupt_slowly_unordered
set.seed(1010)
L=100
Nobs=500
NumX=10
# x=matrix(0,L,NumX) 
x[,1]=seq(1,L)/L
x[,2]=seq(L,1)/L
x[,3:NumX]=matrix(runif(L*(NumX-2)),L,NumX-2)
x=matrix(runif(L*NumX),L,NumX)  # u
ts.sim=matrix(0,L,Nobs)
x=data.frame(x)

for(l in 1:L)
{
  if(x[l,1]<0.5)
  {
    ts.sim[l,]=arima.sim(list(order=c(1,0,0),ar=-0.7+1.4*x[l,2]),n=Nobs) 
  }else{
    ts.sim[l,]=arima.sim(list(order=c(1,0,0),ar=-0.9+1.8*x[l,2]),n=Nobs) 
    
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
ts.plot(x_t[,2])



## initial
Ntree=5
opt=list(nloop=10000,nwarmup=5000,nbasis=7,
         sigmasqalpha=100,
         plotting=TRUE)


VarType=c(rep("ORD",1000),"CAT")
numcut=c(rep(100,1000),4)
nus=2
Gs=10
param_random=0.2

 
## DART
theta=NumX
rho=NumX
a=0.5
b=1
darton=0
### BART
set.seed(1001)
aa=Bart(x_t,x,Nobs,L,Ntree,NumX,VarType,numcut,nus,Gs,param_random,theta,rho,a,b,darton,opt)


#### True log spectrum
nfreq <- floor(Nobs/2)

library(pillar)
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


########### True log spectrum slowly varying
library(pillar)
spec_true=matrix(0,(nfreq+1),L)
ar1=diag(2)
ar2=diag(2)
sig=diag(2)
freq_hat=(0:nfreq)/Nobs


for(l in 1:L)
{
  #a1=0.25*sin(pi*x[l,1]*x[l,2])+(x[l,3]-0.5)^2+0.25*x[l,4]+0.25*x[l,5]
  #a1=0.5*sin(pi*x[l,1]*x[l,2])-1.5*(x[l,3]-0.5)^2+0.5*x[l,4]-0.25*x[l,5]
  #a1=sin(pi*x[l,1]*x[l,2])-(x[l,3]-0.5)^2+0.25*x[l,4]-0.5*x[l,5]
  a1=0.5*sin(pi*x[l,1]*x[l,2])-(x[l,3]-0.5)^2+0.35*sign(x[l,4]-0.5)-0.15*x[l,5]
  a2=0
  spec=ARspec(cbind(a1*ar1,a2*ar2),sig,freq_hat)
  spec_true[,l]=log(Re(spec[1,1,]))
  
}


#### plot
for(j in 1:L)
{
  plot(aa$Y[2:(nfreq+1),j],main=paste("fitted vs periodogram of ",j,"st Obs of average",sep = ""),
       xlab = "Timepoints",ylab="Y")
  lines(aa$spec_est[1:(nfreq+1),j],col='red')
  lines(spec_true[,j],col='blue')
}

### MSE
mse=mean(apply((spec_true[1:(nfreq+1),]-aa$spec_est[1:(nfreq+1),])^2,2,mean))
  
#### accept ratio
###### acceptance ratio
sum=0
sum_change=0
for (i in opt$nwarmup:opt$nloop)
{
  
  for(j in 1:Ntree)
  {
    
    sum=sum+ifelse(aa$result[[i,j]]$done==1,1,0)
    # sum=sum+ifelse(aa$result[[i,j]]$done==1 & aa$result[[i,j]]$step=="Change",1,0)
    # sum_change=sum_change+ifelse(aa$result[[i,j]]$step=="Change",1,0)
    
    
  }
  
}

ratio=sum/((opt$nloop-opt$nwarmup)*Ntree)
ratio

change_ratio=sum/sum_change
change_ratio


###### inclusion proportions
var_list=matrix(list(),opt$nloop,1)
var_prop=matrix(0,opt$nloop,NumX)
for(p in opt$nwarmup:opt$nloop)
{
  for(i in 1:Ntree)
  {
    if(!is.null(aa$ttrree_list[[p,i]]$varidx))
    {
      var_list[[p,1]]=append(var_list[[p,1]],aa$ttrree_list[[p,i]]$varidx)
      
    }
  }
  
  temp=as.vector(table(var_list[[p,1]])/length(var_list[[p,1]]))
  var_prop[p,as.numeric(names(table(var_list[[p,1]])))]=temp
  
}


prop=data.frame("idx"=seq(1,NumX),"proportion"=apply(var_prop[opt$nwarmup:opt$nloop,],2,mean))
sort_prop=prop[with(prop,order(proportion,decreasing = T)),]

barplot(sort_prop$proportion[1:10],names.arg=sort_prop$idx[1:10],xlab="Index of Covariate",ylab="Frequency")


############ diagnostic plots
##### Number of Nodes for each iteration
plot(aa$Num_node[,1],xlab="Iteration", ylab="Number of nodes",ylim=c(0,150),type = "l",col="red")
lines(aa$Num_node[,2],type = "l",main = "Number of Nodes for each iteration",col="yellow")
lines(aa$Num_node[,3],type = "l",main = "Number of Nodes for each iteration",col="blue")
lines(aa$Num_node[,4],type = "l",main = "Number of Nodes for each iteration",col="green")
lines(aa$Num_node[,5],type = "l",main = "Number of Nodes for each iteration",col="black")
#legend("topright", legend=c("Tree1", "Tree2","Tree3","Tree4","Tree5"),
#       col=c("red", "yellow","blue","green","black"), cex=0.7,lty=c(1,1,1,1,1),y.intersp = 0.3)

##### plot ave tau, g, bottom nodes
tausum_ite=c()
betasum_ite=c()
gsum_ite=c()
tau_ave=c()
beta_ave=c()
g_ave=c()
len=c()
for (i in 1:opt$nloop)
{
  
  tausum_ite[i]=sum(aa$tau_sum[i,])
  gsum_ite[i]=sum(aa$g_sum[i,])
  betasum_ite[i]=sum(aa$beta_sum[i,,])
  len[i]=length(unlist(aa$aaindex[i,]))
  tau_ave[i]=tausum_ite[i]/length(unlist(aa$aaindex[i,]))
  g_ave[i]=gsum_ite[i]/length(unlist(aa$aaindex[i,]))
  beta_ave[i]=betasum_ite[i]/(length(unlist(aa$aaindex[i,]))*(opt$nbasis+1))
  
}


plot(log(tau_ave),xlab="Iteration",ylab="Log of ave tau square")
plot(log(g_ave),xlab="Iteration",ylab="Log of ave g")
plot(beta_ave, xlab="Iteration",ylab="Ave beta")
plot(len,type="l",xlab="Iteration",ylab="Number of bottom nodes")

plot.ts(aa$s)

