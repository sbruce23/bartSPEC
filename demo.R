#################################################################
#####  Please install the packages if they are not available 
#############################################################
install.packages(c("pracma","mvtnorm","MASS","NPflow","plotly","invgamma","pillar","gtools","ggplot2","ggpubr","viridis"))
install.packages("programs/BayesSumOfTreesSPEC_1.0.tar.gz", repos=NULL, type="source")
install.packages("programs/optimizationarmadillo_1.0.tar.gz", repos=NULL,type="source")
install.packages("programs/Treecpp12262020_1.0.tar.gz", repos=NULL, type="source")




rm(list = ls())

library(pracma)
library(mvtnorm)
library(MASS)
library(NPflow)
library(plotly)
library(invgamma)
library(pillar)
library(gtools)
library(ggplot2)
library(BayesSumOfTreesSPEC)
library(optimizationarmadillo)
library(Treecpp12262020)

source("programs/Simulated_data.r")
source("programs/True_Spectrum.r")
source("programs/Bottom_nodes.r")
############################ 
###  Tree parameter
############################
Ntree=5
opt=list(nloop=10000,nwarmup=5000,nbasis=7,sigmasqalpha=100)

### hyperparameters for half-t distribution
nus=2  # nu_tau
Gs=10  #  A_tau
param_random=0.2  # parameters control the range of proposed uniform distribution for tau and g

  
## DART
theta=1  # sigma
darton=0  # if 1: apply Dirichlet prior; if 0: do not apply Dirichlet prior


#######################################
###  Abrupt+Smooth Simulation Data
#######################################
seed=1001
L=100
Nobs=250
sim="Abrupt+Smooth"
NumX=2
simdata=simulate(sim,L,Nobs,NumX,seed)
attach(simdata)



############################ 
###  BartSPEC model
############################
set.seed(1208)
start.time= Sys.time()
aa=BayesSumOfTreesSPEC(x_t,x,Nobs,L,Ntree,NumX,VarType,numcut,nus,Gs,param_random,theta,darton,opt)
end.time=Sys.time()
end.time-start.time



############################ 
###  True log spectrum
############################

spec_true=spectrue(sim,Nobs,L)


##################################
###  Convergence diagnostic plots
##################################

aaindex=matrix(list(),opt$nloop,Ntree)
for(p in 1:opt$nloop)
{
  tree=aa$tree_store[p,]
  
  for(i in 1:Ntree)
  {
    
    
    ## Number of bottom nodes
    aaindex[[p,i]]=BotNodes_without_condition(tree,i,1)$BotIdx
    
  }
}

nfreq=floor(Nobs/2)
len=c()
for(p in 1:opt$nloop)
{
  
  len[p]=length(unlist(aaindex[p,]))
  
}


par(mar=c(5,5,5,5),mai=c(1,1,1,1))


### mean of squared residuals 
res=rep(0,opt$nloop)
for(p in 1:opt$nloop)
{
  for(i in 1:L)
  {
    res[p]=res[p]+mean((aa$Y[2:(nfreq+1),i]-aa$fhat_log[p,2:(nfreq+1),i])^2)
  }
  
  res[p]=res[p]/L
  
}
data=data.frame(x=1:opt$nloop,y=res)
ggplot(data=data,aes(x,y)) +  geom_point()+theme_minimal()+ylab("Average Mean of Squared Residuals\n")+xlab("\nIteration")+
  theme(axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),axis.text = element_text(size = 18))


### average of estimated log power spectrum
ave_spectrum=apply(aa$fhat_log[,1:(nfreq+1),], 1,mean)
data=data.frame(x=1:opt$nloop,y=ave_spectrum)
ggplot(data=data,aes(x,y)) +  geom_point()+theme_minimal()+ylab("Average Estimated Log Power Spectrum\n")+xlab("\nIteration")+
  theme(plot.margin=margin(r=8),axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),axis.text = element_text(size = 18))


## number of bottom nodes
bottom=data.frame(x=1:opt$nloop,y=len)
ggplot(data=bottom,aes(x,y))+geom_line(data=bottom,color="black")+xlab("\nIteration")+ylab("Number of Bottom Nodes")+theme_minimal()+ylim(5,30)+
  theme(axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),legend.text = element_text(size = 18), axis.text = element_text(size = 18))

## number of nodes
node=as.data.frame(aa$Num_node)
node$ID=1:opt$nloop
node_melt=reshape2::melt(node,id.var="ID")
ggplot(data=node_melt,aes(x=ID,y=value,color=variable))+geom_line(size=1)+xlab("\nIteration")+ylim(0,25)+labs(color = "Tree")+ylab("Number of Nodes")+scale_color_discrete(labels = c("1", "2", "3","4","5"))+theme_minimal()+
  theme(axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),legend.position = c(0.6, 0.9),legend.direction = "horizontal", legend.text = element_text(size = 18), axis.text = element_text(size = 18))


##################################
###  Plots of Estimation 
##################################
nfreq=floor(Nobs/2)

#### plot
for(j in 1:L)
{
  
    datatrue=data.frame(x=1:(nfreq+1),y=spec_true[,j])
    dataest=data.frame(x=1:(nfreq+1),y=aa$spec_est[1:(nfreq+1),j])
    dataperio=data.frame(x=1:nfreq,y=aa$Y[2:(nfreq+1),j])

    print(ggplot(data=datatrue,aes(x,y)) +  geom_point(data=dataperio,color="gray")+theme_minimal()+
         theme(plot.title = element_text(hjust = 0.5,size=15),axis.title.x = element_text(size = 18),axis.title.y = element_blank(),axis.text = element_text(size = 15))+
         geom_line(data=dataest,color="blue",size=1)+geom_line(color="red",size=1)+scale_x_continuous(breaks=seq(0,nfreq,by=nfreq/5),labels=seq(0, 0.5, by=0.1)) +
         xlab("Frequency")+ggtitle(paste("Estimated vs true of log power spectrum for ",j,"th obs",sep = "")))
    
}


#######################################
###  AR-Friedman Simulation Data
#######################################
seed=1001
L=100
Nobs=250
sim="AR-Friedman"
NumX=5
simdata=simulate(sim,L,Nobs,NumX,seed)
attach(simdata)



############################ 
###  BartSPEC model
############################
set.seed(1208)
start.time= Sys.time()
aa=BayesSumOfTreesSPEC(x_t,x,Nobs,L,Ntree,NumX,VarType,numcut,nus,Gs,param_random,theta,darton,opt)
end.time=Sys.time()
end.time-start.time



############################ 
###  True log spectrum
############################

spec_true=spectrue(sim,Nobs,L)


##################################
###  Convergence diagnostic plots
##################################
aaindex=matrix(list(),opt$nloop,Ntree)
for(p in 1:opt$nloop)
{
  tree=aa$tree_store[p,]
  
  for(i in 1:Ntree)
  {
    
    
    ## Number of bottom nodes
    aaindex[[p,i]]=BotNodes_without_condition(tree,i,1)$BotIdx
    
  }
}

nfreq=floor(Nobs/2)
len=c()
for(p in 1:opt$nloop)
{
  
  len[p]=length(unlist(aaindex[p,]))
  
}


par(mar=c(5,5,5,5),mai=c(1,1,1,1))


### mean of squared residuals 
res=rep(0,opt$nloop)
for(p in 1:opt$nloop)
{
  for(i in 1:L)
  {
    res[p]=res[p]+mean((aa$Y[2:(nfreq+1),i]-aa$fhat_log[p,2:(nfreq+1),i])^2)
  }
  
  res[p]=res[p]/L
  
}
data=data.frame(x=1:opt$nloop,y=res)
ggplot(data=data,aes(x,y)) +  geom_point()+theme_minimal()+ylab("Average Mean of Squared Residuals\n")+xlab("\nIteration")+
  theme(axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),axis.text = element_text(size = 18))


### average of estimated log power spectrum
ave_spectrum=apply(aa$fhat_log[,1:(nfreq+1),], 1,mean)
data=data.frame(x=1:opt$nloop,y=ave_spectrum)
ggplot(data=data,aes(x,y)) +  geom_point()+theme_minimal()+ylab("Average Estimated Log Power Spectrum\n")+xlab("\nIteration")+
  theme(plot.margin=margin(r=8),axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),axis.text = element_text(size = 18))


## number of bottom nodes
bottom=data.frame(x=1:opt$nloop,y=len)
ggplot(data=bottom,aes(x,y))+geom_line(data=bottom,color="black")+xlab("\nIteration")+ylab("Number of Bottom Nodes")+theme_minimal()+ylim(5,25)+
  theme(axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),legend.text = element_text(size = 18), axis.text = element_text(size = 18))

## number of nodes
node=as.data.frame(aa$Num_node)
node$ID=1:opt$nloop
node_melt=reshape2::melt(node,id.var="ID")
ggplot(data=node_melt,aes(x=ID,y=value,color=variable))+geom_line(size=1)+xlab("\nIteration")+ylim(0,25)+labs(color = "Tree")+ylab("Number of Nodes")+scale_color_discrete(labels = c("1", "2", "3","4","5"))+theme_minimal()+
  theme(axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),legend.position = c(0.6, 0.9),legend.direction = "horizontal", legend.text = element_text(size = 18), axis.text = element_text(size = 18))


##################################
###  Plots of Estimation 
##################################
nfreq=floor(Nobs/2)

#### plot
for(j in 1:L)
{
  
  datatrue=data.frame(x=1:(nfreq+1),y=spec_true[,j])
  dataest=data.frame(x=1:(nfreq+1),y=aa$spec_est[1:(nfreq+1),j])
  dataperio=data.frame(x=1:nfreq,y=aa$Y[2:(nfreq+1),j])
  
  print(ggplot(data=datatrue,aes(x,y)) +  geom_point(data=dataperio,color="gray")+theme_minimal()+
          theme(plot.title = element_text(hjust = 0.5,size=15),axis.title.x = element_text(size = 18),axis.title.y = element_blank(),axis.text = element_text(size = 15))+
          geom_line(data=dataest,color="blue",size=1)+geom_line(color="red",size=1)+scale_x_continuous(breaks=seq(0,nfreq,by=nfreq/5),labels=seq(0, 0.5, by=0.1)) +
          xlab("Frequency")+ggtitle(paste("Estimated vs true of log power spectrum for ",j,"th obs",sep = "")))
  
}



###########################################
###  Adjusted-AdaptSPEC-X Simulation Data
###########################################
seed=1001
L=100
Nobs=250
sim="Adjusted-AdaptSPEC-X"
NumX=2
simdata=simulate(sim,L,Nobs,NumX,seed)
attach(simdata)



############################ 
###  BartSPEC model
############################
set.seed(1208)
start.time= Sys.time()
aa=BayesSumOfTreesSPEC(x_t,x,Nobs,L,Ntree,NumX,VarType,numcut,nus,Gs,param_random,theta,darton,opt)
end.time=Sys.time()
end.time-start.time



############################ 
###  True log spectrum
############################

spec_true=spectrue(sim,Nobs,L)


##################################
###  Convergence diagnostic plots
##################################
aaindex=matrix(list(),opt$nloop,Ntree)
for(p in 1:opt$nloop)
{
  tree=aa$tree_store[p,]
  
  for(i in 1:Ntree)
  {
    
    
    ## Number of bottom nodes
    aaindex[[p,i]]=BotNodes_without_condition(tree,i,1)$BotIdx
    
  }
}

nfreq=floor(Nobs/2)
len=c()
for(p in 1:opt$nloop)
{
  
  len[p]=length(unlist(aaindex[p,]))
  
}


par(mar=c(5,5,5,5),mai=c(1,1,1,1))


### mean of squared residuals 
res=rep(0,opt$nloop)
for(p in 1:opt$nloop)
{
  for(i in 1:L)
  {
    res[p]=res[p]+mean((aa$Y[2:(nfreq+1),i]-aa$fhat_log[p,2:(nfreq+1),i])^2)
  }
  
  res[p]=res[p]/L
  
}
data=data.frame(x=1:opt$nloop,y=res)
ggplot(data=data,aes(x,y)) +  geom_point()+theme_minimal()+ylab("Average Mean of Squared Residuals\n")+xlab("\nIteration")+
  theme(axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),axis.text = element_text(size = 18))


### average of estimated log power spectrum
ave_spectrum=apply(aa$fhat_log[,1:(nfreq+1),], 1,mean)
data=data.frame(x=1:opt$nloop,y=ave_spectrum)
ggplot(data=data,aes(x,y)) +  geom_point()+theme_minimal()+ylab("Average Estimated Log Power Spectrum\n")+xlab("\nIteration")+
  theme(plot.margin=margin(r=8),axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),axis.text = element_text(size = 18))


## number of bottom nodes
bottom=data.frame(x=1:opt$nloop,y=len)
ggplot(data=bottom,aes(x,y))+geom_line(data=bottom,color="black")+xlab("\nIteration")+ylab("Number of Bottom Nodes")+theme_minimal()+ylim(5,100)+
  theme(axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),legend.text = element_text(size = 18), axis.text = element_text(size = 18))

## number of nodes
node=as.data.frame(aa$Num_node)
node$ID=1:opt$nloop
node_melt=reshape2::melt(node,id.var="ID")
ggplot(data=node_melt,aes(x=ID,y=value,color=variable))+geom_line(size=1)+xlab("\nIteration")+ylim(0,25)+labs(color = "Tree")+ylab("Number of Nodes")+scale_color_discrete(labels = c("1", "2", "3","4","5"))+theme_minimal()+
  theme(axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),legend.position = c(0.6, 0.9),legend.direction = "horizontal", legend.text = element_text(size = 18), axis.text = element_text(size = 18))


##################################
###  Plots of Estimation 
##################################
nfreq=floor(Nobs/2)

#### plot
for(j in 1:L)
{
  
  datatrue=data.frame(x=1:(nfreq+1),y=spec_true[,j])
  dataest=data.frame(x=1:(nfreq+1),y=aa$spec_est[1:(nfreq+1),j])
  dataperio=data.frame(x=1:nfreq,y=aa$Y[2:(nfreq+1),j])
  
  print(ggplot(data=datatrue,aes(x,y)) +  geom_point(data=dataperio,color="gray")+theme_minimal()+
          theme(plot.title = element_text(hjust = 0.5,size=15),axis.title.x = element_text(size = 18),axis.title.y = element_blank(),axis.text = element_text(size = 15))+
          geom_line(data=dataest,color="blue",size=1)+geom_line(color="red",size=1)+scale_x_continuous(breaks=seq(0,nfreq,by=nfreq/5),labels=seq(0, 0.5, by=0.1)) +
          xlab("Frequency")+ggtitle(paste("Estimated vs true of log power spectrum for ",j,"th obs",sep = "")))
  
}



