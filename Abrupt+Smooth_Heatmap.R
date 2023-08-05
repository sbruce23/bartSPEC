#####################################################################
# Description: This file generate the heatmap of the conditional 
#              estimation for the Abrupt+Smooth simulation as 
#              described in the paper
#
# Instruction: Please install all the packages in the demo.R file
##################################################################


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


library(ggpubr)
library(viridis)

library(BayesSumOfTreesSPEC)
library(optimizationarmadillo)
library(Treecpp12262020)

source("programs/Simulated_data.r")
source("programs/True_Spectrum.r")

############################ 
###  Tree parameter
############################
Ntree=5
opt=list(nloop=10000,nwarmup=5000,nbasis=7,sigmasqalpha=100)

### hyperparameters for half-t distribution
nus=2
Gs=10
param_random=0.2


## DART
theta=1
darton=0

############################ 
###  Simulation Data
############################
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
aa=BayesSumOfTreesSPEC(x_t,x,Nobs,L,Ntree,NumX,VarType,numcut,nus,Gs,param_random,theta,darton,opt)



##########################
### test var 2
###########################

splitnum=50  # number of unique values for estimations
var_test=2   # index of the variable to be tested

max=max(x[,var_test])
min=min(x[,var_test])
xinc=(max-min)/(splitnum+1)
numcut[var_test]=numcut[var_test]
xinc_cut=(max-min)/(numcut[var_test]+1)

## set possible cut point for the test variable
rule_vartest=seq(min+xinc_cut,min+numcut[var_test]*xinc_cut,by=xinc_cut)


## set several different values for the test variable 
partial_value=seq(0,1,1/(splitnum-1))

spec_est_partial=array(0,c(splitnum,Nobs,L))

for(qq in 1:splitnum)
{
  
  ### set test value
  xx=x
  xx[,var_test]=partial_value[qq]
  
  ### initialize
  nseg_time_temp=Nobs
  Ntime=Nobs
  NumObs=L
  nwarmup=opt$nwarmup
  nloop=opt$nloop
  
  mtotalfit=matrix(0,nseg_time_temp,NumObs)
  mtrainFits=array(0,c(Ntree,nseg_time_temp,NumObs))
  fhat_log=array(0,c(nloop,nseg_time_temp,NumObs))
  
  ### rule
  RuleNum=numcut
  
  Rulemat=list()
  for (i in 1:NumX)
  {

      if(VarType[i]=="CAT")
      {
        
        Rulemat[[i]]=levels(as.factor(x[,i]))
        
      }else{
        
        max=max(x[,i])
        min=min(x[,i])
        xinc=(max-min)/(RuleNum[i]+1)
        Rulemat[[i]]=seq(min+xinc,min+RuleNum[i]*xinc,by=xinc)
        
      }
    
  }
  
  
  
  ### pass to the tree model
  for (p in nwarmup:nloop)
  {
    ## set pth loop tree
    tree=aa$tree_store[p,]
    
    for(i in 1:Ntree)
    {
      cat("ite qq is",qq,"\n")
      cat("ite p is",p,"\n")
      cat("tree is",i,"\n")
      
      
      for (j in 1:NumObs)
      {
        
        idx=FindNodeC(j,1,i,tree,xx,VarType,RuleNum,Rulemat) # from the top node of the ith tree, find the bottom idx for jth obs
        
        # method1
        if(nseg_time_temp%%2!=0)
        {
          mfit=c(tree[[i]][[paste("node",idx,sep = "")]]$fhat,tree[[i]][[paste("node",idx,sep = "")]]$fhat[((Ntime/2)+1):2])
          
        }else{
          
          mfit=c(tree[[i]][[paste("node",idx,sep = "")]]$fhat,tree[[i]][[paste("node",idx,sep = "")]]$fhat[(Ntime/2):2])
          
        }
        
        mtotalfit[,j] = mtotalfit[,j] + mfit-mtrainFits[i,,j]
        mtrainFits[i,,j]=mfit
        
      }
      
      
      # method1
      apply_sum_vecC(mtrainFits,c(Ntree,Ntime,NumObs),fhat_log,nloop,p)
      
    }
  }
  
  
  
  nfreq <- floor(nseg_time_temp/2)
  spec_est=matrix(0,nseg_time_temp,NumObs)
  
  for(p in nwarmup:nloop)
  {
    spec_est=spec_est+fhat_log[p,,]/(nloop-nwarmup+1)
  }
  
  
  spec_est_partial[qq,,]=spec_est
  
}



##  index of observations with x1<0.5
smallvar=which(x[,1]<0.5)

## store the estimation of power spectrum for x1<0.5
spec_var1_small=apply(spec_est_partial[,,smallvar],c(1,2),mean)
xindex=partial_value
yindex=seq(0,0.5,0.5/(Nobs/2))
data1 <- expand.grid(X=xindex, Y=yindex)
data1$Z=as.numeric(spec_var1_small[,1:(nfreq+1)])


## store the estimation of power spectrum for x1>=0.5
spec_var1_large=apply(spec_est_partial[,,-smallvar],c(1,2),mean)
xindex=partial_value
yindex=seq(0,0.5,0.5/(Nobs/2))
data2 <- expand.grid(X=xindex, Y=yindex)
data2$Z=as.numeric(spec_var1_large[,1:(nfreq+1)])










############################
### test var 1
##############################


################# run for x2==0
case="small" 

##### set test variable
splitnum=50
var_test=1


## set possible cut point for the test variable
max=max(x[,var_test])
min=min(x[,var_test])
xinc_cut=(max-min)/(numcut[var_test]+1)
rule_vartest1=seq(min+xinc_cut,min+numcut[var_test]*xinc_cut,by=xinc_cut)


## set several different values for the test variable 
partial_value=seq(0,1,1/(splitnum-1))



spec_est_partial=array(0,c(splitnum,Nobs,L))

for(qq in 1:splitnum)
{

  ### set test value
  xx=x
  xx[,var_test]=partial_value[qq]
  
  if(case=="small")
  {
    xx[,2]=0
  }else if(case=="large")
  {
    xx[,2]=1
  }

  
  ### initialize
  nseg_time_temp=Nobs
  Ntime=Nobs
  NumObs=L
  nwarmup=opt$nwarmup
  nloop=opt$nloop
  
  mtotalfit=matrix(0,nseg_time_temp,NumObs)
  mtrainFits=array(0,c(Ntree,nseg_time_temp,NumObs))
  fhat_log=array(0,c(nloop,nseg_time_temp,NumObs))
  
  ### rule
  RuleNum=numcut
  
  Rulemat=list()
  for (i in 1:NumX)
  {
    
    if(VarType[i]=="CAT")
    {
      
      Rulemat[[i]]=levels(as.factor(x[,i]))
      
    }else{
      
      max=max(x[,i])
      min=min(x[,i])
      xinc=(max-min)/(RuleNum[i]+1)
      Rulemat[[i]]=seq(min+xinc,min+RuleNum[i]*xinc,by=xinc)
      
    }
    
  }
  
  
  
  
  ### pass to the tree model
  for (p in nwarmup:nloop)
  {
    ## set pth loop tree
    tree=aa$tree_store[p,]
    
    for(i in 1:Ntree)
    {
      cat("ite qq is",qq,"\n")
      cat("ite p is",p,"\n")
      cat("tree is",i,"\n")
      
      
      for (j in 1:NumObs)
      {
        
        idx=FindNodeC(j,1,i,tree,xx,VarType,RuleNum,Rulemat) # from the top node of the ith tree, find the bottom idx for jth obs
        
        # method1
        if(nseg_time_temp%%2!=0)
        {
          mfit=c(tree[[i]][[paste("node",idx,sep = "")]]$fhat,tree[[i]][[paste("node",idx,sep = "")]]$fhat[((Ntime/2)+1):2])
          
        }else{
          
          mfit=c(tree[[i]][[paste("node",idx,sep = "")]]$fhat,tree[[i]][[paste("node",idx,sep = "")]]$fhat[(Ntime/2):2])
          
        }
        
        mtotalfit[,j] = mtotalfit[,j] + mfit-mtrainFits[i,,j]
        mtrainFits[i,,j]=mfit
        
      }
      
      
      # method1
      apply_sum_vecC(mtrainFits,c(Ntree,Ntime,NumObs),fhat_log,nloop,p)
      
    }
  }
  
  
  
  nfreq <- floor(nseg_time_temp/2)
  spec_est=matrix(0,nseg_time_temp,NumObs)
  
  for(p in nwarmup:nloop)
  {
    spec_est=spec_est+fhat_log[p,,]/(nloop-nwarmup+1)
  }
  
  
  spec_est_partial[qq,,]=spec_est
  
}


spec_var2_small=apply(spec_est_partial,c(1,2),mean)
xindex=partial_value
yindex=seq(0,0.5,0.5/(Nobs/2))
data3 <- expand.grid(X=xindex, Y=yindex)
data3$Z=as.numeric(spec_var2_small[,1:(nfreq+1)])



################# run for x2==1
case="large"


##### set test variable
splitnum=50
var_test=1


## set possible cut point for the test variable
max=max(x[,var_test])
min=min(x[,var_test])
xinc_cut=(max-min)/(numcut[var_test]+1)
rule_vartest1=seq(min+xinc_cut,min+numcut[var_test]*xinc_cut,by=xinc_cut)



## set several different values for the test variable 
partial_value=seq(0,1,1/(splitnum-1))



spec_est_partial=array(0,c(splitnum,Nobs,L))
for(qq in 1:splitnum)
{
  
  ### set test value
  xx=x
  xx[,var_test]=partial_value[qq]
  
  if(case=="small")
  {
    xx[,2]=0
  }else if(case=="large")
  {
    xx[,2]=1
  }
  
  
  ### initialize
  nseg_time_temp=Nobs
  Ntime=Nobs
  NumObs=L
  nwarmup=opt$nwarmup
  nloop=opt$nloop
  
  mtotalfit=matrix(0,nseg_time_temp,NumObs)
  mtrainFits=array(0,c(Ntree,nseg_time_temp,NumObs))
  fhat_log=array(0,c(nloop,nseg_time_temp,NumObs))
  
  ### rule
  RuleNum=numcut
  
  Rulemat=list()
  for (i in 1:NumX)
  {
    
    if(VarType[i]=="CAT")
    {
      
      Rulemat[[i]]=levels(as.factor(x[,i]))
      
    }else{
      
      max=max(x[,i])
      min=min(x[,i])
      xinc=(max-min)/(RuleNum[i]+1)
      Rulemat[[i]]=seq(min+xinc,min+RuleNum[i]*xinc,by=xinc)
      
    }
    
  }
  
  
  
  
  ### pass to the tree model
  for (p in nwarmup:nloop)
  {
    ## set pth loop tree
    tree=aa$tree_store[p,]
    
    for(i in 1:Ntree)
    {
      cat("ite qq is",qq,"\n")
      cat("ite p is",p,"\n")
      cat("tree is",i,"\n")
      
      
      for (j in 1:NumObs)
      {
        
        idx=FindNodeC(j,1,i,tree,xx,VarType,RuleNum,Rulemat) # from the top node of the ith tree, find the bottom idx for jth obs
        
        # method1
        if(nseg_time_temp%%2!=0)
        {
          mfit=c(tree[[i]][[paste("node",idx,sep = "")]]$fhat,tree[[i]][[paste("node",idx,sep = "")]]$fhat[((Ntime/2)+1):2])
          
        }else{
          
          mfit=c(tree[[i]][[paste("node",idx,sep = "")]]$fhat,tree[[i]][[paste("node",idx,sep = "")]]$fhat[(Ntime/2):2])
          
        }
        
        mtotalfit[,j] = mtotalfit[,j] + mfit-mtrainFits[i,,j]
        mtrainFits[i,,j]=mfit
        
      }
      
      
      # method1
      apply_sum_vecC(mtrainFits,c(Ntree,Ntime,NumObs),fhat_log,nloop,p)
      
    }
  }
  
  
  
  nfreq <- floor(nseg_time_temp/2)
  spec_est=matrix(0,nseg_time_temp,NumObs)
  
  for(p in nwarmup:nloop)
  {
    spec_est=spec_est+fhat_log[p,,]/(nloop-nwarmup+1)
  }
  
  
  spec_est_partial[qq,,]=spec_est
  
}


spec_var2_large=apply(spec_est_partial,c(1,2),mean)
xindex=partial_value
yindex=seq(0,0.5,0.5/(Nobs/2))
data4 <- expand.grid(X=xindex, Y=yindex)
data4$Z=as.numeric(spec_var2_large[,1:(nfreq+1)])




########### True log spectrum slowly varying

condition="var1_small"
nfreq <- floor(Nobs/2)
test_num=100
var_value=seq(0,1,1/(test_num-1))
spec_true=matrix(0,test_num,(nfreq+1))
ar1=diag(2)
ar2=diag(2)
sig=diag(2)
freq_hat=(0:nfreq)/Nobs


for(l in 1:test_num)
{
  if(condition=="var1_small")
  {
    a1=-0.7+1.4*var_value[l]
  }else if(condition=="var1_large")
  {
    a1=0.9-1.8*var_value[l]
  }else if(condition=="var2_small")
  {
    if(var_value[l]<0.5)
    {
      a1=-0.7+1.4*0
    }else{
      a1=0.9-1.8*0
    }
  }else if(condition=="var2_large")
  {
    if(var_value[l]<0.5)
    {
      a1=-0.7+1.4*1
    }else{
      a1=0.9-1.8*1
    }
  }
  
  a2=0
  spec=ARspec(cbind(a1*ar1,a2*ar2),sig,freq_hat)
  spec_true[l,]=log(Re(spec[1,1,]))
  
}

xindex=var_value
yindex=seq(0,0.5,0.5/(Nobs/2))
data5 <- expand.grid(X=xindex, Y=yindex)
data5$Z=as.numeric(spec_true)




condition="var1_large"
nfreq <- floor(Nobs/2)
test_num=100
var_value=seq(0,1,1/(test_num-1))
spec_true=matrix(0,test_num,(nfreq+1))
ar1=diag(2)
ar2=diag(2)
sig=diag(2)
freq_hat=(0:nfreq)/Nobs


for(l in 1:test_num)
{
  if(condition=="var1_small")
  {
    a1=-0.7+1.4*var_value[l]
  }else if(condition=="var1_large")
  {
    a1=0.9-1.8*var_value[l]
  }else if(condition=="var2_small")
  {
    if(var_value[l]<0.5)
    {
      a1=-0.7+1.4*0
    }else{
      a1=0.9-1.8*0
    }
  }else if(condition=="var2_large")
  {
    if(var_value[l]<0.5)
    {
      a1=-0.7+1.4*1
    }else{
      a1=0.9-1.8*1
    }
  }
  
  a2=0
  spec=ARspec(cbind(a1*ar1,a2*ar2),sig,freq_hat)
  spec_true[l,]=log(Re(spec[1,1,]))
  
}


xindex=var_value
yindex=seq(0,0.5,0.5/(Nobs/2))
data6 <- expand.grid(X=xindex, Y=yindex)
data6$Z=as.numeric(spec_true)




condition="var2_small"
nfreq <- floor(Nobs/2)
test_num=100
var_value=seq(0,1,1/(test_num-1))
spec_true=matrix(0,test_num,(nfreq+1))
ar1=diag(2)
ar2=diag(2)
sig=diag(2)
freq_hat=(0:nfreq)/Nobs


for(l in 1:test_num)
{
  if(condition=="var1_small")
  {
    a1=-0.7+1.4*var_value[l]
  }else if(condition=="var1_large")
  {
    a1=0.9-1.8*var_value[l]
  }else if(condition=="var2_small")
  {
    if(var_value[l]<0.5)
    {
      a1=-0.7+1.4*0
    }else{
      a1=0.9-1.8*0
    }
  }else if(condition=="var2_large")
  {
    if(var_value[l]<0.5)
    {
      a1=-0.7+1.4*1
    }else{
      a1=0.9-1.8*1
    }
  }
  
  a2=0
  spec=ARspec(cbind(a1*ar1,a2*ar2),sig,freq_hat)
  spec_true[l,]=log(Re(spec[1,1,]))
  
}


xindex=var_value
yindex=seq(0,0.5,0.5/(Nobs/2))
data7 <- expand.grid(X=xindex, Y=yindex)
data7$Z=as.numeric(spec_true)



condition="var2_large"
nfreq <- floor(Nobs/2)
test_num=100
var_value=seq(0,1,1/(test_num-1))
spec_true=matrix(0,test_num,(nfreq+1))
ar1=diag(2)
ar2=diag(2)
sig=diag(2)
freq_hat=(0:nfreq)/Nobs


for(l in 1:test_num)
{
  if(condition=="var1_small")
  {
    a1=-0.7+1.4*var_value[l]
  }else if(condition=="var1_large")
  {
    a1=0.9-1.8*var_value[l]
  }else if(condition=="var2_small")
  {
    if(var_value[l]<0.5)
    {
      a1=-0.7+1.4*0
    }else{
      a1=0.9-1.8*0
    }
  }else if(condition=="var2_large")
  {
    if(var_value[l]<0.5)
    {
      a1=-0.7+1.4*1
    }else{
      a1=0.9-1.8*1
    }
  }
  
  a2=0
  spec=ARspec(cbind(a1*ar1,a2*ar2),sig,freq_hat)
  spec_true[l,]=log(Re(spec[1,1,]))
  
}


xindex=var_value
yindex=seq(0,0.5,0.5/(Nobs/2))
data8 <- expand.grid(X=xindex, Y=yindex)
data8$Z=as.numeric(spec_true)


## set the limit of power spectrum for the plot 
lowlimit=min(data1$Z,data2$Z,data3$Z,data4$Z,data5$Z,data6$Z,data7$Z,data8$Z)-0.1
highlimit=max(data1$Z,data2$Z,data3$Z,data4$Z,data5$Z,data6$Z,data7$Z,data8$Z)+0.1


### plot
est_var1_small=ggplot(data1, aes(X, Y, fill= Z)) +
  geom_tile()+xlab(expression(omega[2])) + ylab("Frequency")+scale_fill_viridis(limits=c(lowlimit, highlimit),discrete=FALSE)+theme_bw()+
  theme(legend.title = element_blank(),plot.margin = margin(1,5,15,1),axis.title.y = element_blank(), panel.border=element_blank(),legend.text = element_text(size = 15), axis.text = element_text(size = 15),plot.title = element_text(hjust = 0.5,size=16),axis.title.x = element_text(size = 20))+
  ggtitle(expression(atop('Estimated Log Power', paste('Spectrum ('~omega[1]<0.5~")"))))


est_var1_large=ggplot(data2, aes(X, Y, fill= Z)) +
  geom_tile()+xlab(expression(omega[2])) + ylab("Frequency")+scale_fill_viridis(limits=c(lowlimit, highlimit),discrete=FALSE)+theme_bw()+
  theme(plot.margin = margin(1,5,15,1),axis.title.y = element_blank(), panel.border=element_blank(),legend.text = element_text(size = 15), axis.text = element_text(size = 15),plot.title = element_text(hjust = 0.5,size=16),legend.title = element_text(size = 18),axis.title.x = element_text(size = 20))+
  ggtitle(expression(atop('Estimated Log Power', paste('Spectrum ('~omega[1]>=0.5~")"))))      


est_var2_small=ggplot(data3, aes(X, Y, fill= Z)) +
  geom_tile()+xlab(expression(omega[1])) + ylab("Frequency")+scale_fill_viridis(limits=c(lowlimit, highlimit),discrete=FALSE)+theme_bw()+
  theme(plot.margin = margin(1,5,15,1),axis.title.y = element_blank(), panel.border=element_blank(),legend.text = element_text(size = 15), axis.text = element_text(size = 15),plot.title = element_text(hjust = 0.5,size=16),legend.title = element_text(size = 18),axis.title.x = element_text(size = 20))+
  ggtitle(expression(atop('Estimated Log Power', paste('Spectrum ('~omega[2]==0~")"))))



est_var2_large=ggplot(data4, aes(X, Y, fill= Z)) +
  geom_tile()+xlab(expression(omega[1])) + ylab("Frequency")+scale_fill_viridis(limits=c(lowlimit, highlimit),discrete=FALSE)+theme_bw()+
  theme(plot.margin = margin(1,5,15,1),axis.title.y = element_blank(), panel.border=element_blank(),legend.text = element_text(size = 15), axis.text = element_text(size = 15),plot.title = element_text(hjust = 0.5,size=16),legend.title = element_text(size = 18),axis.title.x = element_text(size = 20))+
  ggtitle(expression(atop('Estimated Log Power', paste('Spectrum ('~omega[2]==1~")"))))



True_var1_small=ggplot(data5, aes(X, Y, fill= Z)) +
  geom_tile()+xlab(expression(omega[2])) + ylab("Frequency")+scale_fill_viridis(limits=c(lowlimit, highlimit),discrete=FALSE)+theme_bw()+
  theme(plot.margin = margin(15,5,1,1),axis.title.y = element_blank(), panel.border=element_blank(),legend.text = element_text(size = 15), axis.text = element_text(size = 15),plot.title = element_text(hjust = 0.5,size=16),legend.title = element_text(size = 18),axis.title.x = element_text(size = 20))+
  ggtitle(expression(atop('True Log Power', paste('Spectrum ('~omega[1]<0.5~")"))))


True_var1_large=ggplot(data6, aes(X, Y, fill= Z)) +
  geom_tile()+xlab(expression(omega[2])) + ylab("Frequency")+scale_fill_viridis(limits=c(lowlimit, highlimit),discrete=FALSE)+theme_bw()+
  theme(plot.margin = margin(15,5,1,1),axis.title.y = element_blank(), panel.border=element_blank(),legend.text = element_text(size = 15), axis.text = element_text(size = 15),plot.title = element_text(hjust = 0.5,size=16),legend.title = element_text(size = 18),axis.title.x = element_text(size = 20))+
  ggtitle(expression(atop('True Log Power', paste('Spectrum ('~omega[1]>=0.5~")"))))     



True_var2_small=ggplot(data7, aes(X, Y, fill= Z)) +
  geom_tile()+xlab(expression(omega[1])) + ylab("Frequency")+scale_fill_viridis(limits=c(lowlimit, highlimit),discrete=FALSE)+theme_bw()+
  theme(plot.margin = margin(15,5,1,1),axis.title.y = element_blank(), panel.border=element_blank(),legend.text = element_text(size = 15), axis.text = element_text(size = 15),plot.title = element_text(hjust = 0.5,size=16),legend.title = element_text(size = 18),axis.title.x = element_text(size = 20))+
  ggtitle(expression(atop('True Log Power', paste('Spectrum ('~omega[2]==0~")"))))


True_var2_large=ggplot(data8, aes(X, Y, fill= Z)) +
  geom_tile()+xlab(expression(omega[1])) +scale_fill_viridis(limits=c(lowlimit, highlimit),discrete=FALSE)+theme_bw()+
  theme(plot.margin = margin(15,5,1,1),axis.title.y = element_blank(), panel.border=element_blank(),legend.text = element_text(size = 15), axis.text = element_text(size = 15),plot.title = element_text(hjust = 0.5,size=16),legend.title = element_text(size = 18),axis.title.x = element_text(size = 20))+
  ggtitle(expression(atop('True Log Power', paste('Spectrum ('~omega[2]==1~")"))))



##### combine the plots
theme_set(theme_pubr())

figure <- ggarrange(est_var1_small,est_var1_large,est_var2_small,est_var2_large,True_var1_small,True_var1_large,True_var2_small,True_var2_large, common.legend=TRUE, legend="bottom", ncol=4,nrow = 2)
annotate_figure(figure,left = text_grob("Frequency\n", size=22,rot = 90))






