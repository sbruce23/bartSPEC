########################################################################################
# Description: This file generate age estimation with inverse regression 
#               for gait maturation data and the plots as described in the paper
#
# Instruction: Please install all the packages in the demo.R file
#              Please skip the generate age-dependence power spectrum part to save running time,
#              and load the partial dependence R data file to generate the plots
#########################################################################################


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



########################################### 
###  Application Data---Gait Maturation
##########################################

load("Gait_Maturation/R_gait_data_stridenumber.RData")

#x=x[,c(1,2,6)]

Nobs=256
L=50
NumX=3



###  Covariate type and possible cut number
VarType=c("ORD","CAT","ORD")
numcut=c(100,2,100)


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


### LOOCV


num_train=49
num_test=1
index=c(1:50)



est_age=rep(0,L)
true_age=rep(0,L)

est_gender=rep(0,L)
true_gender=rep(0,L)


#####  Partial Dependence initial settings

var_test=1  # var_test=1 is for age, var_test=2 is for gender

if(var_test==1)
{
  splitnum=20
  
}else if(var_test==2){
  
  splitnum=2
}

### initial spec_est
spec_partial_mean=array(0,c(L,splitnum,Nobs))




if(var_test==1)
{
  
  ##### set test variable
  
  max=max(x[,var_test])
  min=min(x[,var_test])
  xinc=(max-min)/(splitnum+1)
  numcut[var_test]=100
  xinc_cut=(max-min)/(100+1)
  
  ## set possible cut point for the test variable
  rule_vartest=seq(min+xinc_cut,min+numcut[var_test]*xinc_cut,by=xinc_cut)
  
  ## set several different values for the test variable 
  partial_value=seq(min,max,(max-min)/(splitnum-1))
  
}else if(var_test==2){
  
  
  partial_value=c("F","M")
  
}


###############################################
#  Generate age-dependent power spectrum data
################################################


library(parallel)
library(doParallel)


repli=50

ncore=detectCores(all.tests = FALSE, logical = TRUE)
c1 <- makeCluster(ncore-1,outfile="log1.txt")
registerDoParallel(c1)


result <- foreach(ii=1:repli, .errorhandling="pass") %dopar%{
  
  cat(paste("Starting iteration",ii,"\n"))
  
  ###  BartSPEC model
  
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
  
  #set.seed(6666)
  aa=BayesSumOfTreesSPEC(x_t[,-ii],x[-ii,],Nobs,num_train,Ntree,NumX,VarType,numcut,nus,Gs,param_random,theta,darton,opt)
  
  return(list(tree=aa$tree_store))
  
}

stopCluster(c1)





repli=50
ncore=detectCores(all.tests = FALSE, logical = TRUE)
c1 <- makeCluster(ncore-1,outfile="log3.txt")
registerDoParallel(c1)

result <- foreach(ii=1:repli, .errorhandling="pass") %dopar%{
  cat(paste("Starting iteration",ii,"\n"))
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
  
  ## tree information
  aa=result[[i]]$tree
  
  ##### Age-dependence estimation 
  x_train=x[-ii,]
  x_test=x[ii,]
  
  
  if(var_test==1)
  {
    
    ##### set test variable
    
    max=max(x[,var_test])
    min=min(x[,var_test])
    xinc=(max-min)/(splitnum+1)
    numcut[var_test]=100
    xinc_cut=(max-min)/(100+1)
    
    ## set possible cut point for the test variable
    rule_vartest=seq(min+xinc_cut,min+numcut[var_test]*xinc_cut,by=xinc_cut)
    
    ## set several different values for the test variable 
    partial_value=seq(min,max,(max-min)/(splitnum-1))
    
  }else if(var_test==2){
    
    
    partial_value=c("F","M")
    
  }
  
  
  xx=x_train
  for(qq in 1:splitnum)
  {
    
    ### set test value
    xx[,var_test]=partial_value[qq]
    
    ### initialize
    nseg_time_temp=Nobs
    Ntime=Nobs
    NumObs=num_train
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
      #tree=aa$tree_store[p,]
      tree=aa[p,]
      
      for(i in 1:Ntree)
      {
        cat("ite ii is",ii,"\n")
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
    
    
    spec_est_mean=matrix(0,(nloop-nwarmup+1),nseg_time_temp)
    spec_est_mean=apply(fhat_log[nwarmup:nloop,,],c(1,2), mean)
    
    
    spec_partial_mean[ii,qq,]=apply(spec_est_mean,2,mean)
    
    
  }
  
  

  
  return(spec_partial_mean[ii,,])
  
}

stopCluster(c1)




###############################################
#  Load the results from partial dependence
#################################################

load("Gait_Maturation/PartialDependenceParallel_age.RData")



spec_partial_mean=array(0,c(L,splitnum,Nobs))


for(ii in 1:L)
{
  
  spec_partial_mean[ii,,]=result[[ii]]
}

###############################################
#  Estimation of power spectrum of test data
#################################################

spec_test=matrix(0,L,Nobs/2)

for(ii in 1:L)
{
  xxx=x_t[,ii]
  spec_test[ii,] <- log(spectrum(xxx,span=10,plot=F)$spec)
  
}



#############
#    MSE
############
mse=matrix(0,L,splitnum)
for(ii in 1:L)
{
  for(qq in 1:splitnum)
  {
    mse[ii,qq]=mean((spec_test[ii,]-spec_partial_mean[ii,qq,1:(Nobs/2)])^2)
    
  }
  
}


min_idx=apply(mse,1,function(x) which(x==min(x))[1])


if(var_test==1)
{
  est_age=(partial_value/12)[min_idx]
  true_age=(x$age)/12
  

}else if(var_test==2)
{
  est_gender=partial_value[min_idx]
  true_gender=x$gender
  

}


## plot of estimated age vs true age
data=data.frame(x=true_age,y=est_age)

print(ggplot(data=data,aes(x,y)) +  geom_point(color="grey")+theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5,size=18),axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),axis.text = element_text(size = 15))+
        xlab("True Age")+ylab("Estimated Age")+ggtitle("Estimated vs True of Age for 50 Observations"))+geom_abline(intercept = 0, slope = 1,color="red")


## fit linear regression
model=lm(est_age~true_age-1)
summary(model)
