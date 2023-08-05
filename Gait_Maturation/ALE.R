#####################################################################
# Description: This file generate ALE plots of age gender, and speed for gait 
#              maturation data and the convergence diagnostics as 
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

library(BayesSumOfTreesSPEC)
library(optimizationarmadillo)
library(Treecpp12262020)


source("programs/Bottom_nodes.r")
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

############################ 
###  BartSPEC model
############################
set.seed(6666)
aa=BayesSumOfTreesSPEC(x_t,x,Nobs,L,Ntree,NumX,VarType,numcut,nus,Gs,param_random,theta,darton,opt)


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
ggplot(data=bottom,aes(x,y))+geom_line(data=bottom,color="black")+xlab("\nIteration")+ylab("Number of Bottom Nodes")+theme_minimal()+ylim(5,50)+
  theme(axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),legend.text = element_text(size = 18), axis.text = element_text(size = 18))

## number of nodes
node=as.data.frame(aa$Num_node)
node$ID=1:opt$nloop
node_melt=reshape2::melt(node,id.var="ID")
ggplot(data=node_melt,aes(x=ID,y=value,color=variable))+geom_line(size=1)+xlab("\nIteration")+ylim(0,25)+labs(color = "Tree")+ylab("Number of Nodes")+scale_color_discrete(labels = c("1", "2", "3","4","5"))+theme_minimal()+
  theme(axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18),legend.position = c(0.6, 0.9),legend.direction = "horizontal", legend.text = element_text(size = 18), axis.text = element_text(size = 18))





##########################
###  ALE of age
###########################

var_test=x$age
partial_value=quantile(var_test,seq(0,1,1/6))


xx=x
xx$age_small=var_test
xx$age_big=var_test


LL=c()
for(i in 1:(length(partial_value)-1))
{
  if(i<(length(partial_value)-1))
  {
    xx$age_small[which(var_test<partial_value[i+1] & var_test>=partial_value[i])]=partial_value[i]
    xx$age_big[which(var_test<partial_value[i+1] & var_test>=partial_value[i])]=partial_value[i+1]
    LL[i]=length(which(var_test<partial_value[i+1] & var_test>=partial_value[i]))
    
  }else{
    xx$age_small[which(var_test<=partial_value[i+1] & var_test>=partial_value[i])]=partial_value[i]
    xx$age_big[which(var_test<=partial_value[i+1] & var_test>=partial_value[i])]=partial_value[i+1]
    LL[i]=length(which(var_test<=partial_value[i+1] & var_test>=partial_value[i]))
    
  }
  
}



## initialize 
nseg_time_temp=Nobs
Ntime=Nobs
NumObs=L
nwarmup=opt$nwarmup
nloop=opt$nloop
  
  
## rule
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
  



################# SMALL ######################
mtotalfit=matrix(0,nseg_time_temp,NumObs)
mtrainFits=array(0,c(Ntree,nseg_time_temp,NumObs))
fhat_log_small=array(0,c(nloop,nseg_time_temp,NumObs))


########## data pass to model
# age
xxx=xx[,c((NumX+1),2:NumX)]



### pass to the tree model
  for (p in nwarmup:nloop)
  {
    ## set pth loop tree
    tree=aa$tree_store[p,]
    
    for(i in 1:Ntree)
    {
     
      cat("ite p is",p,"\n")
      cat("tree is",i,"\n")
      
      
      for (j in 1:NumObs)
      {
        
        idx=FindNodeC(j,1,i,tree,xxx,VarType,RuleNum,Rulemat) # from the top node of the ith tree, find the bottom idx for jth obs
        
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
      apply_sum_vecC(mtrainFits,c(Ntree,Ntime,NumObs),fhat_log_small,nloop,p)
      
    }
    
  }
  
  
################# BIG ######################
mtotalfit=matrix(0,nseg_time_temp,NumObs)
mtrainFits=array(0,c(Ntree,nseg_time_temp,NumObs))
fhat_log_big=array(0,c(nloop,nseg_time_temp,NumObs))


########## data pass to model
# age
xxx=xx[,c((NumX+2),2:NumX)]



### pass to the tree model
for (p in nwarmup:nloop)
{
  ## set pth loop tree
  tree=aa$tree_store[p,]
  
  for(i in 1:Ntree)
  {
    
    cat("ite p is",p,"\n")
    cat("tree is",i,"\n")
    
    
    for (j in 1:NumObs)
    {
      
      idx=FindNodeC(j,1,i,tree,xxx,VarType,RuleNum,Rulemat) # from the top node of the ith tree, find the bottom idx for jth obs
      
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
    apply_sum_vecC(mtrainFits,c(Ntree,Ntime,NumObs),fhat_log_big,nloop,p)
    
  }
  
}



############# ratio #####################
sum_bin=matrix(0,L,(nloop-nwarmup+1))


### low-to-high ratio
xindex=seq(0,0.5,0.5/(Nobs/2))
low=which(xindex>0.05 & xindex<=0.25)
high=which(xindex>0.25 & xindex<=0.5)



for(i in 1:length(LL))
{
  
  if(i==1)
  {
    
    #sum_temp=apply(exp(fhat_log_big[nwarmup:nloop,,1:LL[i]])-exp(fhat_log_small[nwarmup:nloop,,1:LL[i]]),c(1,2),mean)
    
    sumlow_big=apply(exp(fhat_log_big[nwarmup:nloop,low,1:LL[i]]),c(1,3),sum)
    sumhigh_big=apply(exp(fhat_log_big[nwarmup:nloop,high,1:LL[i]]),c(1,3),sum)
    ratio_big=sumlow_big/sumhigh_big
    
    sumlow_small=apply(exp(fhat_log_small[nwarmup:nloop,low,1:LL[i]]),c(1,3),sum)
    sumhigh_small=apply(exp(fhat_log_small[nwarmup:nloop,high,1:LL[i]]),c(1,3),sum)
    ratio_small=sumlow_small/sumhigh_small
    
    sum_temp=apply(ratio_big-ratio_small,1,mean)
    
    sum_bin[1:LL[i],]=repmat(sum_temp,LL[i],1)
    l=0+LL[i]
    print(l)
    
  }else
  {
   
    #sum_temp=sum_temp+apply(exp(fhat_log_big[nwarmup:nloop,,(l+1):(l+LL[i])])-exp(fhat_log_small[nwarmup:nloop,,(l+1):(l+LL[i])]),c(1,2),mean)
    
    
    sumlow_big=apply(exp(fhat_log_big[nwarmup:nloop,low,(l+1):(l+LL[i])]),c(1,3),sum)
    sumhigh_big=apply(exp(fhat_log_big[nwarmup:nloop,high,(l+1):(l+LL[i])]),c(1,3),sum)
    ratio_big=sumlow_big/sumhigh_big
    
    sumlow_small=apply(exp(fhat_log_small[nwarmup:nloop,low,(l+1):(l+LL[i])]),c(1,3),sum)
    sumhigh_small=apply(exp(fhat_log_small[nwarmup:nloop,high,(l+1):(l+LL[i])]),c(1,3),sum)
    ratio_small=sumlow_small/sumhigh_small
    
    sum_temp=sum_temp+apply(ratio_big-ratio_small,1,mean)
    
    
    #sum_temp=sum_temp+apply(exp(fhat_log_big[nwarmup:nloop,,(l+1):(l+LL[i])])-exp(fhat_log_small[nwarmup:nloop,,(l+1):(l+LL[i])]),c(1,2),mean)
    
    
    sum_bin[(l+1):(l+LL[i]),]=repmat(sum_temp,LL[i],1)
    l=l+LL[i]
    
    print(l)
  }
  
}

## centered
sum_bin_final=apply(sum_bin, 2, function(y) y - mean(y))
spec_partial_mean=apply(sum_bin_final,1,mean)
spec_partial_05=apply(sum_bin_final,1,quantile,probs=0.025)
spec_partial_95=apply(sum_bin_final,1,quantile,probs=0.975)

# bin index
bindex=c()
c=1
for(i in 1:length(LL))
{

  if(i==1)
  {
    bindex[i]=c
  }else{
    
    c=c+LL[i-1]
    bindex[i]=c
  }
  
}


ratio <- data.frame(X =sort(var_test)[bindex]/12,
                    Y =spec_partial_mean[bindex],
                    L =spec_partial_05[bindex],
                    U =spec_partial_95[bindex])


ggplot(ratio, aes(x=X)) + 
  geom_line(aes(y=Y), colour="blue",size=0.8) + geom_point(aes(y=Y),colour="blue")+xlab("\nAge (year)")+ylab("ALE\n")+theme_minimal()+scale_x_continuous(breaks = c(4,6,8,10))+scale_y_continuous(breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5),limits = c(-1.8,1.5))+
  geom_ribbon(aes(ymin=L, ymax=U), alpha=0.3)+
  theme(plot.margin=margin(10,10,10,10), axis.title.x = element_text(size = 19),axis.title.y = element_text(size = 19),axis.text = element_text(size = 19))





############# Power spectrum #####################
sum_bin=array(0,c((nloop-nwarmup+1),Nobs,L))



for(i in 1:length(LL))
{
  
  if(i==1)
  {
    
    sum_temp=apply(exp(fhat_log_big[nwarmup:nloop,,1:LL[i]])-exp(fhat_log_small[nwarmup:nloop,,1:LL[i]]),c(1,2),mean)
    
    sum_bin[,,(1:LL[i])]=sum_temp
    l=0+LL[i]
    print(l)
    
  }else
  {
    
    sum_temp=sum_temp+apply(exp(fhat_log_big[nwarmup:nloop,,(l+1):(l+LL[i])])-exp(fhat_log_small[nwarmup:nloop,,(l+1):(l+LL[i])]),c(1,2),mean)
    
    sum_bin[,,(l+1):(l+LL[i])]=sum_temp
    l=l+LL[i]
    
    print(l)
  }
  
}



## centered
sum_bin_final=apply(sum_bin, c(1,2), function(y) y - mean(y))

spec_partial_mean=t(apply(sum_bin_final,c(1,3),mean))


### plot
nfreq <- floor(Nobs/2)
yindex=sort(var_test)[bindex]/12
xindex=seq(0,0.5,0.5/(Nobs/2))
Log_Spectrum=spec_partial_mean[1:(nfreq+1),bindex]


library(plotly)
fig=plot_ly(x=~yindex,y=~xindex,z=~Log_Spectrum,showscale=FALSE) %>% add_surface() %>% 
  layout(scene=list(yaxis=list(title="Frequency",tickfont=list(size=14), titlefont = list(size = 20)),xaxis=list(title="Age (year)",tickfont=list(size=14), titlefont = list(size = 20)),zaxis=list(title="ALE",dtick = 0.004,range=c(-0.01,0.01),tickfont=list(size=14), titlefont = list(size = 20))))
fig






########################
#    ALE for Speed
#########################
var_test=x$speed

partial_value=quantile(var_test,seq(0,1,1/6))


xx=x
xx$speed_small=var_test
xx$speed_big=var_test


LL=c()
for(i in 1:(length(partial_value)-1))
{
  if(i<(length(partial_value)-1))
  {
    xx$speed_small[which(var_test<partial_value[i+1] & var_test>=partial_value[i])]=partial_value[i]
    xx$speed_big[which(var_test<partial_value[i+1] & var_test>=partial_value[i])]=partial_value[i+1]
    LL[i]=length(which(var_test<partial_value[i+1] & var_test>=partial_value[i]))
    
  }else{
    xx$speed_small[which(var_test<=partial_value[i+1] & var_test>=partial_value[i])]=partial_value[i]
    xx$speed_big[which(var_test<=partial_value[i+1] & var_test>=partial_value[i])]=partial_value[i+1]
    LL[i]=length(which(var_test<=partial_value[i+1] & var_test>=partial_value[i]))
    
  }
  
}



## initialize 
nseg_time_temp=Nobs
Ntime=Nobs
NumObs=L
nwarmup=opt$nwarmup
nloop=opt$nloop


## rule
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




################# SMALL ######################
mtotalfit=matrix(0,nseg_time_temp,NumObs)
mtrainFits=array(0,c(Ntree,nseg_time_temp,NumObs))
fhat_log_small=array(0,c(nloop,nseg_time_temp,NumObs))


########## data pass to model
# speed
xxx=xx[,c(1,2,(NumX+1))]


### pass to the tree model
for (p in nwarmup:nloop)
{
  ## set pth loop tree
  tree=aa$tree_store[p,]
  
  for(i in 1:Ntree)
  {
    
    cat("ite p is",p,"\n")
    cat("tree is",i,"\n")
    
    
    for (j in 1:NumObs)
    {
      
      idx=FindNodeC(j,1,i,tree,xxx,VarType,RuleNum,Rulemat) # from the top node of the ith tree, find the bottom idx for jth obs
      
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
    apply_sum_vecC(mtrainFits,c(Ntree,Ntime,NumObs),fhat_log_small,nloop,p)
    
  }
  
}


################# BIG ######################
mtotalfit=matrix(0,nseg_time_temp,NumObs)
mtrainFits=array(0,c(Ntree,nseg_time_temp,NumObs))
fhat_log_big=array(0,c(nloop,nseg_time_temp,NumObs))


########## data pass to model
# speed
xxx=xx[,c(1,2,(NumX+2))]


### pass to the tree model
for (p in nwarmup:nloop)
{
  ## set pth loop tree
  tree=aa$tree_store[p,]
  
  for(i in 1:Ntree)
  {
    
    cat("ite p is",p,"\n")
    cat("tree is",i,"\n")
    
    
    for (j in 1:NumObs)
    {
      
      idx=FindNodeC(j,1,i,tree,xxx,VarType,RuleNum,Rulemat) # from the top node of the ith tree, find the bottom idx for jth obs
      
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
    apply_sum_vecC(mtrainFits,c(Ntree,Ntime,NumObs),fhat_log_big,nloop,p)
    
  }
  
}



############# ratio #####################
sum_bin=matrix(0,L,(nloop-nwarmup+1))


### low-to-high ratio
xindex=seq(0,0.5,0.5/(Nobs/2))
low=which(xindex>0.05 & xindex<=0.25)
high=which(xindex>0.25 & xindex<=0.5)



for(i in 1:length(LL))
{
  
  if(i==1)
  {
    
    #sum_temp=apply(exp(fhat_log_big[nwarmup:nloop,,1:LL[i]])-exp(fhat_log_small[nwarmup:nloop,,1:LL[i]]),c(1,2),mean)
    
    sumlow_big=apply(exp(fhat_log_big[nwarmup:nloop,low,1:LL[i]]),c(1,3),sum)
    sumhigh_big=apply(exp(fhat_log_big[nwarmup:nloop,high,1:LL[i]]),c(1,3),sum)
    ratio_big=sumlow_big/sumhigh_big
    
    sumlow_small=apply(exp(fhat_log_small[nwarmup:nloop,low,1:LL[i]]),c(1,3),sum)
    sumhigh_small=apply(exp(fhat_log_small[nwarmup:nloop,high,1:LL[i]]),c(1,3),sum)
    ratio_small=sumlow_small/sumhigh_small
    
    sum_temp=apply(ratio_big-ratio_small,1,mean)
    
    sum_bin[1:LL[i],]=repmat(sum_temp,LL[i],1)
    l=0+LL[i]
    print(l)
    
  }else
  {
    
    #sum_temp=sum_temp+apply(exp(fhat_log_big[nwarmup:nloop,,(l+1):(l+LL[i])])-exp(fhat_log_small[nwarmup:nloop,,(l+1):(l+LL[i])]),c(1,2),mean)
    
    
    sumlow_big=apply(exp(fhat_log_big[nwarmup:nloop,low,(l+1):(l+LL[i])]),c(1,3),sum)
    sumhigh_big=apply(exp(fhat_log_big[nwarmup:nloop,high,(l+1):(l+LL[i])]),c(1,3),sum)
    ratio_big=sumlow_big/sumhigh_big
    
    sumlow_small=apply(exp(fhat_log_small[nwarmup:nloop,low,(l+1):(l+LL[i])]),c(1,3),sum)
    sumhigh_small=apply(exp(fhat_log_small[nwarmup:nloop,high,(l+1):(l+LL[i])]),c(1,3),sum)
    ratio_small=sumlow_small/sumhigh_small
    
    sum_temp=sum_temp+apply(ratio_big-ratio_small,1,mean)
    
    
    #sum_temp=sum_temp+apply(exp(fhat_log_big[nwarmup:nloop,,(l+1):(l+LL[i])])-exp(fhat_log_small[nwarmup:nloop,,(l+1):(l+LL[i])]),c(1,2),mean)
    
    
    sum_bin[(l+1):(l+LL[i]),]=repmat(sum_temp,LL[i],1)
    l=l+LL[i]
    
    print(l)
  }
  
}

## centered
sum_bin_final=apply(sum_bin, 2, function(y) y - mean(y))
spec_partial_mean=apply(sum_bin_final,1,mean)
spec_partial_05=apply(sum_bin_final,1,quantile,probs=0.025)
spec_partial_95=apply(sum_bin_final,1,quantile,probs=0.975)

# bin index
bindex=c()
c=1
for(i in 1:length(LL))
{
  
  if(i==1)
  {
    bindex[i]=c
  }else{
    
    c=c+LL[i-1]
    bindex[i]=c
  }
  
}


ratio <- data.frame(X =sort(var_test)[bindex],
                    Y =spec_partial_mean[bindex],
                    L =spec_partial_05[bindex],
                    U =spec_partial_95[bindex])

ggplot(ratio, aes(x=X)) + 
  geom_line(aes(y=Y), colour="blue",size=0.8) + geom_point(aes(y=Y),colour="blue")+xlab("\nGait Speed (m/sec)")+ylab("ALE\n")+theme_minimal()+scale_y_continuous(breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5), limits = c(-1.8,1.5))+
  geom_ribbon(aes(ymin=L, ymax=U), alpha=0.3)+
  theme(plot.margin=margin(10,10,10,10), axis.title.x = element_text(size = 19),axis.title.y = element_text(size = 19),axis.text = element_text(size = 19))






############# Power spectrum #####################
sum_bin=array(0,c((nloop-nwarmup+1),Nobs,L))



for(i in 1:length(LL))
{
  
  if(i==1)
  {
    
    sum_temp=apply(exp(fhat_log_big[nwarmup:nloop,,1:LL[i]])-exp(fhat_log_small[nwarmup:nloop,,1:LL[i]]),c(1,2),mean)
    
    sum_bin[,,(1:LL[i])]=sum_temp
    l=0+LL[i]
    print(l)
    
  }else
  {
    
    sum_temp=sum_temp+apply(exp(fhat_log_big[nwarmup:nloop,,(l+1):(l+LL[i])])-exp(fhat_log_small[nwarmup:nloop,,(l+1):(l+LL[i])]),c(1,2),mean)
    
    sum_bin[,,(l+1):(l+LL[i])]=sum_temp
    l=l+LL[i]
    
    print(l)
  }
  
}



## centered
sum_bin_final=apply(sum_bin, c(1,2), function(y) y - mean(y))

spec_partial_mean=t(apply(sum_bin_final,c(1,3),mean))


### plot
nfreq <- floor(Nobs/2)
yindex=sort(var_test)[bindex]
xindex=seq(0,0.5,0.5/(Nobs/2))
Log_Spectrum=spec_partial_mean[1:(nfreq+1),bindex]


library(plotly)
fig=plot_ly(x=~yindex,y=~xindex,z=~Log_Spectrum,showscale=FALSE) %>% add_surface() %>% 
  layout(scene=list(yaxis=list(title="Frequency",tickfont=list(size=14), titlefont = list(size = 20)),xaxis=list(title="Gait Speed (m/sec)",tickfont=list(size=14), titlefont = list(size = 20)),zaxis=list(title="ALE",dtick = 0.004,range=c(-0.01,0.01),tickfont=list(size=14), titlefont = list(size = 20))))
fig



########################
#    ALE for Gender
#########################


var_test=x$gender
partial_value=c("M","F")



xx=x
xx$gender_small="F"
xx$gender_big="M"





## initialize 
nseg_time_temp=Nobs
Ntime=Nobs
NumObs=L
nwarmup=opt$nwarmup
nloop=opt$nloop


## rule
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




################# SMALL ######################
mtotalfit=matrix(0,nseg_time_temp,NumObs)
mtrainFits=array(0,c(Ntree,nseg_time_temp,NumObs))
fhat_log_small=array(0,c(nloop,nseg_time_temp,NumObs))


########## data pass to model
# gender
xxx=xx[,c(1,4,3)]



### pass to the tree model
for (p in nwarmup:nloop)
{
  ## set pth loop tree
  tree=aa$tree_store[p,]
  
  for(i in 1:Ntree)
  {
    
    cat("ite p is",p,"\n")
    cat("tree is",i,"\n")
    
    
    for (j in 1:NumObs)
    {
      
      idx=FindNodeC(j,1,i,tree,xxx,VarType,RuleNum,Rulemat) # from the top node of the ith tree, find the bottom idx for jth obs
      
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
    apply_sum_vecC(mtrainFits,c(Ntree,Ntime,NumObs),fhat_log_small,nloop,p)
    
  }
  
}


################# BIG ######################
mtotalfit=matrix(0,nseg_time_temp,NumObs)
mtrainFits=array(0,c(Ntree,nseg_time_temp,NumObs))
fhat_log_big=array(0,c(nloop,nseg_time_temp,NumObs))


########## data pass to model
# gender
xxx=xx[,c(1,5,3)]


### pass to the tree model
for (p in nwarmup:nloop)
{
  ## set pth loop tree
  tree=aa$tree_store[p,]
  
  for(i in 1:Ntree)
  {
    
    cat("ite p is",p,"\n")
    cat("tree is",i,"\n")
    
    
    for (j in 1:NumObs)
    {
      
      idx=FindNodeC(j,1,i,tree,xxx,VarType,RuleNum,Rulemat) # from the top node of the ith tree, find the bottom idx for jth obs
      
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
    apply_sum_vecC(mtrainFits,c(Ntree,Ntime,NumObs),fhat_log_big,nloop,p)
    
  }
  
}



############# ratio #####################
sum_bin=matrix(0,L,(nloop-nwarmup+1))


### low-to-high ratio
xindex=seq(0,0.5,0.5/(Nobs/2))
low=which(xindex>0.05 & xindex<=0.25)
high=which(xindex>0.25 & xindex<=0.5)


LL=50


sumlow_big=apply(exp(fhat_log_big[nwarmup:nloop,low,1:LL]),c(1,3),sum)
sumhigh_big=apply(exp(fhat_log_big[nwarmup:nloop,high,1:LL]),c(1,3),sum)
ratio_big=sumlow_big/sumhigh_big

sumlow_small=apply(exp(fhat_log_small[nwarmup:nloop,low,1:LL]),c(1,3),sum)
sumhigh_small=apply(exp(fhat_log_small[nwarmup:nloop,high,1:LL]),c(1,3),sum)
ratio_small=sumlow_small/sumhigh_small

sum_temp=apply(ratio_big-ratio_small,1,mean)



## centered
spec_partial_mean=mean(sum_temp)
spec_partial_05=quantile(sum_temp,probs=0.025)
spec_partial_95=quantile(sum_temp,probs=0.975)

# bin index
bindex=c()
c=1
for(i in 1:length(LL))
{
  
  if(i==1)
  {
    bindex[i]=c
  }else{
    
    c=c+LL[i-1]
    bindex[i]=c
  }
  
}


ratio <- data.frame(X =sort(var_test)[bindex],
                    Y =spec_partial_mean[bindex],
                    L =spec_partial_05[bindex],
                    U =spec_partial_95[bindex])


ggplot(ratio, aes(x=X)) + 
  geom_point(aes(y=Y),colour="blue")+xlab("\n Gender")+ylab("ALE\n")+theme_minimal()+scale_y_continuous(breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5),limits = c(-1.8,1.5))+
  geom_point(aes(y=L),colour="black")+geom_point(aes(y=U),colour="black")+
  theme(plot.margin=margin(10,10,10,10), axis.title.x = element_text(size = 19),axis.title.y = element_text(size = 19),axis.text = element_text(size = 19))





############# Power spectrum #####################
sum_bin=array(0,c((nloop-nwarmup+1),Nobs,L))



for(i in 1:length(LL))
{
  
  if(i==1)
  {
    
    sum_temp=apply(exp(fhat_log_big[nwarmup:nloop,,1:LL[i]])-exp(fhat_log_small[nwarmup:nloop,,1:LL[i]]),c(1,2),mean)
    
    sum_bin[,,(1:LL[i])]=sum_temp
    l=0+LL[i]
    print(l)
    
  }else
  {
    
    sum_temp=sum_temp+apply(exp(fhat_log_big[nwarmup:nloop,,(l+1):(l+LL[i])])-exp(fhat_log_small[nwarmup:nloop,,(l+1):(l+LL[i])]),c(1,2),mean)
    
    sum_bin[,,(l+1):(l+LL[i])]=sum_temp
    l=l+LL[i]
    
    print(l)
  }
  
}



## centered
#sum_bin_final=apply(sum_bin, c(1,2), function(y) y - mean(y))

spec_partial_mean=apply(sum_bin,c(2,3),mean)
spec_partial_05=apply(sum_bin,c(2,3),quantile,probs=0.025)
spec_partial_95=apply(sum_bin,c(2,3),quantile,probs=0.975)

### plot
nfreq <- floor(Nobs/2)
yindex=sort(var_test)[bindex]
xindex=seq(0,0.5,0.5/(Nobs/2))
Log_Spectrum=spec_partial_mean[1:(nfreq+1),bindex]

plot(x=xindex,y=Log_Spectrum, ylim=c(-0.002,0.001),xlab = "frequency",ylab = "ALE")
lines(x=xindex,y=spec_partial_05[1:(nfreq+1),bindex],col="blue")
lines(x=xindex,y=spec_partial_95[1:(nfreq+1),bindex],col="blue")



power <- data.frame(X =xindex,
                    Y =Log_Spectrum,
                    L =spec_partial_05[1:(nfreq+1),bindex],
                    U =spec_partial_95[1:(nfreq+1),bindex])

ggplot(power, aes(x=X)) + 
  geom_line(aes(y=Y), colour="blue",size=0.3) + geom_point(aes(y=Y),colour="blue")+xlab("\nFrequency")+ylab("ALE\n")+theme_minimal()+ylim(-0.0020,0.0010)+
  geom_ribbon(aes(ymin=L, ymax=U), alpha=0.3)+
  theme(plot.margin=margin(10,10,10,10), axis.title.x = element_text(size = 19),axis.title.y = element_text(size = 19),axis.text = element_text(size = 19))



