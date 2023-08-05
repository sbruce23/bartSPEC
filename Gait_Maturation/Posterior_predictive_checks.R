#################################################################################
# Description: This file generate Posterior Predictive checks 
#               for gait maturation data and the plots as described in the paper
#
# Instruction: Please install all the packages in the demo.R file
#################################################################################




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
###  Tree model
############################
set.seed(456)
Ntree=5
opt=list(nloop=10000,nwarmup=5000,nbasis=7,
         sigmasqalpha=100,batchsize=2000,conv_diag="on")

nus=2
Gs=10
param_random=0.2


## DART
theta=1
rho=NumX
a=0.5
b=1
darton=0

### BART
aa=BayesSumOfTreesSPEC(x_t,x,Nobs,L,Ntree,NumX,VarType,numcut,nus,Gs,param_random,theta,rho,a,b,darton,opt)


####### PPC calculation
nfreq <- floor(Nobs/2)
esti_periodogram=array(0,c(L,1000,(nfreq)))

for(j in 1:L)
{
  for(p in 1:500)
  {
    
    esti_periodogram[j,p,]=aa$fhat_log[(p+9500-1),2:(nfreq+1),j]+log(rchisq(n=(nfreq),df=2)/2)
    
  }
  
}


### Plot of PPC
hw <- theme_gray()+ theme(
  plot.title=element_text(hjust=0.5,size=18,face="bold"),
  plot.subtitle=element_text(hjust=0.5,size=12),
  plot.caption=element_text(hjust=-.5,size=10),
  
  #  strip.text.y = element_blank(),
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), size=.2),
  
  panel.border=element_rect(fill=FALSE,colour=gray(.70)),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.spacing.x = unit(0.2,"cm"),
  panel.spacing.y = unit(0.2,"cm"),
  
  # axis.ticks.y= element_blank()
  # axis.ticks=element_blank(),
  axis.text=element_text(colour="black",size=10),
  axis.text.y=element_text(margin=margin(0,3,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0)),
  axis.title=element_text(size=16,face="bold"),
  
  legend.text=element_text(size=14),
  legend.title = element_blank(),
  
  
)

#### log periodogram plot for first participant

for(j in 1:L)
{
  
  ydata=aa$Y[2:(nfreq+1),j]
  freq = seq(from=1/256,by=1/256,length.out=nfreq)
  df=data.frame(frequency=freq,observed=ydata)
  p1=ggplot()+xlab('Frequency')+ylab('Log Periodogram')
  p2=ggplot()+xlab('Log Periodogram')+ylab('Density')
  for (i in 1:250){
    dfobs=data.frame(frequency=freq,estimated=esti_periodogram[j,i,])
    p1=p1+geom_line(data=dfobs,aes(x=frequency,y=estimated),alpha=1,color='lightblue')
    p2=p2+geom_density(data=dfobs,aes(x=estimated),alpha=1,color='lightblue')
  }
  p1=p1+geom_line(data=df,aes(x=frequency,y=observed))+hw;
  p2=p2+geom_density(data=df,aes(x=observed))+hw;
  ggarrange(p1,p2);
  
  path=paste('Posterior_predictive_plot/posteriorpredictiveplotws_',j,'.png',sep = "")
  ggsave(path)
  
}










