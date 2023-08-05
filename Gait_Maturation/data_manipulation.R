################################################################################
# COMMENT: due to seed issue, the produced data might be different from that  ##
# contained in R_gain_data_stridenumber                                       ##
################################################################################


rm(list=ls())

gaindata=list()
newdata=list()


set.seed(6666)


## read the variable table
variabledata=read.table("Gait_Maturation/data/variable_table.txt",header=TRUE,sep = ",")
names(variabledata)=c("ID","age","gender","height","weight","leg_length","speed","group")

## read the data index+age
for(i in 1:50)
{
  gaindata[[i]]=read.table(paste("Gait_Maturation/data/",i,"-",variabledata$age[i],".txt",sep = ""))
  
}

### 
for(i in 1:50)
{
  gain=gaindata[[i]]
  
  ## remove the first 60s and the last 5s
  index1=which(gain[,1]>(min(gain[,1])+60) & gain[,1]<(max(gain[,1])-5))
  gain1=gain[index1,]
  

  # ## remove the pause that stride time > 2s and the 5s before and after the pause
  index2=which(gain1[,2]>2)
  
  if(length(index2)==0)
  {
    newdata[[i]]=gain1[1:256,]
    
  }else{
    
    jj=0
    index3=c()
    for(j in 1:length(index2))
    {
      jj=jj+1
      if(jj==1)
      {
        index3=which(gain1[,1]>(gain1[index2[jj],1]+5) | gain1[,1]<(gain1[index2[jj],1]-5))
      }else{
        index_temp=which(gain1[,1]>(gain1[index2[jj],1]+5) | gain1[,1]<(gain1[index2[jj],1]-5))
        index3=intersect(index3,index_temp)
      }
      
    }
    
    gain_temp=gain1[index3,]
    newdata[[i]]=gain_temp[1:256,]
    
  }
  
}



### get the output time series data
outdata=matrix(0,256,50)
for(i in 1:50)
{
  outdata[,i]=newdata[[i]][,2]
}

### plot the time series data
for(i in 1:50)
{
  ts.plot(outdata[,i], main=i,ylim=c(0.7,1.6))
}


## copy the output
outdata_c=outdata

############### remove outliers

##########
i=6
ts.plot(outdata[,i], main=paste("original",i))

p10=quantile(outdata[,i],0.1)
p90=quantile(outdata[,i],0.9)

outlierpoint=which(outdata[,i]>1.2)
l=length(outlierpoint)
outdata_c[outlierpoint,i]=runif(l)*(p90-p10)+p10

ts.plot(outdata_c[,i], main=paste("after remove outliers",i))

##########
i=14
ts.plot(outdata[,i], main=paste("original",i))

p10=quantile(outdata[,i],0.1)
p90=quantile(outdata[,i],0.9)

outlierpoint=which(outdata[,i]>1.2)
l=length(outlierpoint)
outdata_c[outlierpoint,i]=runif(l)*(p90-p10)+p10

ts.plot(outdata_c[,i], main=paste("after remove outliers",i))

##########
i=15
ts.plot(outdata[,i], main=paste("original",i))

p10=quantile(outdata[,i],0.1)
p90=quantile(outdata[,i],0.9)

outlierpoint=which(outdata[,i]>1)
l=length(outlierpoint)
outdata_c[outlierpoint,i]=runif(l)*(p90-p10)+p10

outlierpoint_s=which(outdata[,i]<0.7)
ls=length(outlierpoint_s)
outdata_c[outlierpoint_s,i]=runif(ls)*(p90-p10)+p10

ts.plot(outdata_c[,i], main=paste("after remove outliers",i))

##########
i=16
ts.plot(outdata[,i], main=paste("original",i))

p10=quantile(outdata[,i],0.1)
p90=quantile(outdata[,i],0.9)

outlierpoint=which(outdata[,i]>1.2)
l=length(outlierpoint)
outdata_c[outlierpoint,i]=runif(l)*(p90-p10)+p10

ts.plot(outdata_c[,i], main=paste("after remove outliers",i))


##########
i=23
ts.plot(outdata[,i], main=paste("original",i))

p10=quantile(outdata[,i],0.1)
p90=quantile(outdata[,i],0.9)

outlierpoint=which(outdata[,i]>1.2)
l=length(outlierpoint)
outdata_c[outlierpoint,i]=runif(l)*(p90-p10)+p10

# outlierpoint_s=which(outdata[,i]<0.75)
# ls=length(outlierpoint_s)
# outdata_c[outlierpoint_s,i]=runif(ls)*(p90-p10)+p10

ts.plot(outdata_c[,i], main=paste("after remove outliers",i))

##########
i=35
ts.plot(outdata[,i], main=paste("original",i))

p10=quantile(outdata[,i],0.1)
p90=quantile(outdata[,i],0.9)

outlierpoint=which(outdata[,i]>1.2)
l=length(outlierpoint)
outdata_c[outlierpoint,i]=runif(l)*(p90-p10)+p10

ts.plot(outdata_c[,i], main=paste("after remove outliers",i))


##########
i=50
ts.plot(outdata[,i], main=paste("original",i))

p10=quantile(outdata[,i],0.1)
p90=quantile(outdata[,i],0.9)

outlierpoint=which(outdata[,i]>1.25)
l=length(outlierpoint)
outdata_c[outlierpoint,i]=runif(l)*(p90-p10)+p10

ts.plot(outdata_c[,i], main=paste("after remove outliers",i))


x_t=outdata_c


### final plot
for(i in 1:50)
{
  ts.plot(x_t[,i],main=i)
}

# standardized
for (i in 1:50)
{
  xmat=cbind(matrix(1,dim(x_t)[1],1), matrix(seq(1,dim(x_t)[1],1),dim(x_t)[1],1))
  linfit=solve(t(xmat)%*%xmat)%*%t(xmat)%*%x_t[,i]
  x_t[,i]=x_t[,i]-xmat%*%linfit
}


x=variabledata[,c(2,3,7)]  ## choose Age, Gender, Speed

save(x,x_t,file="Gait_Maturation/R_gait_data_stridenumber.RData")

