####### some functions

## Rule::Right: returns 1 if vector x "goes" to right node, 0 otherelse
Right=function(tree,treeI,idx,x,var,Rule,VarType,RuleNum,Rulemat)
{
  if(VarType[var]=="CAT")
  {
    for(i in 1:RuleNum[var])
    {
      if(x[var]==Rulemat[[var]][i])
      {
        res=ifelse(tree[[treeI]][[paste("node",idx,sep = "")]]$Rule[i]==1,1,0)
      }
    }
  }else{
    
    res=ifelse(x[var]>Rulemat[[var]][Rule],1,0)
  }
  
  return(res)
}

## Node::SetData: add a cell=i to the datalist from index of node equals to index
SetData=function(tree,treeI,index,i,x,VarType,RuleNum,Rulemat)
{
  
  stp=0
  idx=index
  while(stp==0)
  {
    if(!tree[[treeI]][[paste("node",idx,sep = "")]]$Bot)
    {
      if(Right(tree,treeI,idx,x[i,],tree[[treeI]][[paste("node",idx,sep = "")]]$Var,tree[[treeI]][[paste("node",idx,sep = "")]]$Rule,VarType,RuleNum,Rulemat))
      {
        tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$DataList[length(tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$DataList)+1]=i
        tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$DataList=unique(tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$DataList)
        
        idx=tree[[treeI]][[paste("node",idx,sep = "")]]$RightC
      }else{
        
        tree[[treeI]][[paste("node",2*idx,sep = "")]]$DataList[length(tree[[treeI]][[paste("node",2*idx,sep = "")]]$DataList)+1]=i
        tree[[treeI]][[paste("node",2*idx,sep = "")]]$DataList=unique(tree[[treeI]][[paste("node",2*idx,sep = "")]]$DataList)
        
        idx=tree[[treeI]][[paste("node",idx,sep = "")]]$LeftC
      }
      
    }else{
      
      tree[[treeI]][[paste("node",idx,sep = "")]]$DataList[length(tree[[treeI]][[paste("node",idx,sep = "")]]$DataList)+1]=i
      tree[[treeI]][[paste("node",idx,sep = "")]]$DataList=unique(tree[[treeI]][[paste("node",idx,sep = "")]]$DataList)
      stp=1
    }
  }
  
  return(tree)
  
}

## BotNodes: returns number of bottom nodes and list of index of bottom nodes
BotNodes=function(tree,treeI,idx) # idx: top node
{
  
  sum=0
  index=c()
  
  if(!tree[[treeI]][[paste("node",idx,sep = "")]]$Bot)
  {
    #idx_l=tree[[treeI]][[paste("node",idx,sep = "")]]$LeftC
    #idx_r=tree[[treeI]][[paste("node",idx,sep = "")]]$RightC
    sum=sum+BotNodes(tree,treeI,2*idx)$BotNumber+BotNodes(tree,treeI,2*idx+1)$BotNumber
    index=append(BotNodes(tree,treeI,2*idx)$BotIdx,BotNodes(tree,treeI,2*idx+1)$BotIdx)
    
  }else if(tree[[treeI]][[paste("node",idx,sep = "")]]$Bot && length(tree[[treeI]][[paste("node",idx,sep = "")]]$DataList)>1)
  {
    index=append(index,idx)
    sum=sum+tree[[treeI]][[paste("node",idx,sep = "")]]$Bot
  }
  
  if(length(index)==0)
  {
    sum=0
    index=NULL
  }
  
  list(BotNumber=sum,BotIdx=index)
}

BotNodes_without_condition=function(tree,treeI,idx) # idx: top node
{
  
  sum=0
  index=c()
  
  if(!tree[[treeI]][[paste("node",idx,sep = "")]]$Bot)
  {
    #idx_l=tree[[treeI]][[paste("node",idx,sep = "")]]$LeftC
    #idx_r=tree[[treeI]][[paste("node",idx,sep = "")]]$RightC
    sum=sum+BotNodes_without_condition(tree,treeI,2*idx)$BotNumber+BotNodes_without_condition(tree,treeI,2*idx+1)$BotNumber
    index=append(BotNodes_without_condition(tree,treeI,2*idx)$BotIdx,BotNodes_without_condition(tree,treeI,2*idx+1)$BotIdx)
    
  }else if(tree[[treeI]][[paste("node",idx,sep = "")]]$Bot)
  {
    index=append(index,idx)
    sum=sum+tree[[treeI]][[paste("node",idx,sep = "")]]$Bot
  }
  
  if(length(index)==0)
  {
    sum=0
    index=NULL
  }
  
  list(BotNumber=sum,BotIdx=index)
}

## NogNodes: returns number of Nog nodes and list of index of Nog nodes
NogNodes=function(tree,treeI,idx)
{
  sum=0
  index=c()
  if(tree[[treeI]][[paste("node",idx,sep = "")]]$Bot)
  {
    sum=0
    index=c()
  }else if(tree[[treeI]][[paste("node",idx,sep = "")]]$Nog)
  {
    
    index=append(index,idx)
    sum=tree[[treeI]][[paste("node",idx,sep = "")]]$Nog
  }else{
    
    sum=NogNodes(tree,treeI,2*idx)$NogNumber+NogNodes(tree,treeI,2*idx+1)$NogNumber
    index=append(NogNodes(tree,treeI,2*idx)$NogIdx,NogNodes(tree,treeI,2*idx+1)$NogIdx)
  }
  
  list(NogNumber=sum,NogIdx=index)
}

## Depth
Depth=function(tree,treeI,idx)
{
  d=0
  while(!tree[[treeI]][[paste("node",idx,sep = "")]]$Top)
  {
    d=d+1
    idx=tree[[treeI]][[paste("node",idx,sep = "")]]$Parent
  }
  return(d)
}

##BottomPath: obtain the useage variables and cut points for each bottom node
BottomPath=function(tree,treeI,idx) # input is BotNodes$BotIdx
{
  varidx=c()
  ruleidx=c()
  LeftorRight=c()
  if(tree[[treeI]][[paste("node",idx,sep = "")]]$Top)
  {
    varidx=-1
    ruleidx=-1
    LeftorRight=-1
  }else{
    
    stp=0
    while(stp==0)
    {
      
      idx_p=tree[[treeI]][[paste("node",idx,sep = "")]]$Parent
      LeftorRight=append(LeftorRight,ifelse(tree[[treeI]][[paste("node",idx_p,sep = "")]]$RightC==idx,1,0)) # if right child, return 1
      idx=idx_p
      if(!tree[[treeI]][[paste("node",idx,sep = "")]]$Top)
      {
        varidx=append(varidx,tree[[treeI]][[paste("node",idx,sep = "")]]$Var)
        ruleidx=append(ruleidx,tree[[treeI]][[paste("node",idx,sep = "")]]$Rule)
        
      }else{
        varidx=append(varidx,tree[[treeI]][[paste("node",idx,sep = "")]]$Var)
        ruleidx=append(ruleidx,tree[[treeI]][[paste("node",idx,sep = "")]]$Rule)
        stp=1
      }
      
    }
    
  }
  list(varidx=varidx,ruleidx=ruleidx,LeftorRight=LeftorRight)
}


TopPath=function(tree,treeI,idx) # input is Top_id
{
  varidx=c()
  ruleidx=c()
  node_id=c()
  
  if(tree[[treeI]][[paste("node",idx,sep = "")]]$Bot)
  {
    
    varidx=c()
    ruleidx=c()
    node_id=c()
    
  }else if(tree[[treeI]][[paste("node",idx,sep = "")]]$Nog)
  {
    
    varidx=tree[[treeI]][[paste("node",idx,sep = "")]]$Var
    ruleidx=tree[[treeI]][[paste("node",idx,sep = "")]]$Rule
    node_id=idx
    
  }else{
    
    varidx=c(tree[[treeI]][[paste("node",idx,sep = "")]]$Var,TopPath(tree,treeI,2*idx)$varidx,TopPath(tree,treeI,2*idx+1)$varidx)
    ruleidx=c(tree[[treeI]][[paste("node",idx,sep = "")]]$Rule,TopPath(tree,treeI,2*idx)$ruleidx,TopPath(tree,treeI,2*idx+1)$ruleidx)
    node_id=c(idx,TopPath(tree,treeI,2*idx)$node_id,TopPath(tree,treeI,2*idx+1)$node_id)
  }
  
  list(node_id=node_id,varidx=varidx,ruleidx=ruleidx)
}


## DrPriVar:returns index of variable to split on
DrPriVar=function(tree,treeI,idx)
{
  SumGoodVar=sum(tree[[treeI]][[paste("node",idx,sep = "")]]$VarAvail)  # SumGoodVar
  varpossible=floor(runif(1)*SumGoodVar)+1
  varidx=which(tree[[treeI]][[paste("node",idx,sep = "")]]$VarAvail==1)[varpossible]
  
  return(varidx)
}


DrPriVar_Dirichlet=function(s)
{
  # stp=0
  # while(stp==0)
  # {
  #   var_select=which(rmultinom(1,1,s)==1)
  #   if(tree[[treeI]][[paste("node",idx,sep = "")]]$VarAvail[var_select]==1)
  #   {
  #     stp=1
  #     return(var_select)
  #   }
  # }
  
  var_select=which(rmultinom(1,1,s)==1)
  return(var_select)
  
}

SplitRule_Bad=function(tree,treeI,varid,idx)
{
  stp=0
  while(stp==0)
  {
    idx_p=tree[[treeI]][[paste("node",idx,sep = "")]]$Parent
    if(tree[[treeI]][[paste("node",idx_p,sep = "")]]$Var==varid)
    {
      stp=1
      cat("stp",stp,"\n")
      cat("rule",tree[[treeI]][[paste("node",idx_p,sep = "")]]$Rule,"\n")
      
    }else{
      
      idx=idx_p
    }
  }
  
  
  list(LeftEx=1,RightEx=1,Rule=tree[[treeI]][[paste("node",idx_p,sep = "")]]$Rule)
  
  
}


SplitRule=function(tree,treeI,idx,x,varidx,VarType,RuleNum,Rulemat)
{
  
  LeftEx=0
  RightEx=0
  
  if(VarType[varidx]=="CAT")
  {
    
    get_cats=unique(x[tree[[treeI]][[paste("node",idx,sep = "")]]$DataList,varidx])
    Ncat=length(get_cats)
    
    index=c()
    sel=rep(0,Ncat)
    for(i in 1:Ncat)
    {
      index[i]=which(Rulemat[[varidx]]==get_cats[i])
    }
    
    rule=rep(0,RuleNum[varidx])
    
    sel[1]=1
    if(Ncat>2)
    {
      aa=sample(2:Ncat,1,replace = F)
      sel[aa]=0
      sel[-c(1,aa)]=sample(c(0,1),Ncat-2,replace=T)
      
    }else{
      
      sel[2]=0
    }
    rule[index] = sel
    
    
    if(Ncat-sum(sel)==1)
    {
      LeftEx=1
    }
    if(sum(sel)==1)
    {
      RightEx=1
    }
    
    list(LeftEx=LeftEx,RightEx=RightEx,Rule=rule)
    
  }else{
    
    x_min=min(x[tree[[treeI]][[paste("node",idx,sep = "")]]$DataList,varidx])
    x_max=max(x[tree[[treeI]][[paste("node",idx,sep = "")]]$DataList,varidx])
    
    LeftI=min(which(Rulemat[[varidx]]>=x_min))
    RightI=max(which(Rulemat[[varidx]]<x_max))
    
    numsplit=RightI-LeftI+1
    tree[[treeI]][[paste("node",idx,sep = "")]]$Rule=LeftI+floor(runif(1)*numsplit)
    
    if(tree[[treeI]][[paste("node",idx,sep = "")]]$Rule==LeftI)
    {
      LeftEx=1
    }
    
    if(tree[[treeI]][[paste("node",idx,sep = "")]]$Rule==RightI)
    {
      RightEx=1
    }
    
    list(LeftEx=LeftEx,RightEx=RightEx,Rule=tree[[treeI]][[paste("node",idx,sep = "")]]$Rule)
    
  }
}


SplitRule_change=function(tree,treeI,idx,varidx,x,VarType,Rulemat)
{
  
  LeftEx=0
  RightEx=0
  
  if(VarType[varidx]=="CAT")
  {
    get_cats=unique(x[tree[[treeI]][[paste("node",idx,sep = "")]]$DataList,varidx])
    Ncat=length(get_cats)
    
    if(Ncat-sum(tree[[treeI]][[paste("node",idx,sep = "")]]$Rule)==1)
    {
      LeftEx=1
    }
    if(sum(tree[[treeI]][[paste("node",idx,sep = "")]]$Rule)==1)
    {
      RightEx=1
    }
    
    list(LeftEx=LeftEx,RightEx=RightEx,Rule=tree[[treeI]][[paste("node",idx,sep = "")]]$Rule)
    
  }else{
    
    x_min=min(x[tree[[treeI]][[paste("node",idx,sep = "")]]$DataList,varidx])
    x_max=max(x[tree[[treeI]][[paste("node",idx,sep = "")]]$DataList,varidx])
    
    LeftI=min(which(Rulemat[[varidx]]>=x_min))
    RightI=max(which(Rulemat[[varidx]]<x_max))
    
    if(tree[[treeI]][[paste("node",idx,sep = "")]]$Rule==LeftI)
    {
      LeftEx=1
    }
    
    if(tree[[treeI]][[paste("node",idx,sep = "")]]$Rule==RightI)
    {
      RightEx=1
    }
    
    list(LeftEx=LeftEx,RightEx=RightEx,Rule=tree[[treeI]][[paste("node",idx,sep = "")]]$Rule)
    
  }
  
}

## SpawnChildren: create a new node   
SpawnChildren=function(tree,treeI,idx,LeftEx,RightEx,x,VarType,RuleNum,Rulemat)
{
  tree[[treeI]][[paste("node",idx,sep = "")]]$Bot = 0
  tree[[treeI]][[paste("node",idx,sep = "")]]$Nog = 1
  if(!tree[[treeI]][[paste("node",idx,sep = "")]]$Top)
  {
    idx_p=tree[[treeI]][[paste("node",idx,sep = "")]]$Parent
    tree[[treeI]][[paste("node",idx_p,sep = "")]]$Nog = 0
  }
  
  tree[[treeI]][[paste("node",idx,sep = "")]]$LeftC = 2*idx
  tree[[treeI]][[paste("node",idx,sep = "")]]$RightC = 2*idx+1
  
  # Left Child
  tree[[treeI]][[paste("node",2*idx,sep = "")]]$Top=0
  tree[[treeI]][[paste("node",2*idx,sep = "")]]$Bot=1
  tree[[treeI]][[paste("node",2*idx,sep = "")]]$Nog=0
  tree[[treeI]][[paste("node",2*idx,sep = "")]]$Parent=idx
  
  
  tree[[treeI]][[paste("node",2*idx,sep = "")]]$VarAvail=tree[[treeI]][[paste("node",idx,sep = "")]]$VarAvail
  if(LeftEx)
  {
    
    tree[[treeI]][[paste("node",2*idx,sep = "")]]$VarAvail[tree[[treeI]][[paste("node",idx,sep = "")]]$Var]=0
  }
  
  # Right Child
  tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$Top=0
  tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$Bot=1
  tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$Nog=0
  tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$Parent=idx
  
  tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$VarAvail=tree[[treeI]][[paste("node",idx,sep = "")]]$VarAvail # still something wrong
  if(RightEx)
  {
    tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$VarAvail[tree[[treeI]][[paste("node",idx,sep = "")]]$Var]=0
  }
  
  
  for (i in 1:length(tree[[treeI]][[paste("node",idx,sep = "")]]$DataList))
  {
    tree=SetData(tree,treeI,idx,tree[[treeI]][[paste("node",idx,sep = "")]]$DataList[i],x,VarType,RuleNum,Rulemat)
  }
  
  
  return(tree)
  
}

# Setdata
# SpawnChildrenData=function(tree,treeI,idx,x,VarType,RuleNum,Rulemat)
# {
#   for (i in 1:length(tree[[treeI]][[paste("node",idx,sep = "")]]$DataList))
#   {
#     
#     tree=SetData(tree,treeI,idx,tree[[treeI]][[paste("node",idx,sep = "")]]$DataList[i],x,VarType,RuleNum,Rulemat)
#   }
#   
#   return(tree)
# }


## DrBotNode: draw from the set of bottom nodes such that a birth is possible, returns 0 if no bots can grow, 1 otherwise
DrBotNode=function(tree,treeI,idx,x,RuleNum,Rulemat)  # 
{
  
  bottom_node=BotNodes(tree,treeI,idx)
  nbot=bottom_node$BotNumber
  botidx=bottom_node$BotIdx
  
  
  if(is.null(botidx))
  {
    can=0
    nprob=0
    idx_select=0
    
  }else{
    
    probs=c()
    rule_sum=c()
    for(i in 1:nbot)
    {
      probs[i]=ifelse(sum(tree[[treeI]][[paste("node",botidx[i],sep = "")]]$VarAvail),1,0)
      rule_sum[i]=probs[i]
      
      if(probs[i]==1)
      {
        
        VarI=which(tree[[treeI]][[paste("node",botidx[i],sep = "")]]$VarAvail==1)
        rule_can=c()
        
        for (j in 1:length(VarI))
        {
          if(VarType[VarI[j]]=="ORD")
          {
            x_min=min(x[tree[[treeI]][[paste("node",botidx[i],sep = "")]]$DataList,VarI[j]])
            x_max=max(x[tree[[treeI]][[paste("node",botidx[i],sep = "")]]$DataList,VarI[j]])
            
            cut_avail=0
            for(c in 1:RuleNum[VarI[j]])
            {
              if(Rulemat[[VarI[j]]][c]<x_max & Rulemat[[VarI[j]]][c]>=x_min)
              {
                cut_avail=cut_avail+1
              }
            }
            rule_can[j]=ifelse(cut_avail>=1,1,0)
            tree[[treeI]][[paste("node",botidx[i],sep = "")]]$VarAvail[VarI[j]] = rule_can[j]  ## update VarAvail information for botidx[i]
            
          }else{
            
            get_cats=unique(x[tree[[treeI]][[paste("node",botidx[i],sep = "")]]$DataList,VarI[j]])
            rule_can[j]=ifelse(length(get_cats)>1,1,0)
            tree[[treeI]][[paste("node",botidx[i],sep = "")]]$VarAvail[VarI[j]]=rule_can[j]
          }
          
        }
        
        rule_sum[i]=ifelse(sum(rule_can),1,0)
      }else
      {
        rule_sum[i]=0
      }
      
      
    }
    sum=sum(rule_sum)
    
    if(sum>0)
    {
      
      can=1
      probs=rule_sum/sum
      
      # select one of the bottom node
      u=runif(1)
      s=probs[1]
      i=1
      while(s<u)
      {
        i=i+1
        s=s+probs[i]
      }
      
      idx_select=botidx[i]
      nprob=probs[i]    # prob of drawn bottom node
      
    }else {
      can=0
      nprob=0
      idx_select=0
    }
  }
  
  list(tree=tree,can=can,idx_select=idx_select,nprob=nprob)
}

##PrBotNode:prob of drawing the bottom node as a birth node
PrBotNode=function(tree,treeI,idx,index)  # idx is top node, index is the bottoon node
{
  
  bottom_node=BotNodes(tree,treeI,idx)
  nbot=bottom_node$BotNumber
  botidx=bottom_node$BotIdx
  
  probs=c()
  for(i in 1:nbot)
  {
    probs[i]=ifelse(sum(tree[[treeI]][[paste("node",botidx[i],sep = "")]]$VarAvail),1,0)
  }
  sum=sum(probs)
  
  probs=probs/sum
  
  PrNode=probs[which(botidx==index)]
  
  return(PrNode)
}


## DrNogNode: returns prob of a nog node
DrNogNode=function(tree,treeI,idx)
{
  
  nog_node=NogNodes(tree,treeI,idx)
  nnog=nog_node$NogNumber
  nogidx=nog_node$NogIdx
  
  r=runif(1)
  NodeI=floor(r*nnog)+1
  
  idx_select=nogidx[NodeI]
  nprob=1/nnog
  
  list(idx_select=idx_select,nprob=nprob)
  
}

## PBirth: returns prob of a birth step, used in BirthDeath
PBirth=function(tree,treeI,idx,x,RuleNum,Rulemat)
{
  can=DrBotNode(tree,treeI,idx,x,RuleNum,Rulemat)$can
  if(!can)
  {
    PB=0
  }else if(length(tree[[treeI]])==1)
  {
    PB=1
  }else
  {
    PB=0.5
  }
  
  return(PB)
}

## PGrow: prob of a node to be split
PGrow=function(tree,treeI,idx)
{
  alpha=0.95
  beta=2
  SumGoodVar=sum(tree[[treeI]][[paste("node",idx,sep = "")]]$VarAvail)
  if(SumGoodVar)
  {
    nobs=length(tree[[treeI]][[paste("node",idx,sep = "")]]$DataList)
    if(nobs<3) {
      
      return (.001*alpha/((1.0+Depth(tree,treeI,idx))^beta))
      
    }else {
      
      return (alpha/((1.0+Depth(tree,treeI,idx))^beta))
    }
  } else {
    return (0)
  }
}

## KillChildren   
KillChildren=function(tree,treeI,idx)
{
  tree[[treeI]][[paste("node",2*idx,sep = "")]]=NULL
  tree[[treeI]][[paste("node",2*idx+1,sep = "")]]=NULL
  
  tree[[treeI]][[paste("node",idx,sep = "")]]$Bot = 1
  tree[[treeI]][[paste("node",idx,sep = "")]]$Nog = 0
  
  if(!tree[[treeI]][[paste("node",idx,sep = "")]]$Top)
  {
    idx_p=tree[[treeI]][[paste("node",idx,sep = "")]]$Parent
    if(idx==tree[[treeI]][[paste("node",idx_p,sep = "")]]$LeftC)
    {
      if(tree[[treeI]][[paste("node",idx+1,sep = "")]]$Bot)
      {
        tree[[treeI]][[paste("node",idx_p,sep = "")]]$Nog = 1
      }
    }else{
      if(tree[[treeI]][[paste("node",idx-1,sep = "")]]$Bot)
      {
        tree[[treeI]][[paste("node",idx_p,sep = "")]]$Nog = 1
      }
    }
  }
  
  return(tree)
}


### NumNogNodes
NumNogNodes=function(tree,treeI,idx)
{
  if(tree[[treeI]][[paste("node",idx,sep = "")]]$Bot) 
  {
    return (0)
    
  }else if(tree[[treeI]][[paste("node",idx,sep = "")]]$Nog) 
  {
    return(1)
    
  }else
  {
    return (NumNogNodes(tree,treeI,tree[[treeI]][[paste("node",idx,sep = "")]]$LeftC) + NumNogNodes(tree,treeI,tree[[treeI]][[paste("node",idx,sep = "")]]$RightC))
  }
}

## BirthDeath: does either a birth or death step
BirthDeath=function(tree,treeI,YDat,Nnode,x,VarType,RuleNum,Rulemat,Top_idx,param_random,nbeta,nseg_time_temp,nus,Gs,opt)
{
  
  PBx=PBirth(tree,treeI,Top_idx,x,RuleNum,Rulemat)
  r=runif(1)
  n=Nnode
  if(r<PBx)
  {
    BD=1
    
    dr_bot_node=DrBotNode(tree,treeI,Top_idx,x,RuleNum,Rulemat)
    tree=dr_bot_node$tree
    idx=dr_bot_node$idx_select
    Pbot=dr_bot_node$nprob
    
    PGn = PGrow(tree,treeI,idx)
    VarI = DrPriVar(tree,treeI,idx)
    rule=SplitRule(tree,treeI,idx,x,VarI,VarType,RuleNum,Rulemat) # draw rule
      
      
    tree[[treeI]][[paste("node",idx,sep = "")]]$Var=VarI
    tree[[treeI]][[paste("node",idx,sep = "")]]$Rule=rule$Rule
      
      tree=SpawnChildren(tree,treeI,idx,rule$LeftEx,rule$RightEx,x,VarType,RuleNum,Rulemat) # create children
     
      n=n+2
      
      PGl=PGrow(tree,treeI,2*idx)
      PGr=PGrow(tree,treeI,2*idx+1)
      
      Pnog=1/(NogNodes(tree,treeI,Top_idx)$NogNumber)
      PDy = 1.0-PBirth(tree,treeI,Top_idx,x,RuleNum,Rulemat)
      
      # PDy: P(PRUNE)
      # Pnog: 1/w2*: 1/number of Nog nodes
      # PGn: P(SPLIT)
      # Pbot: 1/b: 1/number of bottom nodes
      alpha1 = (PGn*(1.0-PGl)*(1.0-PGr)*PDy*Pnog)/((1.0-PGn)*PBx*Pbot) # transition ratio * tree structure ratio
      
      # Drawing new tausq
      tau_prop=rep(1,2)
      g_prop=rep(1,2)
      
      zz=param_random+runif(1)*(1-2*param_random)
      tau_prop[1]=tree[[treeI]][[paste("node",idx,sep = "")]]$tau*(zz/(1-zz))
      tau_prop[2]=tree[[treeI]][[paste("node",idx,sep = "")]]$tau*((1-zz)/zz)
      
      tt=param_random+runif(1)*(1-2*param_random)
      g_prop[1]=tree[[treeI]][[paste("node",idx,sep = "")]]$g*(tt/(1-tt))
      g_prop[2]=tree[[treeI]][[paste("node",idx,sep = "")]]$g*((1-tt)/tt)
      
      
      loglike_prop=0
      log_beta_prop=0
      log_beta_prior_prop=0
      log_tau_prior_prop=0
      log_g_prior_prop=0
      
      nfreq <- floor(nseg_time_temp/2)
      fhat=matrix(0,nfreq+1,2)
      
      # Drawing a new value of beta 
      # Evaluating the Likelihood, Proposal and Prior Densities at the Proposed values
      
      beta_prop=matrix(0,nbeta,2)
      jj=0
      for (k in (2*idx):(2*idx+1))
      {
        
        jj=jj+1
        postbeta_1=postbeta(tree,treeI,k,nseg_time_temp,tau_prop[jj],YDat,nbeta,opt)
        beta_prop[,jj]=rmvnorm(1,postbeta_1$beta_mean,0.5*(postbeta_1$beta_var+t(postbeta_1$beta_var)))
        
        log_beta_prop=log_beta_prop+dmvnorm(beta_prop[,jj],postbeta_1$beta_mean,0.5*(postbeta_1$beta_var+t(postbeta_1$beta_var)),log = T)
        log_beta_prior_prop=log_beta_prior_prop+dmvnorm(beta_prop[,jj],matrix(0,nbeta,1),diag(c(opt$sigmasqalpha, tau_prop[jj]*matrix(1,opt$nbasis,1))),log = T) # Prior Density of beta
        log_tau_prior_prop=log_tau_prior_prop+log(dgamma(1/tau_prop[jj],nus/2,scale=g_prop[jj]/nus)) # Prior Density of Tausq
        log_g_prior_prop=log_g_prior_prop+log(dgamma(1/g_prop[jj],1/2,scale=Gs^2)) # Prior Density of g
        fhat[,jj]=postbeta_1$nu_mat%*%beta_prop[,jj]
        log_prop_spec_dens=whittle_like(postbeta_1$y,fhat[,jj],nseg_time_temp) # Loglikelihood  at proposed values
        loglike_prop=loglike_prop+log_prop_spec_dens
        
      }
      
      # Calculating Jacobian
      log_jacobian=log(2*tree[[treeI]][[paste("node",idx,sep = "")]]$tau/(zz*(1-zz)))+log(2*tree[[treeI]][[paste("node",idx,sep = "")]]$g/(tt*(1-tt)))
      
      # Calculating log proposal density at proposed values q(T*,M* | T,M)
      log_proposal_prop = log_beta_prop
      
      # Calculating log prior density at proposed values
      log_prior_prop = log_beta_prior_prop+log_tau_prior_prop+log_g_prior_prop
      
      # Calculating target density at proposed values
      log_target_prop=loglike_prop+log_prior_prop		
      
      
      # CURRENT VALUES		
      # Evaluating the Likelihood, Proposal and Prior Densities at the Current values
      
      loglike_curr=0
      log_beta_curr=0
      log_beta_prior_curr=0
      log_tau_prior_curr=0
      log_g_prior_curr=0
      
      # Beta proposal and prior
      
      postbeta_3=postbeta(tree,treeI,idx,nseg_time_temp,tree[[treeI]][[paste("node",idx,sep = "")]]$tau,YDat,nbeta,opt)
      log_beta_curr=log_beta_curr+dmvnorm(tree[[treeI]][[paste("node",idx,sep = "")]]$beta,postbeta_3$beta_mean,0.5*(postbeta_3$beta_var+t(postbeta_3$beta_var)),log = T)
      log_beta_prior_curr=log_beta_prior_curr+dmvnorm(tree[[treeI]][[paste("node",idx,sep = "")]]$beta,matrix(0,nbeta,1),diag(c(opt$sigmasqalpha, tree[[treeI]][[paste("node",idx,sep = "")]]$tau*matrix(1,opt$nbasis,1))),log=T)
      log_tau_prior_curr=log_tau_prior_curr+log(dgamma(1/tree[[treeI]][[paste("node",idx,sep = "")]]$tau,nus/2,scale=tree[[treeI]][[paste("node",idx,sep = "")]]$g/nus))
      log_g_prior_curr=log_g_prior_curr+log(dgamma(1/tree[[treeI]][[paste("node",idx,sep = "")]]$g,1/2,scale=Gs^2))
      
      # Log likelihood  at current values
      fhat_curr=postbeta_3$nu_mat%*%matrix(tree[[treeI]][[paste("node",idx,sep = "")]]$beta,nbeta,1)
      log_curr_spec_dens=whittle_like(postbeta_3$y,fhat_curr,nseg_time_temp)
      loglike_curr=loglike_curr+log_curr_spec_dens
      
      # Calculating log proposal density at current values
      log_proposal_curr=log_beta_curr
      
      # Calculating priors at current values
      log_prior_curr=log_beta_prior_curr+log_tau_prior_curr+log_g_prior_curr
      
      # Evaluating target densities at current values
      log_target_curr=loglike_curr+log_prior_curr
      
      alpha=min(1,exp(log_target_prop-log_target_curr+log_proposal_curr-log_proposal_prop+log_jacobian)*alpha1)
      
      r1=runif(1)
      if(r1<alpha)
      {
        done=1
        tree[[treeI]][[paste("node",2*idx,sep = "")]]$tau=tau_prop[1]
        tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$tau=tau_prop[2]
        
        tree[[treeI]][[paste("node",2*idx,sep = "")]]$g=g_prop[1]
        tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$g=g_prop[2]
        
        tree[[treeI]][[paste("node",2*idx,sep = "")]]$beta=beta_prop[,1]
        tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$beta=beta_prop[,2]
        
        tree[[treeI]][[paste("node",2*idx,sep = "")]]$fhat=fhat[,1]
        tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$fhat=fhat[,2]
        
        
        list(tree=tree,alpha=alpha,BD=BD,done=done,Nnode=n,idx=idx,Var=VarI,cutpoint=rule$Rule,taul=tree[[treeI]][[paste("node",2*idx,sep = "")]]$tau,taur=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$tau,gl=tree[[treeI]][[paste("node",2*idx,sep = "")]]$g,gr=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$g)
        
      }else
      {
        n=n-2
        tree=KillChildren(tree,treeI,idx)
        tree[[treeI]][[paste("node",idx,sep = "")]]$fhat=as.vector(fhat_curr)
        
        done=0
        
        list(tree=tree,alpha=alpha,BD=BD,done=done,Nnode=n,idx=idx,Var=VarI,cutpoint=rule$Rule,tau=tree[[treeI]][[paste("node",idx,sep = "")]]$tau,g=tree[[treeI]][[paste("node",idx,sep = "")]]$g)
        
      }
      
  }else
  {
    BD=0
    
    PDx=1-PBx
    
    dr_nog_node=DrNogNode(tree,treeI,Top_idx)
    Pnog=dr_nog_node$nprob  # same as 1/(NogNodes(n)$NogNumber)?
    idx=dr_nog_node$idx_select
    
    PGl = PGrow(tree,treeI,2*idx)
    PGr = PGrow(tree,treeI,2*idx+1)
    
    #Copy rule
    var_copy=tree[[treeI]][[paste("node",idx,sep = "")]]$Var
    rule_copy=tree[[treeI]][[paste("node",idx,sep = "")]]$Rule
    betaL=tree[[treeI]][[paste("node",2*idx,sep = "")]]$beta
    betaR=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$beta
    tauL=tree[[treeI]][[paste("node",2*idx,sep = "")]]$tau
    tauR=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$tau
    gL=tree[[treeI]][[paste("node",2*idx,sep = "")]]$g
    gR=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$g
    fhatL=tree[[treeI]][[paste("node",2*idx,sep = "")]]$fhat
    fhatR=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$fhat
    
    
    Left_ex=1-tree[[treeI]][[paste("node",2*idx,sep = "")]]$VarAvail[tree[[treeI]][[paste("node",idx,sep = "")]]$Var]
    Right_ex=1-tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$VarAvail[tree[[treeI]][[paste("node",idx,sep = "")]]$Var]
    
    # CURRENT VALUES
    # Evaluating the Likelihood, Proposal and Prior Densities at the Current values
    # Beta proposal and prior
    loglike_curr=0
    log_beta_curr=0
    log_beta_prior_curr=0
    log_tau_prior_curr=0
    log_g_prior_curr=0
    
    nfreq <- floor(nseg_time_temp/2)
    fhat=matrix(0,nfreq+1,2)
    
    jj=0
    for (k in (2*idx):(2*idx+1))
    {
      jj=jj+1
      
      postbeta_5=postbeta(tree,treeI,k,nseg_time_temp,tree[[treeI]][[paste("node",k,sep = "")]]$tau,YDat,nbeta,opt)
      log_beta_curr=log_beta_curr+dmvnorm(tree[[treeI]][[paste("node",k,sep = "")]]$beta,postbeta_5$beta_mean,0.5*(postbeta_5$beta_var+t(postbeta_5$beta_var)),log = T)
      log_beta_prior_curr=log_beta_prior_curr+dmvnorm(tree[[treeI]][[paste("node",k,sep = "")]]$beta,matrix(0,nbeta,1),diag(c(opt$sigmasqalpha, tree[[treeI]][[paste("node",k,sep = "")]]$tau*matrix(1,opt$nbasis,1))),log = T)
      log_tau_prior_curr=log_tau_prior_curr+log(dgamma(1/tree[[treeI]][[paste("node",k,sep = "")]]$tau,nus/2,scale=tree[[treeI]][[paste("node",k,sep = "")]]$g/nus))
      log_g_prior_curr=log_g_prior_curr+log(dgamma(1/tree[[treeI]][[paste("node",k,sep = "")]]$g,1/2,scale=Gs^2))
      
      # Loglikelihood  at current values
      fhat[,jj]=postbeta_5$nu_mat%*%matrix(tree[[treeI]][[paste("node",k,sep = "")]]$beta,nbeta,1)
      log_curr_spec_dens=whittle_like(postbeta_5$y,fhat[,jj],nseg_time_temp)
      loglike_curr=loglike_curr+log_curr_spec_dens
      
    }
    
    # Calculating log proposal density at current values
    log_proposal_curr=log_beta_curr
    
    # Calculating priors at current values
    log_prior_curr=log_beta_prior_curr+log_tau_prior_curr+log_g_prior_curr
    
    # Evaluating Target density at current values(taking duplicates into consideration)
    log_target_curr=loglike_curr+log_prior_curr
    
    #### PROPOSAL VALUES
    tau_prop=sqrt(tree[[treeI]][[paste("node",2*idx,sep = "")]]$tau*tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$tau) # Combining 2 taus into 1
    g_prop=sqrt(tree[[treeI]][[paste("node",2*idx,sep = "")]]$g*tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$g) # Combining 2 taus into 1
    
    # Evaluating the Likelihood, Proposal and Prior Densities at the Proposed values
    
    loglike_prop=0
    log_beta_prop=0
    log_beta_prior_prop=0
    log_tau_prior_prop=0
    log_g_prior_prop=0
    
    # Computing mean and variances for beta proposals
    
    postbeta_4=postbeta(tree,treeI,idx,nseg_time_temp,tau_prop,YDat,nbeta,opt)
    beta_prop=rmvnorm(1,postbeta_4$beta_mean,0.5*(postbeta_4$beta_var+t(postbeta_4$beta_var))) # Drawing a new value of beta
    
    # Loglikelihood  at proposed values
    fhat=postbeta_4$nu_mat%*%matrix(beta_prop,nbeta,1)
    log_prop_spec_dens=whittle_like(postbeta_4$y,fhat,nseg_time_temp)
    loglike_prop=loglike_prop+log_prop_spec_dens
    
    # Evaluating the Prior Densities at the Proposed values for tau and
    
    # Beta
    log_beta_prior_prop=log_beta_prior_prop+dmvnorm(beta_prop,matrix(0,nbeta,1),diag(c(opt$sigmasqalpha, tau_prop*matrix(1,opt$nbasis,1))), log = T)
    log_tau_prior_prop=log_tau_prior_prop+log(dgamma(1/tau_prop,nus/2,scale=g_prop/nus))
    log_g_prior_prop=log_g_prior_prop+log(dgamma(1/tau_prop,1/2,scale=Gs^2))
    
    # Evaluating the Proposal Densities at the Proposed values of beta
    
    log_beta_prop=log_beta_prop+dmvnorm(beta_prop,postbeta_4$beta_mean,0.5*(postbeta_4$beta_var+t(postbeta_4$beta_var)),log = T)
    
    # Calculating Jacobian(sum of log Jacobian for each covariate seg)
    log_jacobian_tau=-log(2*(sqrt(tree[[treeI]][[paste("node",2*idx,sep = "")]]$tau)+sqrt(tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$tau))^2)
    log_jacobian_g=-log(2*(sqrt(tree[[treeI]][[paste("node",2*idx,sep = "")]]$g)+sqrt(tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$g))^2)
    log_jacobian=log_jacobian_tau+log_jacobian_g
    
    # Calculating log proposal density at proposed values
    log_proposal_prop=log_beta_prop
    
    # Calculating log prior density at proposed values
    log_prior_prop=log_beta_prior_prop+log_tau_prior_prop+log_g_prior_prop
    
    # Evaluating Target density at proposed values(taking duplicates into consideration)
    log_target_prop=loglike_prop+log_prior_prop
    
    
    tree=KillChildren(tree,treeI,idx)
    
    n=n-2
    PBy=PBirth(tree,treeI,Top_idx,x,RuleNum,Rulemat)
    PGn=PGrow(tree,treeI,idx)
    Pbot=PrBotNode(tree,treeI,Top_idx,idx)
    alpha1 =((1.0-PGn)*PBy*Pbot)/(PGn*(1.0-PGl)*(1.0-PGr)*PDx*Pnog)
    
    alpha=min(1,exp(log_target_prop-log_target_curr+log_proposal_curr-log_proposal_prop+log_jacobian)*alpha1)
    
    r2=runif(1)
    if(r2<alpha)
    {
      done=1
      tree[[treeI]][[paste("node",idx,sep = "")]]$tau=tau_prop
      tree[[treeI]][[paste("node",idx,sep = "")]]$g=g_prop
      tree[[treeI]][[paste("node",idx,sep = "")]]$beta=beta_prop
      tree[[treeI]][[paste("node",idx,sep = "")]]$fhat=as.vector(fhat)
      
      list(tree=tree,alpha=alpha,BD=BD,done=done,Nnode=n,idx=idx,tau=tree[[treeI]][[paste("node",idx,sep = "")]]$tau,g=tree[[treeI]][[paste("node",idx,sep = "")]]$g)
      
    }else
    {
      n=n+2
      tree[[treeI]][[paste("node",idx,sep = "")]]$Var = var_copy
      tree[[treeI]][[paste("node",idx,sep = "")]]$Rule = rule_copy
      
      tree=SpawnChildren(tree,treeI,idx,Left_ex,Right_ex,x,VarType,RuleNum,Rulemat)
      
      tree[[treeI]][[paste("node",2*idx,sep = "")]]$tau=tauL
      tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$tau=tauR
      tree[[treeI]][[paste("node",2*idx,sep = "")]]$g=gL
      tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$g=gR
      tree[[treeI]][[paste("node",2*idx,sep = "")]]$beta=betaL
      tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$beta=betaR
      tree[[treeI]][[paste("node",2*idx,sep = "")]]$fhat=fhatL
      tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$fhat=fhatR
      
      done=0
      
      list(tree=tree,alpha=alpha,BD=BD,done=done,Nnode=n,idx=idx,taul=tree[[treeI]][[paste("node",2*idx,sep = "")]]$tau,taur=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$tau,gl=tree[[treeI]][[paste("node",2*idx,sep = "")]]$g,gr=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$g)
      
    }
    
    
  }
  
}

## FindNode: gets index of bottom node for x
## i is the index of observation
FindNode=function(tree,i,idx,treeI,x,VarType,RuleNum,Rulemat) # x is the ith observation we have
{
  if(tree[[treeI]][[paste("node",idx,sep = "")]]$Bot)
  {
    return(idx)
    
  }else
  {
    
    if(Right(tree,treeI,idx,x[i,],tree[[treeI]][[paste("node",idx,sep = "")]]$Var,tree[[treeI]][[paste("node",idx,sep = "")]]$Rule,VarType,RuleNum,Rulemat))
    {
      FindNode(tree,i,tree[[treeI]][[paste("node",idx,sep = "")]]$RightC,treeI,x,VarType,RuleNum,Rulemat)
      
    }else{
      
      FindNode(tree,i,tree[[treeI]][[paste("node",idx,sep = "")]]$LeftC,treeI,x,VarType,RuleNum,Rulemat)
    }
  }
}


NotBotNodes=function(tree,treeI,idx)
{
  sum=0
  index=c()
  if(tree[[treeI]][[paste("node",idx,sep = "")]]$Bot)
  {
    sum=0
    index=c()
  }else if(tree[[treeI]][[paste("node",idx,sep = "")]]$Nog)
  {
    
    index=append(index,idx)
    sum=tree[[treeI]][[paste("node",idx,sep = "")]]$Nog
    
  }else{
    
    sum=1+NotBotNodes(tree,treeI,2*idx)$NotBotNumber+NotBotNodes(tree,treeI,2*idx+1)$NotBotNumber
    index=append(idx,c(NotBotNodes(tree,treeI,2*idx)$NotBotIdx,NotBotNodes(tree,treeI,2*idx+1)$NotBotIdx))
  }
  
  list(NotBotNumber=sum,NotBotIdx=index)
}


SplitRule_smallmove=function(tree,treeI,idx,varidx,x,VarType,Rulemat)
{
  
  LeftEx=0
  RightEx=0
  rule=tree[[treeI]][[paste("node",idx,sep = "")]]$Rule
  
  if(VarType[varidx]=="CAT")
  {
    
    get_cats=unique(x[tree[[treeI]][[paste("node",idx,sep = "")]]$DataList,varidx])
    Ncat=length(get_cats)
    index_cats=c()
    if(Ncat==2)
    {
      rule_new=rule
      
    }else if(Ncat-sum(rule)==1)
    {
      r=runif(1)
      if(r<0.5)
      {
        index=sample(which(rule==1),1,replace=F)
        rule[index]=0
        rule_new=rule
        
      }else{
        rule_new=rule
      }
      
    }else if(sum(rule)==1)
    {
      r=runif(1)
      
      if(r<0.5)
      {
        
        for(i in 1:Ncat)
        {
          index_cats[i]=which(Rulemat[[varidx]]==get_cats[i])
        }
        
        index=sample(which(rule[index_cats]==0),1,replace=F)
        rule[index]=1
        rule_new=rule
        
      }else{
        
        rule_new=rule
      }
      
    }else{
      r=runif(1)
      if(r<0.3){
        
        index=sample(which(rule==1),1,replace=F)
        rule[index]=0
        rule_new=rule
        
      }else if(r>=0.3 && r<0.6)
      {
        for(i in 1:Ncat)
        {
          index_cats[i]=which(Rulemat[[varidx]]==get_cats[i])
        }
        
        index=sample(which(rule[index_cats]==0),1,replace=F)
        rule[index]=1
        rule_new=rule
        
      }else{
        
        rule_new=rule
      }
    }
    
    if(Ncat-sum(rule_new)==1)
    {
      LeftEx=1
    }
    if(sum(rule_new)==1)
    {
      RightEx=1
    }
    
    list(LeftEx=LeftEx,RightEx=RightEx,Rule=rule_new)
    
  }else{
    
    
    x_min=min(x[tree[[treeI]][[paste("node",idx,sep = "")]]$DataList,varidx])
    x_max=max(x[tree[[treeI]][[paste("node",idx,sep = "")]]$DataList,varidx])
    
    
    LeftI=min(which(Rulemat[[varidx]]>=x_min))
    RightI=max(which(Rulemat[[varidx]]<x_max))
    
    if(RightI-LeftI==0)
    {
      rule_new=rule
    }else if(rule==LeftI)
    {
      nposs=2  # Number of possible locations for new cutpoint
      new_index=sample(1:nposs,1,replace = T)  # Drawing index of new cutpoint
      rule_new=rule-1+new_index
    }else if(rule==RightI)
    {
      nposs=2  # Number of possible locations for new cutpoint
      new_index=sample(1:nposs,1,replace = T)  # Drawing index of new cutpoint
      rule_new=rule+1-new_index
      
    }else{
      
      nposs=3 #  Number of possible locations for new cutpoint
      new_index=sample(1:nposs,1,replace = T)  # Drawing index of new cutpoint
      rule_new=rule-2+new_index
      
    }
    
    
    if(rule_new==LeftI)
    {
      LeftEx=1
    }
    
    if(rule_new==RightI)
    {
      RightEx=1
    }
    
    #tree[[treeI]][[paste("node",idx,sep = "")]]$Var=varidx
    list(LeftEx=LeftEx,RightEx=RightEx,Rule=rule_new)
    
  }
  
  
}

ChangeRule=function(tree,treeI,YDat,Nnode,x,VarType,RuleNum,Rulemat,Top_idx,param_random,nbeta,nseg_time_temp,nus,Gs,opt)
{
  
  ## Select idx to change
  dr_nog_node=DrNogNode(tree,treeI,Top_idx)
  idx=dr_nog_node$idx_select
  
  
  # # draw new variable (small & big)
  # # draw new order Rule (small & big)
  # r4=runif(1)
  # if(r4<0.5)  # small
  # {
  # 
  #   VarI=tree[[treeI]][[paste("node",idx,sep = "")]]$Var
  #   rule=SplitRule_smallmove(tree,treeI,idx,VarI,x,VarType,Rulemat) # draw rule
  # 
  # }else{  # big
  
  #VarI=tree[[treeI]][[paste("node",idx,sep = "")]]$Var
  VarI = DrPriVar(tree,treeI,idx)
  
  # if(p<=opt$nwarmup)
  # {
  #   
  #   VarI = DrPriVar(tree,treeI,idx) # draw variable
  #   
  # }else{
  #   
  #   VarI = DrPriVar_Dirichlet(s) # draw variable
  # }
  # 
  #   if(!(tree[[treeI]][[paste("node",idx,sep = "")]]$VarAvail[VarI]==1))
  #   {
  #     done=0
  #     
  #     list(tree=tree,alpha=-1,done=done,Var=VarI,cutpoint=rule$Rule,idx=idx,Nnode=Nnode,taul=tree[[treeI]][[paste("node",2*idx,sep = "")]]$tau,taur=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$tau,gl=tree[[treeI]][[paste("node",2*idx,sep = "")]]$g,gr=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$g)
  #     
  #   }else{
  
  rule=SplitRule(tree,treeI,idx,x,VarI,VarType,RuleNum,Rulemat)
  
  #  }
  
  
  PDxl=1-PGrow(tree,treeI,idx*2)
  PDxr=1-PGrow(tree,treeI,(idx*2+1))
  PDxp=PGrow(tree,treeI,idx)
  
  ##### copy old rule
  
  var_copy=tree[[treeI]][[paste("node",idx,sep = "")]]$Var
  rule_copy=tree[[treeI]][[paste("node",idx,sep = "")]]$Rule
  
  tau_copyl=tree[[treeI]][[paste("node",2*idx,sep = "")]]$tau
  tau_copyr=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$tau
  
  g_copyl=tree[[treeI]][[paste("node",2*idx,sep = "")]]$g
  g_copyr=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$g
  
  beta_copyl=tree[[treeI]][[paste("node",2*idx,sep = "")]]$beta
  beta_copyr=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$beta
  
  fhat_copyl=tree[[treeI]][[paste("node",2*idx,sep = "")]]$fhat
  fhat_copyr=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$fhat
  
  
  ### CURRENT 
  
  loglike_curr=0
  log_beta_curr=0
  log_beta_prior_curr=0
  
  nfreq <- floor(nseg_time_temp/2)
  fhat_curr=matrix(0,nfreq+1,2)
  
  jj=0
  for (k in (2*idx):(2*idx+1))
  {
    jj=jj+1
    postbeta_6=postbeta(tree,treeI,k,nseg_time_temp,tree[[treeI]][[paste("node",k,sep = "")]]$tau,YDat,nbeta,opt)
    log_beta_curr=log_beta_curr+dmvnorm(tree[[treeI]][[paste("node",k,sep = "")]]$beta,postbeta_6$beta_mean,0.5*(postbeta_6$beta_var+t(postbeta_6$beta_var)),log = T)
    log_beta_prior_curr=log_beta_prior_curr+dmvnorm(tree[[treeI]][[paste("node",k,sep = "")]]$beta,matrix(0,nbeta,1),diag(c(opt$sigmasqalpha, tree[[treeI]][[paste("node",k,sep = "")]]$tau*matrix(1,opt$nbasis,1))),log = T)
    
    # Loglikelihood at current values
    fhat_curr[,jj]=postbeta_6$nu_mat%*%matrix(tree[[treeI]][[paste("node",k,sep = "")]]$beta,nbeta,1)
    log_curr_spec_dens=whittle_like(postbeta_6$y,fhat_curr[,jj],nseg_time_temp)
    loglike_curr=loglike_curr+log_curr_spec_dens
    
  }
  
  # Calculating log proposal density at current values
  log_proposal_curr=log_beta_curr
  
  # Calculating priors at current values
  log_prior_curr=log_beta_prior_curr
  
  # Evaluating Target density at current values(taking duplicates into consideration)
  log_target_curr=loglike_curr+log_prior_curr
  
  
  ##### Set new variable and order rule
  tree[[treeI]][[paste("node",idx,sep = "")]]$Var=VarI
  tree[[treeI]][[paste("node",idx,sep = "")]]$Rule=rule$Rule
  
  ####### kill original children
  tree=KillChildren(tree,treeI,idx)
  
  #### create new children
  tree=SpawnChildren(tree,treeI,idx,rule$LeftEx,rule$RightEx,x,VarType,RuleNum,Rulemat) # create children
 
  tree[[treeI]][[paste("node",2*idx,sep = "")]]$tau = tau_copyl
  tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$tau = tau_copyr
  
  tree[[treeI]][[paste("node",2*idx,sep = "")]]$g=g_copyl
  tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$g=g_copyr
  
  
  ###### PROPOSAL
  # Evaluating the Likelihood, Proposal and Prior Densities at the Proposed values
  loglike_prop=0
  log_beta_prop=0
  log_beta_prior_prop=0
  
  # Computing mean and variances for beta proposals
  
  nfreq <- floor(nseg_time_temp/2)
  fhat_prop=matrix(0,nfreq+1,2)
  beta_prop=matrix(0,nbeta,2)
  
  jj=0
  for (k in (2*idx):(2*idx+1))
  {
    jj=jj+1
    
    postbeta_7=postbeta(tree,treeI,k,nseg_time_temp,tree[[treeI]][[paste("node",k,sep = "")]]$tau,YDat,nbeta,opt)
    beta_prop[,jj]=rmvnorm(1,postbeta_7$beta_mean,0.5*(postbeta_7$beta_var+t(postbeta_7$beta_var))) # Drawing a new value of beta
    # Beta prop
    log_beta_prop=log_beta_prop+dmvnorm(beta_prop[,jj],postbeta_7$beta_mean,0.5*(postbeta_7$beta_var+t(postbeta_7$beta_var)),log = T)
    # Beta prior
    log_beta_prior_prop=log_beta_prior_prop+dmvnorm(beta_prop[,jj],matrix(0,nbeta,1),diag(c(opt$sigmasqalpha, tree[[treeI]][[paste("node",k,sep = "")]]$tau*matrix(1,opt$nbasis,1))), log = T)
    
    # Loglikelihood  at proposed values
    fhat_prop[,jj]=postbeta_7$nu_mat%*%matrix(beta_prop[,jj],nbeta,1)
    log_prop_spec_dens=whittle_like(postbeta_7$y,fhat_prop[,jj],nseg_time_temp)
    loglike_prop=loglike_prop+log_prop_spec_dens
    
  }
  
  # Calculating log proposal density at proposed values
  log_proposal_prop=log_beta_prop
  
  # Calculating log prior density at proposed values
  log_prior_prop=log_beta_prior_prop
  
  # Evaluating Target density at proposed values(taking duplicates into consideration)
  log_target_prop=loglike_prop+log_prior_prop
  
  PDyl=1-PGrow(tree,treeI,idx*2)
  PDyr=1-PGrow(tree,treeI,idx*2+1)
  PDyp=PGrow(tree,treeI,idx)
  
  alpha1=(PDyl*PDyr*PDyp)/(PDxl*PDxr*PDxp)
  alpha=min(1,exp(log_target_prop-log_target_curr+log_proposal_curr-log_proposal_prop)*alpha1)
  
  r3=runif(1)
  if(r3<alpha)
  {
    done=1
    
    jj=0
    for (k in (2*idx):(2*idx+1))
    {
      
      jj=jj+1
      tree[[treeI]][[paste("node",k,sep = "")]]$beta= beta_prop[,jj]
      tree[[treeI]][[paste("node",k,sep = "")]]$fhat= fhat_prop[,jj]
      
    }
    
  }else
  {
    done=0
    
    tree[[treeI]][[paste("node",idx,sep = "")]]$Var= var_copy
    tree[[treeI]][[paste("node",idx,sep = "")]]$Rule= rule_copy
    
    rule_back=SplitRule_change(tree,treeI,idx,var_copy,x,VarType,Rulemat)
    
    ####### kill original children
    tree=KillChildren(tree,treeI,idx)
    
    #### create new children
    tree=SpawnChildren(tree,treeI,idx,rule_back$LeftEx,rule_back$RightEx,x,VarType,RuleNum,Rulemat) # create children
    
    tree[[treeI]][[paste("node",2*idx,sep = "")]]$tau= tau_copyl
    tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$tau= tau_copyr
    
    
    tree[[treeI]][[paste("node",2*idx,sep = "")]]$g=g_copyl
    tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$g= g_copyr
    
    
    beta_curr=matrix(0,nbeta,2)
    fhat_re_curr=matrix(0,nfreq+1,2)
    
    jj=0
    for (k in (2*idx):(2*idx+1))
    {
      jj=jj+1
      
      postbeta_8=postbeta(tree,treeI,k,nseg_time_temp,tree[[treeI]][[paste("node",k,sep = "")]]$tau,YDat,nbeta,opt)
      beta_curr[,jj]=rmvnorm(1,postbeta_8$beta_mean,0.5*(postbeta_8$beta_var+t(postbeta_8$beta_var))) # Drawing a new value of beta
      fhat_re_curr[,jj]=postbeta_8$nu_mat%*%matrix(beta_curr[,jj],nbeta,1)
      
      ## draw new beta and fhat
      tree[[treeI]][[paste("node",k,sep = "")]]$beta= beta_curr[,jj]
      tree[[treeI]][[paste("node",k,sep = "")]]$fhat= fhat_re_curr[,jj]
      
      
    }
    
    
  }
  
  list(tree=tree,alpha=alpha,done=done,Var=VarI,cutpoint=rule$Rule,idx=idx,Nnode=Nnode,taul=tree[[treeI]][[paste("node",2*idx,sep = "")]]$tau,taur=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$tau,gl=tree[[treeI]][[paste("node",2*idx,sep = "")]]$g,gr=tree[[treeI]][[paste("node",2*idx+1,sep = "")]]$g)
  
  #}
  
  
}

Metrop=function(tree,treeI,YDat,Nnode,x,VarType,RuleNum,Rulemat,Top_idx,param_random,nbeta,nseg_time_temp,nus,Gs,opt)
{
  re=list()
  r=runif(1)
  if(Nnode==1)
  {
    
    re=BirthDeath(tree,treeI,YDat,Nnode,x,VarType,RuleNum,Rulemat,Top_idx,param_random,nbeta,nseg_time_temp,nus,Gs,opt)
    if(re$BD)
    {
      re$step=c("Birth")
    }else{
      re$step=c("Death")
    }
    
  }else{
    
    if(r<0.5)
    {
      
      re=BirthDeath(tree,treeI,YDat,Nnode,x,VarType,RuleNum,Rulemat,Top_idx,param_random,nbeta,nseg_time_temp,nus,Gs,opt)
      if(re$BD)
      {
        re$step=c("Birth")
      }else{
        re$step=c("Death")
      }
      
    }else{
      
      re=ChangeRule(tree,treeI,YDat,Nnode,x,VarType,RuleNum,Rulemat,Top_idx,param_random,nbeta,nseg_time_temp,nus,Gs,opt)
      re$step=c("Change")
    }
    
  }
  
  return(re)
}

lin_basis_func <- function(freq, nbeta)
{
  n <- length(freq)
  omega <- matrix(1, n, nbeta)
  for (j in 2:nbeta)
  {
    omega[, j] <- sqrt(2) * cos((j-1) * pi * freq)/(pi*(j-1))
  }
  
  return(omega)
}

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


####### postbeta
# calculate the postbeta for each bottom nodes

#### whittle_derivs2
# whittle_derivs2
whittle_derivs2 <- function(param,n,nu_mat,ytemp,tau_temp,nbeta,nbasis,sigmasqalpha) 
{
  
  ydev=ytemp-repmat(nu_mat%*%param,1,dim(ytemp)[2]) # Apply element-wise operation to two arrays with implicit expansion enabled
  # ydev=logY-logf
  # ytemp=yy: log periodograms
  # n is the segment length in the time domain
  n1=floor(n/2)
  if (n%%2==1) # odd n
  {
    f=sum(sum(repmat(nu_mat[2:(n1+1),]%*%param,1,dim(ytemp)[2])+exp(ydev[2:(n1+1),])))+
      0.5*(sum(as.vector(nu_mat[1,]%*%param)+exp(ydev[1,])))+
      0.5*(t(param[2:nbeta])%*%param[2:nbeta]/tau_temp+param[1]^2/sigmasqalpha)
    
  }else
  {
    f=sum(sum(repmat(nu_mat[2:n1,]%*%param,1,dim(ytemp)[2])+exp(ydev[2:n1,])))+
      0.5*(sum(as.vector(nu_mat[1,]%*%param)+exp(ydev[1,])))+
      0.5*(sum(as.vector(nu_mat[n1+1,]%*%param)+exp(ydev[n1+1,])))+
      0.5*(t(param[2:nbeta])%*%param[2:nbeta]/tau_temp+param[1]^2/sigmasqalpha)
  }
  
  g=matrix(0,nbeta,1)
  g[1,]=param[1]/sigmasqalpha
  g[2:nbeta,]=param[2:nbeta]/tau_temp
  
  h=matrix(0,nbeta,nbeta)
  h[1,1]=1/sigmasqalpha
  h[2:nbeta,2:nbeta]=1/tau_temp*diag(nbasis)
  
  
  if(n%%2==1)
  {
    
    for (i in 1:dim(ydev)[2])
    {
      temp_mat=nu_mat[2:(n1+1),]*repmat((1-exp(as.matrix(ydev[2:(n1+1),i]))),1,dim(nu_mat)[2])
      g=g+as.matrix(apply(temp_mat,2,sum))+t(0.5*(1-exp(ydev[1,i]))%*%nu_mat[1,])
    }
    
    jj=seq(1:nbeta)
    
    library(pracma)
    big_mat=repmat(t(nu_mat[2:(n1+1),]),nbeta,1)*t(nu_mat[2:(n1+1),repmat(jj,nbeta,1)])
    for (i in 1:dim(ydev)[2])
    {
      coef_mat=repmat(exp(t(ydev[2:(n1+1),i])),nbeta^2,1)
      h=h+apply(array(big_mat*coef_mat,dim=c(nbeta, nbeta, n1)),c(1,2),sum)+t(0.5*exp(ydev[1,i])%*%nu_mat[1,])%*%nu_mat[1,]
    }
    
  }else
  {
    for (i in 1:dim(ydev)[2])
    {
      temp_mat=nu_mat[2:n1,]*repmat((1-exp(as.matrix(ydev[2:n1,i]))),1,dim(nu_mat)[2])
      g=g+as.matrix(apply(temp_mat,2,sum))+t(0.5*(1-exp(ydev[1,i]))%*%nu_mat[1,])+
        t(0.5*(1-exp(ydev[n1+1,i]))%*%nu_mat[n1+1,])
    }
    
    jj=seq(1:nbeta)
    big_mat=repmat(t(nu_mat[2:n1,]),nbeta,1)*t(nu_mat[2:n1,repmat(jj,nbeta,1)])
    for (i in 1:dim(ydev)[2])
    {
      coef_mat=repmat(exp(t(ydev[2:n1,i])),nbeta^2,1)
      h=h+apply(array(big_mat*coef_mat,dim=c(nbeta, nbeta, n1-1)),c(1,2),sum)+
        t(0.5*exp(ydev[1,i])%*%nu_mat[1,])%*%nu_mat[1,]+
        t(0.5*exp(ydev[n1+1,i])%*%nu_mat[n1+1,])%*%nu_mat[n1+1,]
    }
    
  }
  list(value = f, gradient = g, hessian = h)
}

### whittle_like
whittle_like <- function(y,fhat,nseg) {
  
  nfreq=floor(nseg/2)
  log_prop_spec_dens=0
  
  if(nseg%%2==1)
  {
    
    for (i in 1:dim(y)[2])
    {
      log_prop_spec_dens=log_prop_spec_dens-sum(fhat[2:(nfreq+1)]+exp(y[2:(nfreq+1),i]-fhat[2:(nfreq+1)]))-
        0.5*(fhat[1]+exp(y[1,i]-fhat[1]))-0.5*nfreq*log(2*pi)
      
    }
    
  }else
  {
    for (i in 1:dim(y)[2])
    {
      log_prop_spec_dens=log_prop_spec_dens-sum(fhat[2:nfreq]+exp(y[2:nfreq,i]-fhat[2:nfreq]))-
        0.5*(fhat[1]+exp(y[1,i]-fhat[1]))-0.5*(fhat[nfreq+1]+exp(y[nfreq+1,i]-fhat[nfreq+1]))-
        0.5*nfreq*log(2*pi)
    }
  }
  
  return(log_prop_spec_dens)
}

postbeta=function(tree,treeI,idx,nseg_time_temp,tau_temp,YDat,nbeta,opt)
{
  
  #idx is the index of one of bottom node
  
  nfreq <- floor(nseg_time_temp/2)
  freq <- (0:nfreq)/(2 * nfreq)
  
  # create set of indices for replications in jth covariate segment
  uu=tree[[treeI]][[paste("node",idx,sep = "")]]$DataList
  
  # create log periodograms for replications in ith time seg and jth cov seg
  y=matrix(0,nseg_time_temp,length(uu))
  yy=matrix(0,nfreq+1,length(uu))
  
  jj=0
  for (k in 1:length(uu)) # k is the index of observations
  {
    
    jj=jj+1
    
    y[,jj]=YDat[,uu[k]]
    yy[,jj]=y[1:(nfreq+1),jj]
    
  }
  
  # pass log periodograms into optimizer to obtain beta_mean and beta_var for normal approximation to beta posterior
  
  nu_mat=lin_basis_func(freq,nbeta) # basis function
  nn=nseg_time_temp
  ytemp=yy
  param <- rep(0, nbeta)
  
  # source('~/Desktop/R_code/whittle_derivs2.R')
  post <- trust(whittle_derivs2, param, rinit = 1, rmax = 100,
                parscale = rep(1, nbeta), iterlim = 100, fterm = sqrt(.Machine$double.eps),
                mterm = sqrt(.Machine$double.eps), minimize = TRUE,
                blather = FALSE, nn, nu_mat, yy, tau_temp, nbeta, opt$nbasis,opt$sigmasqalpha)
  beta_mean <- post$argument
  beta_var <-  solve(post$hessian)
  list(beta_mean = beta_mean, beta_var = beta_var, nu_mat = nu_mat,
       y = y)
  
}

draw_s=function(tree,Top_idx,theta)
{
  
  var_list=c()
  theta_post=c()
  for(i in 1:Ntree)
  {
    var_list=append(var_list,TopPath(tree,i,Top_idx)$varidx)
  }
  
  
  for (z in 1:NumX)
  {
    if(any(names(table(var_list))==z))
    {
      index=which(names(table(var_list))==z)
      theta_post[z]=theta/NumX+table(var_list)[index]
    }else
    {
      theta_post[z]=theta/NumX
    }
    
  }
  
  ## Dirichlet random number
  temp_gamma=c()
  for(k in 1:length(theta_post))
  {
    temp_gamma[k]=log(rgamma(1,shape=theta_post[k]+1,rate=1))+log(runif(1)/theta_post[k])
  }
  
  lse=log(sum(exp(temp_gamma)))
  log_s=temp_gamma-lse
  
  s=exp(log_s)
  #s=rdirichlet(1, theta_post)
  
  list(s=s,log_s=log_s)
  
}


draw_theta=function(rho,log_s,theta,NumX,a,b)
{
  lambda=seq(1,1000)/1001
  theta_list=(lambda*rho)/(1-lambda)
  theta_log_like=lgamma(theta_list)-NumX*lgamma(theta/NumX)+(theta/NumX)*sum(log_s)
  beta_log_prior=(a-1)*log(theta)+(b-1)*log(1-lambda)
  lwt=theta_log_like+beta_log_prior
  
  lse=max(lwt)+log(sum(exp(lwt-max(lwt))))
  lwt=exp(lwt-lse)
  ## set as probability
  lwt=lwt/sum(lwt)
  theta_select=theta_list[which(rmultinom(1,1,lwt)==1)]
  
  return(theta_select)
}



Bart=function(x_t,x,Ntime,NumObs,Ntree,NumX,VarType,numcut,nus,Gs,param_random,theta,rho,a,b,darton,opt)
{

  nbeta=opt$nbasis+1
  nb_alpha=nbeta

  ### build tree
  tree=list()
  for(i in 1:Ntree)
  {
    tree[[i]]=list()
    tree[[i]]$node1=list("Top"=1,"Bot"=1,"Nog"=0,"Parent"=0,"LeftC"=2,"RightC"=3,"Var"=0,"Rule"=0,"VarAvail"=rep(1,NumX),"DataList"=seq(1:NumObs),"tau"=0,"g"=0,"beta"=0)
  }


  #### Set Rule information
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

  ### periodograms for each obervation
  ### time series information
  nseg_time_temp=Ntime
  Y=matrix(0,nseg_time_temp,NumObs)
  YDat=matrix(0,nseg_time_temp,NumObs)
  Nnode=rep(0,Ntree)

  for(j in 1:NumObs)
  {
    Y[,j] = log(abs(fft(x_t[,j]))^2/nseg_time_temp)
    YDat[,j]=Y[,j]
  }

  ### initial data for the Number of node of each tree
  for (i in 1:Ntree)
  {
    Nnode[i]=length(tree[[i]])
  }

  ### initialize tau value for the ith tree
  for(i in 1:Ntree)
  {
    tree[[i]]$node1$tau=1
  }

  ######  initialize g value for the ith tree


  for(i in 1:Ntree)
  {
    tree[[i]]$node1$g=1
  }

  ######  initialize beta value for the ith tree
  for(i in 1:Ntree)
  {

    postbeta_curr=postbeta(tree,i,1,nseg_time_temp,tree[[i]]$node1$tau,Y/Ntree,nbeta,opt)
    tree[[i]]$node1$beta=rmvnorm(1,postbeta_curr$beta_mean,0.5*(postbeta_curr$beta_var+t(postbeta_curr$beta_var)))
  }

  ### Initialize data information
  ###### method2 (initialize the fitted value for each tree)
  mtotalfit=matrix(0,nseg_time_temp,NumObs)
  mtrainFits=array(0,c(Ntree,nseg_time_temp,NumObs))

  postbeta_fit_average=postbeta(tree,1,1,nseg_time_temp,tree[[1]]$node1$tau,Y/Ntree,nbeta,opt)
  beta_prop_average=rmvnorm(1,postbeta_fit_average$beta_mean,0.5*(postbeta_fit_average$beta_var+t(postbeta_fit_average$beta_var))) # Drawing a new value of beta
  fhat_fit_average=postbeta_fit_average$nu_mat%*%matrix(beta_prop_average,nbeta,1)

  if(nseg_time_temp%%2!=0)
  {
    fhat_fit_average_complete=c(fhat_fit_average,fhat_fit_average[((nseg_time_temp/2)+1):2,1])  # only for even time

  }else{
    fhat_fit_average_complete=c(fhat_fit_average,fhat_fit_average[(nseg_time_temp/2):2,1])  # only for even time

  }

  mtotalfit=repmat(as.matrix(fhat_fit_average_complete*Ntree),1,NumObs)
  for(i in 1:Ntree)
  {
    mtrainFits[i,,]=mtotalfit/Ntree
  }


  ########### MCMC Sampler
  nloop=opt$nloop

  result=matrix(list(),nloop,Ntree)
  aaindex=matrix(list(),nloop,Ntree)
  Num_node=matrix(0,nloop,Ntree)
  Num_bottom=matrix(0,nloop,Ntree)
  tau_sum=matrix(0,nloop,Ntree)
  g_sum=matrix(0,nloop,Ntree)
  beta_sum=array(0,c(nloop,Ntree,nbeta))
  Num_depth=matrix(0,nloop,Ntree)
  ttrree_list=matrix(list(),nloop,Ntree)
  cut_point=matrix(list(),nloop,NumX)
  tree_store=matrix(list(),nloop,Ntree)
  fhat_log=array(0,c(nloop,nseg_time_temp,NumObs))



  Top_idx=1
  library(mvtnorm)
  for(p in 1:nloop)
  {
    for(i in 1:Ntree)
    {
      cat("ite p is",p,"\n")
      cat("tree is",i,"\n")
      # get residual for the jth tree
      for (j in 1:NumObs)
      {
        #method1
        YDat[,j]=Y[,j]-(mtotalfit[,j]-mtrainFits[i,,j])
      }


      result[[p,i]]=Metrop(tree,i,YDat,Nnode[i],x,VarType,RuleNum,Rulemat,Top_idx,param_random,nbeta,nseg_time_temp,nus,Gs,opt)
      tree=result[[p,i]]$tree
      Nnode[i]=result[[p,i]]$Nnode
      Num_node[p,i]=result[[p,i]]$Nnode


      ## update tau & g
      bot_index=BotNodes_without_condition(tree,i,Top_idx)$BotIdx

      for(s in 1:length(bot_index))
      {

        g_a=(nus + 1)/2
        g_b=(nus)/tree[[i]][[paste("node",bot_index[s],sep = "")]]$tau + 1/(Gs^2)
        tree[[i]][[paste("node",bot_index[s],sep = "")]]$g = 1/rgamma(1,g_a,scale=1/g_b)
        #cat("g",tree[[i]][[paste("node",bot_index[s],sep = "")]]$g,"\n")
        tau_a=(nb_alpha - 1 + nus)/2
        tau_b=sum(tree[[i]][[paste("node",bot_index[s],sep = "")]]$beta[2:nbeta]^2)/(2)+nus/tree[[i]][[paste("node",bot_index[s],sep = "")]]$g
        tree[[i]][[paste("node",bot_index[s],sep = "")]]$tau = 1/rgamma(1,tau_a,rate=tau_b)
        #cat("tau",tree[[i]][[paste("node",bot_index[s],sep = "")]]$tau,"\n")
      }

      # calculate mfit
      for (j in 1:NumObs)
      {

        idx=FindNode(tree,j,1,i,x,VarType,RuleNum,Rulemat) # from the top node of the ith tree, find the bottom idx for jth obs

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
      fhat_log[p,,]=apply(mtrainFits,c(2,3),sum)

      ##### diagnostic
      ## store tree for pth ite
      tree_store[[p,i]]=tree[[i]]

      ## Number of bottom nodes
      nbottom=BotNodes_without_condition(tree,i,Top_idx)
      Num_bottom[p,i]=nbottom$BotNumber


      ### Index of bottom nodes
      aaindex[[p,i]]=nbottom$BotIdx

      ### Ave tau, g and beta
      aa_index=nbottom$BotIdx
      for(s in 1:length(aa_index))
      {
        tau_sum[p,i]=tau_sum[p,i]+tree[[i]][[paste("node",aa_index[s],sep = "")]]$tau
        g_sum[p,i]=g_sum[p,i]+tree[[i]][[paste("node",aa_index[s],sep = "")]]$g
        beta_sum[p,i,]=beta_sum[p,i,]+tree[[i]][[paste("node",aa_index[s],sep = "")]]$beta
      }

      ## Death
      depth_idx=max(aa_index)
      Num_depth[p,i]=Depth(tree,i,depth_idx)


      ##### cutpoint frequency
      if(length(tree[[i]])>1)
      {

        bb_index=NotBotNodes(tree,i,Top_idx)$NotBotIdx
        for(s in 1:length(bb_index))
        {
          for(q in 1:NumX)
          {
            if(tree[[i]][[paste("node",bb_index[s],sep = "")]]$Var==q)
            {
              cut_point[[p,q]]=append(cut_point[[p,q]],tree[[i]][[paste("node",bb_index[s],sep = "")]]$Rule)
            }

          }
        }

      }


      ## Top_path
      ttrree_list[[p,i]]=TopPath(tree,i,Top_idx)


    }

  }


  ######## Estimated log spectrum
  nfreq <- floor(nseg_time_temp/2)
  spec_est=matrix(0,nseg_time_temp,NumObs)
  nwarmup=opt$nwarmup
  for(p in nwarmup:nloop)
  {
    spec_est=spec_est+fhat_log[p,,]/(nloop-nwarmup)
  }

  list(spec_est=spec_est,Y=Y,ttrree_list=ttrree_list,cut_point=cut_point,Num_depth=Num_depth,beta_sum=beta_sum,g_sum=g_sum,tau_sum=tau_sum,Num_bottom=Num_bottom,result=result,Num_node=Num_node,aaindex=aaindex,tree_store=tree_store)

}






