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



