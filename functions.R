library(reshape)
library(ggplot2)
library(plyr)
library(sqldf)
library(mgcv)
library(randomForest)
library(neuralnet)
library(nnet)
library(gbm)
library(e1071)

setwd("C:/Users/rockc_000/Documents/Personal Files/Kaggle/Higgs Boson/")
options(width=100)

AMS = function(weight, act, pred){
    if(any(is.na(act)))
    {
      warning("Removing NA's in data.")
      filt = !is.na(act)
      weight = weight[filt]
      act = act[filt]
      pred = pred[filt]
    }
    s = sum( weight*(act=="s")*(pred=="s") )
    b = sum( weight*(act=="b")*(pred=="s") )
    br = 10
    return( sqrt(2*((s+b+br)*log(1+s/(b+br))-s)) )
}

#Use code below to determine depCols
#(1:ncol(d))[!apply(d[d$Model_No==2,], 2, function(x)any(is.na(x)) )]

cvModelNo = function(modelText, argsText, depCols=list(18:40, 14:40, 6:40, 19:40, c(14:17,19:40), c(6:17,19:40)), useLabel=F )
{
  d$Signal = as.numeric(d$Label=="s")
  mods = lapply(0:5, function(i)
  {
    if(!useLabel)
      model = paste0( modelText, "( Signal ~ ", paste(colnames(d)[depCols[[i+1]]], collapse="+"),
                      argsText, ")" )
    else
      model = paste0( modelText, "( Label ~ ", paste(colnames(d)[depCols[[i+1]]], collapse="+"),
                      argsText, ")" )      
    return( cvModel( d[d$Model_No==i,], d$cvGroup[d$Model_No==i], indCol=which(colnames(d)=="Signal"), model=model ) )
  })
  preds = matrix(0,nrow=nrow(d))
  for(i in 0:5)
  {
    preds[d$Model_No==i,] = mods[[i+1]]$ensem[,1]
  }
  return(preds)
}

makeOutput = function(preds, fname)
{
  if( is.matrix(preds) | is.data.frame(preds) )
  {
    warning("Converting preds to numeric by taking first column.")
    preds = preds[,1]
  }
  if(length(preds)==800000)
  {
    if(is.numeric(preds))
    {
      cutoff = sapply( seq(min(preds), max(preds), length.out=10), function(i){
        AMS( d$Weight, d$Label, ifelse(out>i,"s","b") ) } )
      cutMid = seq(min(preds), max(preds), length.out=10)[which.max(cutoff)]
      cutLow = cutMid - (max(preds)-min(preds))/9
      cutHigh = cutMid + (max(preds)-min(preds))/9
      AMSMid = cutoff[which.max(cutoff)]
      AMSLow = cutoff[which.max(cutoff)-1]
      AMSHigh = cutoff[which.max(cutoff)+1]
      for(i in 1:10)
      {
        if(AMSLow>AMSHigh)
        {
          cutHigh = cutMid
          AMSHigh = AMSMid
          cutMid = (cutHigh + cutLow)/2
          AMSMid = AMS( d$Weight, d$Label, ifelse(out>cutMid,"s","b") )
        } else {
          cutLow = cutMid
          AMSLow = AMSMid
          cutMid = (cutHigh + cutLow)/2
          AMSMid = AMS( d$Weight, d$Label, ifelse(out>cutMid,"s","b") )          
        }
      }
      RankOrder = rank(preds[d$cvGroup==-1], ties.method="random")
      preds = ifelse( preds>cutMid, "s", "b" )
      print(paste("Cutoff value used:", round(cutMid,4)))
    }
    else
    {
      RankOrder = rank(preds[d$cvGroup==-1], ties.method="random")
    }
    score = AMS( d$Weight, d$Label, preds )
    output = data.frame( EventId=d$EventId[d$cvGroup==-1], RankOrder=RankOrder, Class=preds[d$cvGroup==-1] )
    write.csv(output, file=paste0("Submissions/",fname,"_",round(score,5),".csv"), row.names=F)
  }
  if(length(preds)==550000)
  {
    if(is.numeric(preds))
    {
      warning("Cannot optimize cutoff without training values!  Using 0.5")
      preds = ifelse(preds>.5,"s","b")
    }
    RankOrder = rank(preds[d$cvGroup==-1], ties.method="random")
    output = data.frame( EventId=d$EventId[d$cvGroup==-1], RankOrder=RankOrder, Class=preds )
    write.csv(output, file=paste0("Submissions/",fname,".csv"), row.names=F)
  }
}
