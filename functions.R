library(reshape)
library(ggplot2)
library(plyr)
library(sqldf)
library(mgcv)
library(randomForest)
library(neuralnet)
library(nnet)
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

cvModelNo = function(modelText, argsText, depCols=list(c(2:5,9:13,15:21,26,31:36), c(2:5,9:13,15:23,26,31:38), c(2:26,31:40), c(3:5,9:13,15:21,26,31:36), c(3:5,9:13,15:23,26,31:38), c(3:26,31:40)) )
{
  d$Signal = as.numeric(d$Label=="s")
  mods = lapply(0:5, function(i)
  {
    model = paste0( modelText, "( Signal ~ ", paste(colnames(d)[depCols[[i+1]]], collapse="+"),
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
      cutoff = sapply( seq(0,1,length.out=100), function(i){
        AMS( d$Weight, d$Label, ifelse(out>i,"s","b") ) } )
      RankOrder = rank(preds[d$cvGroup==-1], ties.method="random")
      preds = ifelse( preds>seq(0,1,length.out=100)[which.max(cutoff)], "s", "b" )
      print(paste("Optimal cutoff value:", round(seq(0,1,length.out=100)[which.max(cutoff)],2)))
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
