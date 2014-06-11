library(reshape)
library(ggplot2)
library(plyr)
library(sqldf)
library(mgcv)

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
    }
    else
    {
      RankOrder = rank(preds[d$cvGroup==-1], ties.method="random")
    }
    score = AMS( d$Weight, d$Label, preds )
    output = data.frame( EventId=d$EventId[d$cvGroup==-1], RankOrder=RankOrder, Class=preds[d$cvGroup==-1] )
    write.csv(output, file=paste0("Submissions/",fname,"_",round(score,5),".csv"))
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
    write.csv(output, file=paste0("Submissions/",fname,".csv"))
  }
}
