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
library(RSNNS)

if(Sys.info()[1]=="Windows")
  setwd("C:/Users/rockc_000/Documents/Personal Files/Kaggle/Higgs Boson/")
if(Sys.info()[1]=="Linux" & Sys.info()[4]=="?")
  setwd("/media/storage/Personal Files/Kaggle/Higgs Boson/")
if(Sys.info()[1]=="Linux" & grepl("ch120",Sys.info()[4]))
  setwd("~/Kaggle/Higgs Boson/")
source_github("https://raw.githubusercontent.com/rockclimber112358/Ensemble_Building_Code/master/cvModel.R")

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
#(1:ncol(d))[!apply(d[d$Model_No==0,][1:1000,], 2, function(x)any(is.na(x)) )]

#useSin: specifies if the sin and cos of the phi variables should be used.
#useTheta: specifies if the raw phi variables should be used.
cvModelNo = function(modelFunc, predFunc=predict, useSin=F, useTheta=T, constructForm=F
            ,args=list(), pred.cols=1)
{
  #Data quality checks
  if(nrow(d)!=800000)
    stop("d doesn't have 800000 rows, something is likely wrong")
  
  depCols=list(c(20:36,ifelse(useTheta,37:39,NA),ifelse(useSin,40:45,NA))
              ,c(15:16,20:36,ifelse(useTheta,c(17,37:39),NA),ifelse(useSin,c(18:19,40:45),NA))
              ,c(6:11,15:16,20:36,ifelse(useTheta,c(12,17,37:39),NA),ifelse(useSin,c(13:14,18:19,40:45),NA))
              ,c(21:36,ifelse(useTheta,c(37:39),NA),ifelse(useSin,c(40:45),NA))
              ,c(15:19,21:45)
              ,c(6:11,15:16,21:36,ifelse(useTheta,c(12,17,37:39),NA),ifelse(useSin,c(13:14,18:19,40:45),NA)) )
  depCols=lapply(depCols, function(x){x[!is.na(x)]})
  d$Signal = as.numeric(d$Label=="s")
  preds = matrix(0,nrow=nrow(d), ncol=pred.cols)
  for(i in 0:5)
  {
    if(constructForm){
      form = as.formula( paste("Signal ~", paste(colnames(d)[depCols[[i+1]]], collapse="+") ) )
      fit = cvModel( modelFunc, d$cvGroup[d$Model_No==i], predFunc, d=d[d$Model_No==i,]
                ,form=form, args=args, pred.cols=pred.cols)
    } else {
      fit = cvModel( modelFunc, d$cvGroup[d$Model_No==i], predFunc, x=d[d$Model_No==i,depCols[[i+1]]]
                ,y=d[d$Model_No==i,46], args=args, pred.cols=pred.cols)
    }
    preds[d$Model_No==i,] = fit$ensem[1:nrow(fit$ensem),]
  }
  return(preds)
}

makeOutput = function(preds, call, splitByGroup=F, modelNo=NULL)
{
  #Data quality checks
  if(!is(call,"character"))
    stop("!is(call,'character')")
  if(splitByGroup & length(modelNo)!=800000)
    stop("splitByGroup & length(modelNo)!=800000.  To split by group we need modelNo!")
  if( is.matrix(preds) | is.data.frame(preds) )
    stop("is.matrix(preds) | is.data.frame(preds).  Ensure preds is a numeric vector!")
  if(splitByGroup & !is(preds,"numeric"))
    stop("splitByGroup & !is(preds,'numeric').  preds must be numeric to use splitByGroup!")
  if(nrow(d)!=800000)
    stop("d must be defined!")

  files = list.files("Submissions")
  files = files[grepl("_raw", files)]
  ids = as.numeric( gsub("_raw.csv", "", files) )
  ids = ids[!is.na(ids)]
  newId = min(ids,0)+1
  write.csv(preds, paste0("Submissions/",newId,"_raw.csv"), row.names=F)

  #Write out Kaggle submission
  if(length(preds)==800000)
  {
    if(is.numeric(preds))
    {
      if(!splitByGroup)
        modelNo = rep(1,800000)
      
      out = rep(0,800000)
      for(i in unique(modelNo) ){
        predsTemp = preds[modelNo==i]
        dTemp = d[modelNo==i,]
        cutoff = sapply( seq(min(predsTemp), max(predsTemp), length.out=10), function(i){
          AMS( dTemp$Weight, dTemp$Label, ifelse(predsTemp>i,"s","b") ) } )
        cutMid = seq(min(predsTemp), max(predsTemp), length.out=10)[which.max(cutoff)]
        cutLow = cutMid - (max(predsTemp)-min(predsTemp))/9
        cutHigh = cutMid + (max(predsTemp)-min(predsTemp))/9
        AMSMid = cutoff[which.max(cutoff)]
        AMSLow = cutoff[which.max(cutoff)-1]
        AMSHigh = cutoff[which.max(cutoff)+1]
        for(j in 1:10)
        {
          if(AMSLow>AMSHigh)
          {
            cutHigh = cutMid
            AMSHigh = AMSMid
            cutMid = (cutHigh + cutLow)/2
            AMSMid = AMS( dTemp$Weight, dTemp$Label, ifelse(predsTemp>cutMid,"s","b") )
          } else {
            cutLow = cutMid
            AMSLow = AMSMid
            cutMid = (cutHigh + cutLow)/2
            AMSMid = AMS( dTemp$Weight, dTemp$Label, ifelse(predsTemp>cutMid,"s","b") )          
          }
        }
        RankOrder = rank(predsTemp[d$cvGroup==-1], ties.method="random")
        predsTemp = ifelse( predsTemp>cutMid, "s", "b" )
        print(paste("Cutoff value used:", round(cutMid,4)))
        out[modelNo==i] = predsTemp
      }
    }
    else
    {
      RankOrder = rank(preds[d$cvGroup==-1], ties.method="random")
      out = preds
    }
    score = AMS( d$Weight, d$Label, out )
    output = data.frame( EventId=d$EventId[d$cvGroup==-1], RankOrder=RankOrder, Class=out[d$cvGroup==-1] )
    if(splitByGroup)
      write.csv(output, file=paste0("Submissions/",newId,"_splitByGroup_",round(score,5),".csv"), row.names=F)    
    else
      write.csv(output, file=paste0("Submissions/",newId,"_",round(score,5),".csv"), row.names=F)
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
    write.csv(output, file=paste0("Submissions/",newId,".csv"), row.names=F)
  }
  
  if("desc.csv" %in% list.files("Submissions") ){
    desc = read.csv("Submissions/desc.csv", stringsAsFactors=F)
    desc = rbind( desc, data.frame(newId, call) )
  } else {
    desc = data.frame(newId, call)
  }
  write.csv(desc, file="Submissions/desc.csv")
}
