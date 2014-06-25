source_github("https://raw.githubusercontent.com/rockclimber112358/Ensemble_Building_Code/master/optParams.R")
source_github("https://raw.githubusercontent.com/rockclimber112358/Ensemble_Building_Code/master/cvModel.R")
source_github("https://raw.githubusercontent.com/rockclimber112358/Higgs-Boson/master/functions.R")
if(Sys.info()[1]=="Windows")
  setwd("C:/Users/rockc_000/Documents/Personal Files/Kaggle/Higgs Boson/")
if(Sys.info()[1]=="Linux" & Sys.info()[4]=="jb")
  setwd("/media/storage/Personal Files/Kaggle/Higgs Boson/")
if(Sys.info()[1]=="Linux" & grepl("ch120",Sys.info()[4]))
  setwd("~/Kaggle/Higgs Boson")
load("Data/d2.RData")

#randomForest optimization
randomForestWrap = function( x, y, mtry, cutoff, nodesize, sampsize, replace, targetProp, ntree=2 ){
  #Large cutoff values lead to more "s" labels in predictions (as long as y is a factor!)
  if(sampsize>1 | sampsize<0 )
    stop("sampsize should be a value between 0 and 1.  It represents % of data used.")
  sSize = sampsize*nrow(x)*c(1-targetProp,targetProp)
  sSize[1] = min(sSize[1], sum(y=="b"))
  sSize[2] = min(sSize[2], sum(y=="s"))
  return( randomForest( x=x, y=y, strata=y, ntree=round(10^ntree), mtry=mtry, cutoff=c(cutoff,1-cutoff)
                  ,nodesize=nodesize, replace=replace, sampsize=sSize) )
}
randomForestWrap(d2[1:1000,6:8], d2$Label[1:1000], mtry=2, cutoff=.5, nodesize=5, sampsize=.5, replace=T, targetProp=.5)
optArgs = list(as.list(c("mtry", "ordered", 0) ) )
optArgs[[1]][[3]] = c(10,20)
optArgs[[2]] = as.list(c("cutoff", "numeric", 0) )
optArgs[[2]][[3]] = c(0.001,0.999)
#optArgs[[3]] = as.list(c("nodesize", "ordered", 0) )
#optArgs[[3]][[3]] = c(1,25)
#optArgs[[4]] = as.list(c("sampsize", "numeric", 0) )
#optArgs[[4]][[3]] = c(.1,.9)
#optArgs[[5]] = as.list(c("replace", "character", 0) )
#optArgs[[5]][[3]] = c(TRUE,FALSE)
optArgs[[3]] = as.list(c("targetProp", "numeric", 0) )
optArgs[[3]][[3]] = c(.001,.999)
optParams( func=randomForestWrap, x=d2[1:250000,6:45], y=d2$Label[1:250000], optArgs=optArgs
  ,coldStart=200, nTrain=c(10000,20000,50000)
  ,optFunc=function(pred,actual){
    -AMS(d2$Weight[samVal],  d2$Label[samVal], as.character(pred))}
  ,constArgs=list(ntree=log10(500), nodesize=10, sampsize=0.5, replace=TRUE)
)
fit = cvModel( modelFunc=randomForestWrap, cvGroup=d2$cvGroup, x=d2, y=d2$Label
  ,args=list(mtry=10, cutoff=0.2, nodesize=10, sampsize=0.5, replace=TRUE, targetProp=0.5, ntree=3.5)
  ,predFunc=function(fit, newdata){as.numeric(predict(fit,newdata)=="s")})

gbmWrap = function( x, y, wtMult, n.trees, interaction.depth, n.minobsinnode, shrinkage
                    ,bag.fraction, cutoff){
  #Large cutoff values lead to more "s" labels in predictions (as long as y is a factor!)
  return( list(
    fit = gbm.fit( x=x, y=y, n.trees=n.trees, interaction.depth=interaction.depth
          ,n.minobsinnode=n.minobsinnode, shrinkage=shrinkage, bag.fraction=bag.fraction
          ,w=ifelse(y=="b", 1, wtMult), verbose=F)
   ,cutoff = cutoff ) )
}
optArgs = list(as.list(c("wtMult", "numeric", 0) ) )
optArgs[[1]][[3]] = c(100,500)
optArgs[[2]] = as.list(c("interaction.depth", "ordered", 0) )
optArgs[[2]][[3]] = c(10,20)
optArgs[[3]] = as.list(c("n.minobsinnode", "ordered", 0) )
optArgs[[3]][[3]] = c(20,50)
optArgs[[4]] = as.list(c("shrinkage", "numeric", 0) )
optArgs[[4]][[3]] = c(.001,.01)
optArgs[[5]] = as.list(c("bag.fraction", "numeric", 0) )
optArgs[[5]][[3]] = c(.01,.5)
optArgs[[6]] = as.list(c("cutoff", "numeric", 0) )
optArgs[[6]][[3]] = c(.01,.5)
optParams( func=gbmWrap, x=d2[1:250000,6:45], y=d2[1:250000,46], optArgs=optArgs
  ,replications=rep(50,3)
  ,coldStart=200, nTrain=c(10000,20000,50000)
  ,optFunc=function(pred,actual){
    -AMS(d2$Weight[samVal],  d2$Label[samVal], pred)}
  ,predFunc=function(fit, newdata){
    preds = predict(fit[[1]], newdata, n.trees=fit[[1]]$n.trees, type="response")
    preds = ifelse( preds>fit[[2]], "s", "b" )
    return(preds) }
  ,constArgs=list(n.trees=200)
)
fit = cvModel( modelFunc=gbmWrap, cvGroup=d2$cvGroup, x=d2[,6:45], y=d2$Signal
  ,args=list(bag.fraction=0.1, cutoff=0.9, interaction.depth=12, n.minobsinnode=30, shrinkage=0.004, wtMult=200, n.trees=200)
  ,predFunc=function(fit, newdata){
     preds = predict(fit[[1]], newdata, n.trees=fit[[1]]$n.trees, type="response")
#     preds = ifelse( preds>fit[[2]], "s", "b" )
     return(preds) }
)
