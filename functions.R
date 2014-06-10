library(reshape)
library(ggplot2)
library(plyr)
library(sqldf)
library(mgcv)

setwd("C:/Users/rockc_000/Documents/Personal Files/Kaggle/Higgs Boson/")
options(width=100)

AMS = function(weight, act, pred){
    s = sum( weight*(act=="s")*(pred=="s") )
    b = sum( weight*(act=="b")*(pred=="s") )
    br = 10
    return( sqrt(2*((s+b+br)*log(1+s/(b+br))-s)) )
}

#d: Dataset of interest. Should contain all training and test observations!
#cvGroup: Specify the cross-validation group number for the training data (1 to # of cv groups, typically 10). -1=test data, 0=ignored.
#indCol: The column of d containing the independent variable.
#model: A string containing the model specification. data arguments will be ignored, and the function used is required to have a formula argument.
#pred.cols: Some algorithms support multiple predictions (multiple averaged models, for example). pred.cols controls how many models should be estimated, and estimations are choosen in a meaningful way. Defaults to 1 (or 10 if gbm or fit.glmnet)
#Currently supported functions: fit.nn (defined above), neuralnet, gbm, randomForest, glm, lm, rpart, glmnet, pcr
cvModel = function(d, cvGroup, indCol, model="neuralnet(Y ~ X1 + X2 + X3 + X4 + X5, hidden=4, err.fct='sse')", pred.cols=1+9*grepl("(^fit.glmnet|^gbm)",model) ){
  ensem = data.frame( matrix(0, nrow=nrow(d), ncol=pred.cols ) )
  #Set up the rownames of ensem to match d. This makes inserting in predicted values much easier later:
  rownames(ensem) = 1:nrow(d)
  rownames(d) = 1:nrow(d)
  
  model = gsub("data=[A-Za-z0-9_.]*", "data=train", model )
  model = gsub(" ","",model)
  if(!grepl( "data=", model )) model = paste0(substr(model,1,nchar(model)-1),", data=train )")
  
  #Store the models, if desirable
  mods = list()
  
  #Set up the model formula and rename the columns of d appropriately:
  #Holdout each cv-group in turn:
  for( i in sort(unique(cvGroup[cvGroup>0])) ){
    train = d[!cvGroup %in% c(-1,i),]
    predict = d[cvGroup %in% c(-1,i),-indCol]
    
    #Evaluate the model
    fit = eval( parse( text=model) )
    
    #Predict based on the model used:
    if( grepl("^neuralnet", model) ){
      #neuralnet prediction requires a dataframe with only the used variables in it:
      depCols = gsub( ",.*", "", gsub(".*~", "", model ) )
      depCols = strsplit(depCols, "+", fixed=T)[[1]]
      depCols = sapply(depCols, function(x){gsub(" ","",x)} )
      predict.temp = predict[,colnames(predict) %in% depCols]
      preds = compute(fit, predict.temp)$net.result
    }
    if( grepl("^fit.nn", model) ){
      preds = predict.nn(fit, newdata=predict)
      mods[[length(mods)+1]] = fit
    }
    if( grepl("^gbm", model) ){
      if(pred.cols==1) warning("Only generating one prediction for a model that allows many!")
      #Exponentially space out the trees for prediction, but round to nearest integer and remove duplicates:
      tree.list = unique( round( exp(seq(0,log(fit$n.trees),length.out=pred.cols)) ) )
      preds = data.frame(predict(fit, newdata=predict, n.trees=tree.list))
      colnames(ensem) = paste0("gbm_trees",tree.list)
      #Remove extra columns in ensem, if applicable
      ensem = ensem[,1:ncol(preds)]
    }
    if( grepl("(^randomForest|^nnet)", model) )
      preds = data.frame(predict(fit, newdata=predict))
    if( grepl("^([g]*lm|rpart)", model) ){
      preds = data.frame(predict(fit, newdata=predict))
      mods[[length(mods)+1]] = fit$coeff
    }
    if( grepl("^pcr", model) ){
      #Prediction returns a 3-dimensional array (observations x prediction_type (always 1 here) x components). Extract and return all components
      preds = apply(predict(fit, newdata=predict, type="response"), 3, identity)
      if( ncol(preds)!=ncol(ensem) ){
        warning(paste0("Overwriting pred.cols to return all components: ",pred.cols,"->",ncol(preds)))
        if(ncol(ensem)<ncol(preds)) ensem = data.frame( matrix(0, nrow=nrow(d), ncol=ncol(preds)) )
        if(ncol(ensem)>ncol(preds)) ensem = ensem[,1:ncol(preds)]
      }
      colnames(ensem) = colnames(preds)
      preds = data.frame(preds)
    }
    if( grepl("^fit.glmnet", model) ){
      if(pred.cols==1) warning("Only generating one prediction for a model that allows many!")
      preds = data.frame(predict(fit, newx=as.matrix(predict)))
      col.index = round(seq(1,ncol(preds),length.out=pred.cols))
      preds = preds[,col.index]
      colnames(ensem) = paste0("glmnet_lambda",round(fit$lambda[col.index],4))
    }
    colnames(ensem) = gsub("\\.", "d", colnames(ensem))
    rownames(preds) = rownames(predict)
    
    #Insert the predicted values for the cv group into the ensem data.frame.
    pred.index = (1:nrow(ensem))[cvGroup==i]
    ensem[pred.index,] = preds[rownames(preds) %in% pred.index,]
    
    #Insert the predicted values for the test group into the ensem data.frame, but divide by the number of cv-folds (since each fold has a diff. prediction)
    test.index = (1:nrow(ensem))[cvGroup==-1]
    ensem[test.index,] = ensem[test.index,] + preds[rownames(preds) %in% test.index,]/(length(unique(cvGroup))-1)
    print(paste0("Model ",i,"/",length(unique(cvGroup[cvGroup>0]))," has finished"))
  }
  if(length(mods)==0) return(ensem)
  return(list(ensemble=ensem, models=mods))
}
