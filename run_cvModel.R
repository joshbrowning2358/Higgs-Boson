setwd("C:/Users/rockc_000/Documents/Personal Files/Kaggle/Higgs Boson/")
load("Data/finalData2.RData")
source_github("https://raw.githubusercontent.com/rockclimber112358/Ensemble_Building_Code/master/cvModel.R")
source_github("https://raw.githubusercontent.com/rockclimber112358/Higgs-Boson/master/functions.R")

#Use code below to determine depCols
#(1:ncol(d))[!apply(d[d$Model_No==2,], 2, function(x)any(is.na(x)) )]

############################################################################
# Random Forests
############################################################################

out = cvModelNo("randomForest", ", ntree=100")
write.csv(out, file="Submissions/randomForest_100_sin.csv", row.names=F)
makeOutput(out, "randomForest_100_sin")

############################################################################
# GAMs
############################################################################

#Used finaldata and not finaldata2, cvModelNo doesn't work
#out = cvModelNo("gam", "")
#out = exp(out)/(1+exp(out))
#write.csv(out, file="Submissions/gam_defaults_raw.csv", row.names=F)
#makeOutput(out, "gam_defaults")

out = cvModelNo("gam", "")
out = exp(out)/(1+exp(out))
write.csv(out, file="Submissions/gam_defaults_sin_raw.csv", row.names=F)
makeOutput(out, "gam_defaults_sin")

out = cvModelNo("gam", ",weights=Weight")
out = exp(out)/(1+exp(out))
write.csv(out, file="Submissions/gam_weighted_sin_raw.csv", row.names=F)
makeOutput(out, "gam_weighted_sin")

############################################################################
# Naive Bayes
############################################################################

out = cvModelNo("naiveBayes", "", useLabel=T )
write.csv(out, file="Submissions/naivebayes_raw.csv", row.names=F)
makeOutput(out, "gam_weighted_sin")

############################################################################
# Neural Networks (using nnet)
############################################################################

############################################################################
# GBMs
############################################################################

out = cvModelNo("gbm", "" )
write.csv(out, file="Submissions/gbm_default_raw.csv", row.names=F)
makeOutput(out, "gbm_default_sin")

out = cvModelNo("gbm", ", n.trees=1000" )
write.csv(out, file="Submissions/gbm_1000_raw.csv", row.names=F)
makeOutput(out, "gbm_1000_sin")

#Other ideas:
# Try Naive Bayes again (may be useful in an ensemble)
# Use weights in model development
# Boosted trees may be useful
# Neural networks with radial basis functions seem very useful as well (will require RSNNS)
# kNN: Would this be useful at all?

ggplot( data.frame(pred=out[,1], sig=as.numeric(d$Label=="s")), aes(x=pred, y=sig)) +
  geom_smooth()
