library(reshape)
library(ggplot2)
library(plyr)
library(sqldf)
library(bigmemory)

setwd("C:/Users/rockc_000/Documents/Personal Files/Kaggle/Higgs Boson/")
options(width=100)

train = read.csv(file="Data/training.csv")
train[train==-999] = NA
test = read.csv(file="Data/test.csv")
test[test==-999] = NA
test$Weight = NA
test$Label = NA
d = rbind(train, test)
d$Model_No = ifelse( d$PRI_jet_num==0, 0, ifelse( d$PRI_jet_num==1, 1, 2) )
d$Model_No[is.na(d$DER_mass_MMC)] = d$Model_No[is.na(d$DER_mass_MMC)] + 3
d$cvGroup = -1
d$cvGroup[!is.na(d$Label)] = sample(1:10, replace=T, size=sum(!is.na(d$Label)) )

#table( d$cvGroup, d$Model_No )
save(d, file="Data/finalData.RData")

phiCols = colnames(d)[grepl("_phi$",colnames(d))]
for(i in phiCols)
{
  d = cbind(d, sin(d[,i]) )
  d = cbind(d, cos(d[,i]) )
  colnames(d)[ncol(d)+(-1):0] = paste0(c("sin_","cos_"),i)
  d[,i] = NULL
}

save(d, file="Data/finalData2.RData")
