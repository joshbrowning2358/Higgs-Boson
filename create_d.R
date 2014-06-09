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
data = big.matrix(nrow=nrow(d), ncol=200, backingfile="data", backingpath="Data/")

cnames = c()
for(i in 1:ncol(d) ){
  data[,i] = d[,i]
  cnames = c(cnames,colnames(d)[i])
}
summary( data[,1:34] )
write.csv(cnames, "Data/cnames.csv")
