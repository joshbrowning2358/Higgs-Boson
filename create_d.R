library(reshape)
library(ggplot2)
library(plyr)
library(sqldf)

if(Sys.info()[1]=="Windows")
  setwd("C:/Users/rockc_000/Documents/Personal Files/Kaggle/Higgs Boson/")
if(Sys.info()[1]=="Linux")
  setwd("/media/storage/Personal Files/Kaggle/Higgs Boson/")

train = read.csv(file="Data/training.csv")
train[train==-999] = NA
test = read.csv(file="Data/test.csv")
test[test==-999] = NA
test$Weight = NA
test$Label = NA
d = rbind(train, test)
d$Model_No = ifelse( d$PRI_jet_num==0, 0, ifelse( d$PRI_jet_num==1, 1, 2) )
d$Model_No[is.na(d$DER_mass_MMC)] = d$Model_No[is.na(d$DER_mass_MMC)] + 3
set.seed(123)
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
}
d = d[,c("EventId", "Weight", "Label", "Model_No", "cvGroup"
    ,"DER_deltaeta_jet_jet", "DER_mass_jet_jet", "DER_prodeta_jet_jet", "DER_lep_eta_centrality", "PRI_jet_subleading_pt", "PRI_jet_subleading_eta", "PRI_jet_subleading_phi", "sin_PRI_jet_subleading_phi", "cos_PRI_jet_subleading_phi"
    ,"PRI_jet_leading_pt", "PRI_jet_leading_eta", "PRI_jet_leading_phi", "sin_PRI_jet_leading_phi", "cos_PRI_jet_leading_phi"
    ,"DER_mass_MMC"
    ,"DER_mass_transverse_met_lep", "DER_mass_vis", "DER_pt_h", "DER_deltar_tau_lep", "DER_pt_tot", "DER_sum_pt", "DER_pt_ratio_lep_tau", "DER_met_phi_centrality", "PRI_tau_pt", "PRI_tau_eta","PRI_lep_pt", "PRI_lep_eta", "PRI_met", "PRI_met_sumet", "PRI_jet_num", "PRI_jet_all_pt", "PRI_tau_phi", "PRI_lep_phi", "PRI_met_phi", "sin_PRI_tau_phi", "cos_PRI_tau_phi", "sin_PRI_lep_phi", "cos_PRI_lep_phi", "sin_PRI_met_phi", "cos_PRI_met_phi")]

save(d, file="Data/finalData2.RData")
