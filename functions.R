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
