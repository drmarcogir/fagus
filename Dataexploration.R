#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Exploratory analyses of Fagus dataset
# Script created by Marco Girardello 19/02/2015 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# load required libraries
library(corrplot)
# read in data
dat<-read.csv("/mnt/data1tb/Dropbox/Fagus/dataJuly16/newdata.csv")
# standardize variables
source("/mnt/data1tb/Dropbox/Fagus/scripts/dataprepare.R")

###################################
#Intercorrelation among predictors
###################################

# Rpool dataset
envdf<-dat[,c("PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Rpool1","L","M","R","N","T")]
# compute spearman's correlation coefficient for all pairwise complete obervations
cordata <- cor(envdf, use = "pairwise.complete.obs", method = "spearman")

# pretty picture of intercorrelation
tiff(filename="/mnt/data1tb/Dropbox/Fagus/results/correlogramRpool1.tif",width=2000,height=2000,pointsize=40)
corrplot(cordata, method = "pie")
dev.off()

# Gpool dataset
envdf1<-dat[,c("PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Gpool1","L","M","R","N","T")]
cordata <- cor(envdf1, use = "pairwise.complete.obs", method = "spearman")

# another pretty picture of intercorrelation
tiff(filename="/mnt/data1tb/Dropbox/Fagus/results/correlogramGpool1.tif",width=2000,height=2000,pointsize=40)
corrplot(cordata, method = "pie")
dev.off()


################################
# Any outliers in response and
# and predictors?
################################

pred.l<-c("PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Gpool1","L","M","R","N","T","Rpool1","Rpool2","Gpool2")

for (i in 1:length(pred.l)){
  # plot name
  filen<-paste("/mnt/data1tb/Dropbox/Fagus/results/outliersJuly16/",pred.l[i],".png",sep="")
  # save plot
  png(filename=filen,width=1200,height=1200,pointsize=18)
  dotchart(dat[,pred.l[i]],main=pred.l[i])
    dev.off()
  
}

