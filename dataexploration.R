#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Analyses species pools and local communities
# Script created by Marco Girardello v.2
# 19/02/2016 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# load required libraries
library(dismo)
library(lavaan)
library(corrplot)
library(WriteXLS)
library(piecewiseSEM)
library(lme4)
library(pgirmess)
library(vegan)
library(stringi)
library(nlme)
library(MASS)
library(stringr)

# read in data
#dat<-read.csv("/mnt/data1tb/Dropbox/Fagus/dataJuly16/newdata.csv")
dat<-read.csv("/mnt/data1tb/Dropbox/Fagus/dataJuly16/newdata.csv")

#---- Exclude outlier for PH
dat1<-subset(dat,PH > 0)
dat1$PLOT<-replace(dat1$PLOT,is.na(dat1$PLOT),100)


# exclude NAs for Rpool1
dat.rpool1<-subset(dat1,!is.na(Rpool1))
dat.rpool1<-subset(dat.rpool1, Rpool1 > 150)

source("/mnt/data1tb/Dropbox/Fagus/scripts/function.R")

# prepare dataset for analysis
dat.rpool1$Rpool1f<-as.factor(dat.rpool1$Rpool1)
dat.rpool1$Rpool1<-scale(dat.rpool1$Rpool1,center=TRUE,scale=T)
dat.rpool1$LGMS<-scale(dat.rpool1$LGMS,center=TRUE,scale=T)
dat.rpool1$DIST<-scale(dat.rpool1$DIST,center=TRUE,scale=T)
dat.rpool1$TOPO<-scale(dat.rpool1$TOPO,center=TRUE,scale=T)
dat.rpool1$ATEMP<-scale(dat.rpool1$ATEMP,center=TRUE,scale=T)
dat.rpool1$APREC<-scale(dat.rpool1$APREC,center=TRUE,scale=T)
dat.rpool1$SPREC<-scale(dat.rpool1$SPREC,center=TRUE,scale=T)
dat.rpool1$PLOT<-scale(dat.rpool1$PLOT,center=TRUE,scale=T)
dat.rpool1$PH<-scale(dat.rpool1$PH,center=TRUE,scale=T)
dat.rpool1$L<-scale(dat.rpool1$L,center=TRUE,scale=T)
dat.rpool1$T<-scale(dat.rpool1$T,center=TRUE,scale=T)
dat.rpool1$M<-scale(dat.rpool1$M,center=TRUE,scale=T)
dat.rpool1$N<-scale(dat.rpool1$N,center=TRUE,scale=T)
dat.rpool1$R<-scale(dat.rpool1$R,center=TRUE,scale=T)

# exclude NAs for Rpool2
dat.rpool2<-subset(dat1,!is.na(Rpool2))

# prepare dataset for analysis
dat.rpool2$Rpool2f<-as.factor(dat.rpool2$Rpool2)
dat.rpool2$Rpool2<-scale(dat.rpool2$Rpool2,center=TRUE,scale=T)
dat.rpool2$LGMS<-scale(dat.rpool2$LGMS,center=TRUE,scale=T)
dat.rpool2$DIST<-scale(dat.rpool2$DIST,center=TRUE,scale=T)
dat.rpool2$TOPO<-scale(dat.rpool2$TOPO,center=TRUE,scale=T)
dat.rpool2$ATEMP<-scale(dat.rpool2$ATEMP,center=TRUE,scale=T)
dat.rpool2$APREC<-scale(dat.rpool2$APREC,center=TRUE,scale=T)
dat.rpool2$SPREC<-scale(dat.rpool2$SPREC,center=TRUE,scale=T)
dat.rpool2$PLOT<-scale(dat.rpool2$PLOT,center=TRUE,scale=T)
dat.rpool2$PH<-scale(dat.rpool2$PH,center=TRUE,scale=T)
dat.rpool2$L<-scale(dat.rpool2$L,center=TRUE,scale=T)
dat.rpool2$T<-scale(dat.rpool2$T,center=TRUE,scale=T)
dat.rpool2$M<-scale(dat.rpool2$M,center=TRUE,scale=T)
dat.rpool2$N<-scale(dat.rpool2$N,center=TRUE,scale=T)
dat.rpool2$R<-scale(dat.rpool2$R,center=TRUE,scale=T)

# exclude NAs for Gpool1
dat.gpool1<-subset(dat1,!is.na(Gpool1))

# exclude NAs for Gpool2
dat.gpool2<-subset(dat1,!is.na(Gpool2))


#----Intercorrelation amongst predictors

# Rpool1
envdf<-dat[,c("PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Rpool1","L","M","R",
"N","T")]


cordata <- cor(envdf, use = "pairwise.complete.obs", method = "spearman")

tiff(filename="/mnt/data1tb/Dropbox/Fagus/results/correlogramRpool1.tif",width=2000,height=2000,pointsize=40)
corrplot(cordata, method = "pie")
dev.off()

envdf1<-dat[,c("PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Gpool1","L","M","R",
"N","T")]

cordata <- cor(envdf1, use = "pairwise.complete.obs", method = "spearman")

tiff(filename="/mnt/data1tb/Dropbox/Fagus/results/correlogramGpool1.tif",width=2000,height=2000,pointsize=40)
corrplot(cordata, method = "pie")
dev.off()

# Moderate for some get close to 0.5 or just above 0.5


#---- Any outliers in response predictors?

pred.l<-c("PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Gpool1","L","M","R",
"N","T","Rpool1","Rpool2","Gpool2")


for (i in 1:length(pred.l)){

# plot name
filen<-paste("/mnt/data1tb/Dropbox/Fagus/results/outliersJuly16/",pred.l[i],".png",sep="")
# save plot
png(filename=filen,width=1200,height=1200,pointsize=18)
dotchart(dat[,pred.l[i]],main=pred.l[i])
dev.off()

}

