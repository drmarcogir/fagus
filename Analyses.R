#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Analyses species pools and local communities
# Script created by Marco Girardello 19/02/2016 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# load required libraries
library(dismo);library(WriteXLS);library(piecewiseSEM)
library(lme4);library(pgirmess);library(vegan)
library(stringi);library(nlme);library(MASS)
library(stringr);library(boot);library(dismo)

# source required functions
marcofunctions<-list.files("/mnt/data1tb/Dropbox/Fagus/scripts/fagus/marcosfunctions",full.names=TRUE)
for (f in 1:length(marcofunctions)) {source(marcofunctions[f])}
# read in data
dat<-read.csv("/mnt/data1tb/Dropbox/Fagus/dataJuly16/newdata.csv")
# standardize variables
source("/mnt/data1tb/Dropbox/Fagus/scripts/fagus/Dataprepare.R")

###################
# BRT models
###################

# list of models 
predresp<-list(c("SR1","PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Rpool1"),
               c("SR1","PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Gpool1"),
               c("SR1","PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO"),
               c("SR1","PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Rpool1","L","M","R","N","T"),
               c("SR1","PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Gpool1","L","M","R","N","T"),
               c("SR1","PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","L","M","R","N","T"),
               c("SR2","PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Rpool2"),
               c("SR2","PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Gpool2"),
               c("SR2","PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO"),
               c("SR2","PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Rpool2","L","M","R","N","T"),
               c("SR2","PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","Gpool2","L","M","R","N","T"),
               c("SR2","PLOT","ATEMP","APREC","SPREC","PH","LGMS","DIST","TOPO","L","M","R","N","T"))


brtresults<-fitbrt_wrapper(inputdat=dat1,modlist=predresp)
# write output results
WriteXLS(results1,ExcelFileName="/mnt/data1tb/Dropbox/Fagus/resultsJuly16/BRT.xlsx",row.names=FALSE,col.names=TRUE,BoldHeaderRow = TRUE)

###################
# SEM models
###################

# aspatial models (as lists. required by piecewiseSEM package)
semmods<-fitsem_wrapper()

# bootstrap confidence intervals
sr1nopoolci<-getci(modlist=semmods[[1]],indat=dat.rpool1)
sr1poolci<-getci(modlist=semmods[[2]],indat=dat.rpool1)

sr2nopoolci<-getci(modlist=semmods[[3]],indat=dat.rpool2)
sr2poolci<-getci(modlist=semmods[[4]],indat=dat.rpool2)

g1nopoolci<-getci(modlist=semmods[[5]],indat=dat.gpool1)
g1poolci<-getci(modlist=semmods[[6]],indat=dat.gpool1)

g2nopoolci<-getci(modlist=semmods[[7]],indat=dat.gpool2)
g2poolci<-getci(modlist=semmods[[8]],indat=dat.gpool2)

# standardized path coefficients
sr1nopool<-sem.coefs(modelList=semmods[[1]],dat=dat.rpool1,standardize="scale")
sr1pool<-sem.coefs(modelList=semmods[[2]],dat=dat.rpool1,standardize="scale")

sr2nopool<-sem.coefs(modelList=semmods[[3]],dat=dat.rpool2,standardize="scale")
sr2pool<-sem.coefs(modelList=semmods[[4]],dat=dat.rpool2,standardize="scale")

g1nopool<-sem.coefs(modelList=semmods[[5]],dat=dat.gpool1,standardize="scale")
g1pool<-sem.coefs(modelList=semmods[[6]],dat=dat.gpool1,standardize="scale")

g2nopool<-sem.coefs(modelList=semmods[[7]],dat=dat.gpool2,standardize="scale")
g2pool<-sem.coefs(modelList=semmods[[8]],dat=dat.gpool2,standardize="scale")


# aspatial models (individual regressions)
fitsem_aspatial()

# spatial models (individual regressions) 
fitsem_spatial()

# compute correlograms aspatial models

# compute correlograms spatial models

# create correlogram plots

# write outputs for SEMs
sem.aic(modelList=semmods[[1]],dat=dat.gpool1)
sem.aic(modelList=g1nopool,dat=dat.gpool1)
sem.aic(modelList=g1nopool,dat=dat.gpool1)
sem.aic(modelList=g1nopool,dat=dat.gpool1)



















#------------ sr1 Rpool1

# AIC 
sem.aic(modelList=resultsnosac[[1]],dat=datanosac[[1]])
sem.aic(modelList=sr1pool,dat=dat.rpool1)

# path coefficients
sr1nopool.cf<-sem.coefs(modelList=sr1nopool,dat=dat.rpool1,standardize="scale")
tmp<-sr1nopool.cf[1:5]

WriteXLS(tmp,ExcelFileName="/mnt/data1tb/Dropbox/Fagus/resultsJuly16/pathcoefs/sr1nopool.xlsx",row.names=FALSE,col.names=TRUE,BoldHeaderRow = TRUE)

sr1pool.cf<-sem.coefs(modelList=sr1pool,dat=dat.rpool1,standardize="scale")
tmp<-sr1pool.cf[1:5]

WriteXLS(tmp,ExcelFileName="/mnt/data1tb/Dropbox/Fagus/resultsJuly16/pathcoefs/sr1pool.xlsx",row.names=FALSE,col.names=TRUE,BoldHeaderRow = TRUE)


#------------ sr2 Rpool2
# AIC 
sem.aic(modelList=sr2nopool,dat=dat.rpool2)
sem.aic(modelList=sr2pool,dat=dat.rpool2)

# path coefficients
sr2nopool.cf<-sem.coefs(modelList=sr2nopool,dat=dat.rpool2,standardize="scale")
tmp<-sr2nopool.cf[1:5]
WriteXLS(tmp,ExcelFileName="/mnt/data1tb/Dropbox/Fagus/resultsJuly16/pathcoefs/sr2nopool.xlsx",row.names=FALSE,col.names=TRUE,BoldHeaderRow = TRUE)


sr2pool.cf<-sem.coefs(modelList=sr2pool,dat=dat.rpool2,standardize="scale")
tmp<-sr2pool.cf[1:5]
WriteXLS(tmp,ExcelFileName="/mnt/data1tb/Dropbox/Fagus/resultsJuly16/pathcoefs/sr2pool.xlsx",row.names=FALSE,col.names=TRUE,BoldHeaderRow = TRUE)


#------------ sr1 Gpool1
# AIC 

sem.aic(modelList=g1pool,dat=dat.gpool1)

# path coefficients
g1nopool.cf<-sem.coefs(modelList=g1nopool,dat=dat.gpool1,standardize="scale")

tmp<-g1nopool.cf[1:5]
WriteXLS(tmp,ExcelFileName="/mnt/data1tb/Dropbox/Fagus/resultsJuly16/pathcoefs/g1nopool.xlsx",row.names=FALSE,col.names=TRUE,BoldHeaderRow = TRUE)

g1pool.cf<-sem.coefs(modelList=g1pool,dat=dat.gpool1,standardize="scale")
tmp<-g1pool.cf[1:5]
WriteXLS(tmp,ExcelFileName="/mnt/data1tb/Dropbox/Fagus/resultsJuly16/pathcoefs/g1pool.xlsx",row.names=FALSE,col.names=TRUE,BoldHeaderRow = TRUE)

#------------ sr2 Gpool2
# AIC 
sem.aic(modelList=g2nopool,dat=dat.gpool2)
sem.aic(modelList=g2pool,dat=dat.gpool2)

# path coefficients
g2nopool.cf<-sem.coefs(modelList=g2nopool,dat=dat.gpool2,standardize="scale")
tmp<-g2nopool.cf[1:5]
WriteXLS(tmp,ExcelFileName="/mnt/data1tb/Dropbox/Fagus/resultsJuly16/pathcoefs/g2nopool.xlsx",row.names=FALSE,col.names=TRUE,BoldHeaderRow = TRUE)

g2pool.cf<-sem.coefs(modelList=g2pool,dat=dat.gpool2,standardize="scale")
tmp<-g2pool.cf[1:5]
WriteXLS(tmp,ExcelFileName="/mnt/data1tb/Dropbox/Fagus/resultsJuly16/pathcoefs/g2pool.xlsx",row.names=FALSE,col.names=TRUE,BoldHeaderRow = TRUE)

