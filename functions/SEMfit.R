#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Analyses species pools and local communities
# Script created by Marco Girardello 19/02/2016 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# load required libraries
library(dismo);library(WriteXLS);library(piecewiseSEM)
library(lme4);library(pgirmess);library(vegan)
library(stringi);library(nlme);library(MASS)
library(stringr);library(boot)

# source required functions
source("/mnt/data1tb/Dropbox/Fagus/scripts/functions.R")

# read in data
dat<-read.csv("/mnt/data1tb/Dropbox/Fagus/dataJuly16/newdata.csv")
# standardize variables
source("/mnt/data1tb/Dropbox/Fagus/scripts/dataprepare.R")

#####################
# SEM model fitting 
# (piecewise SEMs )
#####################

#------------------------
# Regional species pools
#------------------------

# model for SR1 (no species pool)
sr1nopool=list(
  m1<-glm(SR1~LGMS+DIST+TOPO+ATEMP+APREC+SPREC+PLOT+PH+L+T+M+N+R,data=dat.rpool1,family=poisson),
  m2<-glm(R~PH,data=dat.rpool1,family=gaussian),
  m3<-glm(T~ATEMP,data=dat.rpool1,family=gaussian),
  m4<-glm(M~APREC+SPREC,data=dat.rpool1,family=gaussian),
  m5<-glm(N~APREC+SPREC,data=dat.rpool1,family=gaussian)
)

# model for SR1 (with species pool)
sr1pool=list(
  m1<-glm(Rpool1~LGMS+DIST+TOPO+ATEMP+APREC+SPREC,data=dat.rpool1,family=gaussian),
  m2<-glmer(SR1~LGMS+DIST+TOPO+ATEMP+APREC+SPREC+PLOT+PH+L+T+M+N+R+Rpool1
            +(1|Rpool1f),data=dat.rpool1,family=poisson),
  m3<-glm(R~PH,data=dat.rpool1,family=gaussian),
  m4<-glm(T~ATEMP,data=dat.rpool1,family=gaussian),
  m5<-glm(M~APREC+SPREC,data=dat.rpool1,family=gaussian),
  m6<-glm(N~APREC+SPREC,data=dat.rpool1,family=gaussian)
)

# model for SR2 (no species pool)
sr2nopool=list(
  m1<-glm(SR2~LGMS+DIST+TOPO+ATEMP+APREC+SPREC+PLOT+PH+L+T+M+N+R,data=dat.rpool2,family=poisson),
  m2<-glm(R~PH,data=dat.rpool2,family=gaussian),
  m3<-glm(T~ATEMP,data=dat.rpool2,family=gaussian),
  m4<-glm(M~APREC+SPREC,data=dat.rpool2,family=gaussian),
  m5<-glm(N~APREC+SPREC,data=dat.rpool2,family=gaussian)
)

# model for SR2 (with species pool)
sr2pool=list(
  m1<-glm(Rpool2~LGMS+DIST+TOPO+ATEMP+APREC+SPREC,data=dat.rpool2,family=gaussian),
  m2<-glmer(SR2~LGMS+DIST+TOPO+ATEMP+APREC+SPREC+PLOT+PH+L+T+M+N+R+Rpool2+(1|Rpool2f),data=dat.rpool2,family=poisson),
  m3<-glm(R~PH,data=dat.rpool2,family=gaussian),
  m4<-glm(T~ATEMP,data=dat.rpool2,family=gaussian),
  m5<-glm(M~APREC+SPREC,data=dat.rpool2,family=gaussian),
  m6<-glm(N~APREC+SPREC,data=dat.rpool2,family=gaussian)
)

# Geometric species pools

# model for SR1 (no species pool)
g1nopool=list(
  m1<-glm(SR1~LGMS+DIST+TOPO+ATEMP+APREC+SPREC+PLOT+PH+L+T+M+N+R,data=dat.gpool1,family=poisson),
  m2<-glm(R~PH,data=dat.gpool1,family=gaussian),
  m3<-glm(T~ATEMP,data=dat.gpool1,family=gaussian),
  m4<-glm(M~APREC+SPREC,data=dat.gpool1,family=gaussian),
  m5<-glm(N~APREC+SPREC,data=dat.gpool1,family=gaussian)
)

# model for SR1 (with species pool)
g1pool=list(
  m1<-glm(Gpool1~LGMS+DIST+TOPO+ATEMP+APREC+SPREC,data=dat.gpool1,family=gaussian),
  m2<-glm(SR1~LGMS+DIST+TOPO+ATEMP+APREC+SPREC+PLOT+PH+L+T+M+N+R+Gpool1,data=dat.gpool1,family=poisson),
  m3<-glm(R~PH,data=dat.gpool1,family=gaussian),
  m4<-glm(T~ATEMP,data=dat.gpool1,family=gaussian),
  m5<-glm(M~APREC+SPREC,data=dat.gpool1,family=gaussian),
  m6<-glm(N~APREC+SPREC,data=dat.gpool1,family=gaussian)
)

# model for SR2 (no species pool)
g2nopool=list(
  m1<-glm(SR2~LGMS+DIST+TOPO+ATEMP+APREC+SPREC+PLOT+PH+L+T+M+N+R,data=dat.gpool2,family=poisson),
  m2<-glm(R~PH,data=dat.gpool2,family=gaussian),
  m3<-glm(T~ATEMP,data=dat.gpool2,family=gaussian),
  m4<-glm(M~APREC+SPREC,data=dat.gpool2,family=gaussian),
  m5<-glm(N~APREC+SPREC,data=dat.gpool2,family=gaussian)
)

# model for SR2 (with species pool)
g2pool=list(
  m1<-glm(Gpool2~LGMS+DIST+TOPO+ATEMP+APREC+SPREC,data=dat.gpool2,family=gaussian),
  m2<-glm(SR2~LGMS+DIST+TOPO+ATEMP+APREC+SPREC+PLOT+PH+L+T+M+N+R+Gpool2,data=dat.gpool2,family=poisson),
  m3<-glm(R~PH,data=dat.gpool2,family=gaussian),
  m4<-glm(T~ATEMP,data=dat.gpool2,family=gaussian),
  m5<-glm(M~APREC+SPREC,data=dat.gpool2,family=gaussian),
  m6<-glm(N~APREC+SPREC,data=dat.gpool2,family=gaussian)
)

# bootstrap confidence intervals
sr1nopoolci<-getci(modlist=sr1nopool,indat=dat.rpool1)
sr1poolci<-getci(modlist=sr1pool,indat=dat.rpool1)

sr2nopoolci<-getci(modlist=sr2nopool,indat=dat.rpool2)
sr2poolci<-getci(modlist=sr2pool,indat=dat.rpool2)

g1nopoolci<-getci(modlist=g1nopool,indat=dat.gpool1)
g1poolci<-getci(modlist=g1pool,indat=dat.gpool1)

g2nopoolci<-getci(modlist=g2nopool,indat=dat.gpool2)
g2poolci<-getci(modlist=g2pool,indat=dat.gpool2)

#---------------------
# write out outputs
#--------------------

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
sem.aic(modelList=g1nopool,dat=dat.gpool1)

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


