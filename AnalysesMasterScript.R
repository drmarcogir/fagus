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
source("/mnt/data1tb/Dropbox/Fagus/scripts/functions.R")

# read in data
dat<-read.csv("/mnt/data1tb/Dropbox/Fagus/dataJuly16/newdata.csv")
# standardize variables
source("/mnt/data1tb/Dropbox/Fagus/scripts/dataprepare.R")

# fit sem models

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

fitsem_wrapper()



###################
# SEM with spatial
# aucorrelation
###################



# output for SEM models 








