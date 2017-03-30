## Boosted regression Tree and Structural Equation modelling analyses for plant species richness in Beech forests (Fagus sylvatica) in Europe. 

```r
# load required libraries
library(dismo);library(piecewiseSEM);library(XLConnect)
library(lme4);library(pgirmess);library(vegan)
library(stringi);library(nlme);library(MASS)
library(stringr);library(boot);library(dismo)
library(doMC);library(foreach);library(MASS)
library(nlme);library(spdep);library(ggplot2);
library(WriteXLS)
registerDoMC(cores = 4)

# source required functions
marcofunctions<-list.files("/mnt/data1tb/Dropbox/Fagus/scripts/fagus/marcosfunctions",full.names=TRUE)
for (f in 1:length(marcofunctions)) {source(marcofunctions[f])}

# read in data
dat<-read.csv("/mnt/data1tb/Dropbox/Fagus/dataJuly16/newdata.csv")

# standardize variables
source("/mnt/data1tb/Dropbox/Fagus/scripts/fagus/Dataprepare.R")

################################
# Boosted Regression Tree models
################################

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

# fit models
brtresults<-fitbrt_wrapper(inputdat=dat1,modlist=predresp)

# write output results
writeWorksheetToFile(data=brtresults,file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",sheet = "BRTresults", header = TRUE,startCol=1,
startRow=1,styleAction =XLC$"STYLE_ACTION.NONE")

############################
# Structural Equation Models
############################

# aspatial models (as lists. required by piecewiseSEM package)
semmods<-fitsem_wrapper()
semmods<-fitsem_wrapper1()
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
sr1nopool<-sem.coefs(modelList=semmods[[1]],dat=dat.rpool1,standardize="none")
sr1pool<-sem.coefs(modelList=semmods[[2]],dat=dat.rpool1,standardize="none")

sr2nopool<-sem.coefs(modelList=semmods[[3]],dat=dat.rpool2,standardize="none")
sr2pool<-sem.coefs(modelList=semmods[[4]],dat=dat.rpool2,standardize="none")

g1nopool<-sem.coefs(modelList=semmods[[5]],dat=dat.gpool1,standardize="none")
g1pool<-sem.coefs(modelList=semmods[[6]],dat=dat.gpool1,standardize="none")

g2nopool<-sem.coefs(modelList=semmods[[7]],dat=dat.gpool2,standardize="none")
g2pool<-sem.coefs(modelList=semmods[[8]],dat=dat.gpool2,standardize="none")


# Write results on excel spreadsheet
sr1nopoolall<-merge(sr1nopool[1:5],sr1nopoolci)
writeWorksheetToFile(data=sr1nopoolall,file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",
sheet = "SEMcoefs", header = FALSE,startCol=1,startRow=2,styleAction =XLC$"STYLE_ACTION.NONE")

sr1poolall<-merge(sr1pool[1:5],sr1poolci)
writeWorksheetToFile(data=sr1poolall,file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",
sheet = "SEMcoefs", header = FALSE,startCol=8,startRow=2,styleAction =XLC$"STYLE_ACTION.NONE")

sr2nopoolall<-merge(sr2nopool[1:5],sr2nopoolci)
writeWorksheetToFile(data=sr2nopoolall,file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",
sheet = "SEMcoefs", header = FALSE,startCol=1,startRow=30,styleAction =XLC$"STYLE_ACTION.NONE")

sr2poolall<-merge(sr2pool[1:5],sr2poolci)
writeWorksheetToFile(data=sr2poolall,file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",
sheet = "SEMcoefs", header = FALSE,startCol=8,startRow=30,styleAction =XLC$"STYLE_ACTION.NONE")

g1nopoolall<-merge(g1nopool[1:5],g1nopoolci)
writeWorksheetToFile(data=g1nopoolall,file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",
sheet = "SEMcoefs", header = FALSE,startCol=1,startRow=58,styleAction =XLC$"STYLE_ACTION.NONE")

g1poolall<-merge(g1pool[1:5],g1poolci)
writeWorksheetToFile(data=g1poolall,file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",
sheet = "SEMcoefs", header = FALSE,startCol=8,startRow=58,styleAction =XLC$"STYLE_ACTION.NONE")

g2nopoolall<-merge(g2nopool[1:5],g2nopoolci)
writeWorksheetToFile(data=g2nopoolall,file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",
sheet = "SEMcoefs", header = FALSE,startCol=1,startRow=87,styleAction =XLC$"STYLE_ACTION.NONE")

g2poolall<-merge(g2pool[1:5],g2poolci)
writeWorksheetToFile(data=g2poolall,file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",
sheet = "SEMcoefs", header = FALSE,startCol=8,startRow=87,styleAction =XLC$"STYLE_ACTION.NONE")

# calculate AIC values for SEMs
df1<-data.frame(AIC=sem.aic(modelList=semmods[[1]],dat=dat.rpool1)$AIC,model=names(semmods[1]))
df2<-data.frame(AIC=sem.aic(modelList=semmods[[2]],dat=dat.rpool1)$AIC,model=names(semmods[2]))
df3<-data.frame(AIC=sem.aic(modelList=semmods[[3]],dat=dat.rpool1)$AIC,model=names(semmods[3]))
df4<-data.frame(AIC=sem.aic(modelList=semmods[[4]],dat=dat.rpool1)$AIC,model=names(semmods[4]))
df5<-data.frame(AIC=sem.aic(modelList=semmods[[5]],dat=dat.rpool1)$AIC,model=names(semmods[5]))
df6<-data.frame(AIC=sem.aic(modelList=semmods[[6]],dat=dat.rpool1)$AIC,model=names(semmods[6]))
df7<-data.frame(AIC=sem.aic(modelList=semmods[[7]],dat=dat.rpool1)$AIC,model=names(semmods[7]))
df8<-data.frame(AIC=sem.aic(modelList=semmods[[8]],dat=dat.rpool1)$AIC,model=names(semmods[8]))
aic<-rbind(df1,df2,df3,df4,df5,df6,df7,df8)

# write AIC results
writeWorksheetToFile(data=aic,file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",
sheet = "SEMAIC", header = FALSE,startCol=1,startRow=2,styleAction =XLC$"STYLE_ACTION.NONE")

###################################
# Spatial autocorrelation analyses
###################################

# model list
moddf<-read.csv(file="/mnt/data1tb/Dropbox/Fagus/data/modlistNov16b.csv")
# fit aspatial models
setwd("/mnt/data1tb/Dropbox/Fagus/resultsOctober/new/aspatial")
fitsem_aspatial(inputdf=moddf)
# compute correlograms aspatial models
corrnosac<-sacmg(inputdf=moddf,inpath="/mnt/data1tb/Dropbox/Fagus/resultsOctober/new/aspatial")
#write.csv(corrnosac,file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/new/aspatial/corrnosacb.csv",row.names=F)
#corrnosac<-read.csv("/mnt/data1tb/Dropbox/Fagus/resultsOctober/new/aspatial/corrnosac.csv")
corrnosac$dist.class<-corrnosac$dist.class/1000
corrnosac$title<-strsplmg(corrnosac$title,breaks=25)
# create plot
sacp<-ggplot(corrnosac,aes(x=dist.class,y=coef))+geom_point(size=1.5)+geom_line()+ylab("Moran I")+xlab("Distance (km)")+facet_grid(sem.name~title)+theme_bw()+theme(strip.text.x = element_text(size=7, face="bold"),strip.text.y = element_text(size=8, face="bold"))
# save plot
ggsave(filename="/mnt/data1tb/Dropbox/Fagus/resultsOctober/sacplots/sacp1b.png",plot =sacp,width=12,height=8)


# fit spatial models (autoregressive models)
setwd("/mnt/data1tb/Dropbox/Fagus/resultsOctober/new/spatial")
# select models to fit
moddf1<-moddf[moddf$modID %in% c(10,21,32,43),]
# fit models
spatiamods<-fitsem_spatial1(modlist=moddf1,neigh.l=1:7,coorn=c("UTMx","UTMy"))
write.csv(spatiamods,file="corrsac.csv",row.names=F)
spatiamods$title<-strsplmg(spatiamods$title,breaks=25)
spatiamods1<-merge(moddf[,c("modID","formula")],spatiamods)
spatiamods1$formula<-strsplmg(spatiamods1$formula,breaks=25)

# create Moran's I plot
sacp1<-ggplot(spatiamods1,aes(x=dist.class,y=coef))+geom_point(size=2)+geom_line()+ylab("Moran I")+xlab("Distance (km)")+facet_wrap(~formula+neigh)+theme_bw()

# after a careful examination of correlograms
df1<-subset(spatiamods1,neigh==7 & sem.name=="All species & Geometric pool")
df2<-subset(spatiamods1,neigh==7 & sem.name=="Specialists & Geometric pool")
df3<-subset(spatiamods1,neigh==3 & sem.name=="All species & Regional pool")
df4<-subset(spatiamods1,neigh==3 & sem.name=="Specialists & Regional pool")
combined<-rbind(df1,df2,df3,df4)
combined$dist.class<-combined$dist.class/1000


# create plot
sacp2<-ggplot(combined,aes(x=dist.class,y=coef))+geom_point(size=2)+geom_line()+ylab("Moran I")+xlab("Distance (km)")+facet_wrap(sem.name~title)+theme_bw()

# save plot
ggsave(filename="/mnt/data1tb/Dropbox/Fagus/resultsOctober/sacplots/sacp2.png",plot=sacp2,width=8,height=8)


# load models which have corrected for spatial autocorrelation
# get coefficients, standard errors and p-values
coefsac<-getmodels(modnames=c("SR1pool_n3","SR2pool_n3","SR1poolG_n7","SR2poolG_n7"),inpath="/mnt/data1tb/Dropbox/Fagus/resultsOctober/new/spatial")
row.names(coefsac)<-1:dim(coefsac)[1]

# write results onto excel spreadsheet
writeWorksheetToFile(data=coefsac[1:6,2:5],file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",
sheet = "SEMcoefsSAC", header = FALSE,startCol=9,startRow=87,styleAction =XLC$"STYLE_ACTION.NONE")
writeWorksheetToFile(data=coefsac[7:12,2:5],file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",
                     sheet = "SEMcoefsSAC", header = FALSE,startCol=9,startRow=58,styleAction =XLC$"STYLE_ACTION.NONE")
writeWorksheetToFile(data=coefsac[13:18,2:5],file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",
                     sheet = "SEMcoefsSAC", header = FALSE,startCol=9,startRow=35,styleAction =XLC$"STYLE_ACTION.NONE")
writeWorksheetToFile(data=coefsac[19:24,2:5],file="/mnt/data1tb/Dropbox/Fagus/resultsOctober/excel/Fagusresults.xlsx",
                     sheet = "SEMcoefsSAC", header = FALSE,startCol=9,startRow=7,styleAction =XLC$"STYLE_ACTION.NONE")
```