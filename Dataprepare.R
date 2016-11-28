#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Fit BRT models and 
# calculate variable importance
# Script created by Marco Girardello
# 19/02/2016 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#---- Exclude outlier for PH
dat1<-subset(dat,PH > 0)
dat1$PLOT<-replace(dat1$PLOT,is.na(dat1$PLOT),100)

# exclude NAs for Rpool1
dat.rpool1<-subset(dat1,!is.na(Rpool1))
dat.rpool1<-subset(dat.rpool1, Rpool1 > 150)


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

dat.gpool1$Rpool2f<-as.factor(dat.gpool1$Rpool2)
dat.gpool1$Rpool2<-scale(dat.gpool1$Rpool2,center=TRUE,scale=T)
dat.gpool1$LGMS<-scale(dat.gpool1$LGMS,center=TRUE,scale=T)
dat.gpool1$DIST<-scale(dat.gpool1$DIST,center=TRUE,scale=T)
dat.gpool1$TOPO<-scale(dat.gpool1$TOPO,center=TRUE,scale=T)
dat.gpool1$ATEMP<-scale(dat.gpool1$ATEMP,center=TRUE,scale=T)
dat.gpool1$APREC<-scale(dat.gpool1$APREC,center=TRUE,scale=T)
dat.gpool1$SPREC<-scale(dat.gpool1$SPREC,center=TRUE,scale=T)
dat.gpool1$PLOT<-scale(dat.gpool1$PLOT,center=TRUE,scale=T)
dat.gpool1$PH<-scale(dat.gpool1$PH,center=TRUE,scale=T)
dat.gpool1$L<-scale(dat.gpool1$L,center=TRUE,scale=T)
dat.gpool1$T<-scale(dat.gpool1$T,center=TRUE,scale=T)
dat.gpool1$M<-scale(dat.gpool1$M,center=TRUE,scale=T)
dat.gpool1$N<-scale(dat.gpool1$N,center=TRUE,scale=T)
dat.gpool1$R<-scale(dat.gpool1$R,center=TRUE,scale=T)

# exclude NAs for Gpool2
dat.gpool2<-subset(dat1,!is.na(Gpool2))

dat.gpool2$Rpool2f<-as.factor(dat.gpool2$Rpool2)
dat.gpool2$Rpool2<-scale(dat.gpool2$Rpool2,center=TRUE,scale=T)
dat.gpool2$LGMS<-scale(dat.gpool2$LGMS,center=TRUE,scale=T)
dat.gpool2$DIST<-scale(dat.gpool2$DIST,center=TRUE,scale=T)
dat.gpool2$TOPO<-scale(dat.gpool2$TOPO,center=TRUE,scale=T)
dat.gpool2$ATEMP<-scale(dat.gpool2$ATEMP,center=TRUE,scale=T)
dat.gpool2$APREC<-scale(dat.gpool2$APREC,center=TRUE,scale=T)
dat.gpool2$SPREC<-scale(dat.gpool2$SPREC,center=TRUE,scale=T)
dat.gpool2$PLOT<-scale(dat.gpool2$PLOT,center=TRUE,scale=T)
dat.gpool2$PH<-scale(dat.gpool2$PH,center=TRUE,scale=T)
dat.gpool2$L<-scale(dat.gpool2$L,center=TRUE,scale=T)
dat.gpool2$T<-scale(dat.gpool2$T,center=TRUE,scale=T)
dat.gpool2$M<-scale(dat.gpool2$M,center=TRUE,scale=T)
dat.gpool2$N<-scale(dat.gpool2$N,center=TRUE,scale=T)
dat.gpool2$R<-scale(dat.gpool2$R,center=TRUE,scale=T)

