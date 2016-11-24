#####################
# SEM model fitting 
# (piecewise SEMs )
#####################

fitsem_wrapper<-function(.){
  
  # model for SR1 (no species pool)
  sr1nopool=list(
    m1<-glm(SR1~LGMS+DIST+TOPO+ATEMP+APREC+SPREC+PLOT+PH+L+T+M+N+R,data=dat.rpool1,family=poisson),
    m2<-glm(R~PH,data=dat.rpool1,family=gaussian),
    m3<-glm(T~ATEMP,data=dat.rpool1,family=gaussian),
    m4<-glm(M~APREC+SPREC,data=dat.rpool1,family=gaussian),
    m5<-glm(N~APREC+SPREC,data=dat.rpool1,family=gaussian))
  
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
  
  allmodels<-list(sr1nopool,sr1pool,sr2nopool,sr2pool,g1nopool,g1pool,g2nopool,g2pool)
  names(allmodels)<-c("sr1nopool","sr1pool","sr2nopool","sr2pool","g1nopool","g1pool","g2nopool","g2pool")
  
  return(allmodels)
}
