#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Fit BRT models and 
# calculate variable importance
# Script created by Marco Girardello
# 19/02/2016 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

fitbrt_wrapper<-function(inputdat,modlist){
  # list where to store results
  results<-vector("list",length(predresp))
  # loop through models to fit
  for (i in 1:length(predresp)){
    # match columns to data frame 
    cols<-match(predresp[[i]],names(dat1))
    dat2<-dat1[,cols]
    dat3<-na.exclude(dat2)
    # fit model
    mod<-gbm.step(data=dat3,gbm.x=match(predresp[[i]][-1],names(dat3)),gbm.y=match(predresp[[i]][1],
    names(dat3)),family="poisson",tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.5,step.size=100)
    # save object
    objname<-paste("/mnt/data1tb/Dropbox/Fagus/modelsJuly16/mod_",i,sep="")
    save(mod,file=objname)
    # extract variable importance
    importance<-summary(mod,plotit=FALSE)
    importance$rel.inf<-round(importance$rel.inf,digits=2)
    # create title
    resp<-paste(mod$gbm.call$response.name,"~",sep="")
    pred<-paste(mod$gbm.call$predictor.names,collapse="+")
    title<-paste(resp,pred,sep="")
    # deviance explained
    d2<-round(100-(mod$cv.statistics$deviance.mean*100)/mod$self.statistics$mean.null,digits=2)
    # final data frame
    df<-data.frame(model=title,importance,d2)
    results[[i]]<-df
  }
  results1<-do.call("rbind",results)
  return(results1)
}
