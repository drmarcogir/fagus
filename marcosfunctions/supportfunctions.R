#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Support functions for Fagus
# data analysis project. Created
# by Marco Girardello 19/09/2016
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# SEM bootstrapping support functions
coeflme4<- function(formula, data, indices) {
       d <- data[indices,] # allows boot to select sample 
       fit<-glmer(formula,family=poisson,data=d) 
       return(summary(fit)$coefficients[,1][-1])
       }

coefglm1<- function(formula, data, indices) {
       d <- data[indices,] # allows boot to select sample 
       fit <- glm(formula, data=d,family="gaussian") 
       return(summary(fit)$coefficients[,1][-1])
       }

coefglm2<- function(formula, data, indices) {
       d <- data[indices,] # allows boot to select sample 
       fit <- glm(formula, data=d,family="poisson") 
       return(summary(fit)$coefficients[,1][-1])
       }

# function for splitting text
swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)

# split strings
strsplmg<-function(name,breaks){
  res<-NULL
  for (i in 1:length(name)){
    tmpv<-paste(stri_wrap(name[i],width=breaks,use_length=TRUE),collapse="\n")
    res[i]<-tmpv
  }
  return(res)
}



# get models according to some criterion
getmodels<-function(modnames,inpath){
  results<-NULL
  for (i in 1:length(modnames)){
    tmpmod<-get(load(paste(inpath,"/",modnames[i],sep="")))
    summod<-summary(tmpmod)
    coefs<-summod$Coef[-1,]
    if(stri_sub(modnames[i],from=8,to=8)=="G"){
      res<-data.frame(response=stri_sub(modnames[i],from=1,to=3),predictor=row.names(coefs),coefs[,1:2],p.values=coefs[,4],pool="Gpool")
    } else {
      res<-data.frame(response=stri_sub(modnames[i],from=1,to=3),predictor=row.names(coefs),coefs[,1:2],p.values=coefs[,4],pool="Rpool") 
    }
    results<-rbind(res,results)
  }
  return(results)
}

             