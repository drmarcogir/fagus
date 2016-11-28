#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Bootstrap confidence intervals 
# for piecewise SEMs
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

getci<-function(modlist,indat){
  results<-NULL
  for (i in 1:length(modlist)){ # go through model list 
    mymod<-modlist[[i]] 
    if (class(mymod)[1]==c("glm")){
      if(mymod$family$family==c("poisson")){
       bm<- boot(data=indat, statistic = coefglm2, R =1000, formula =mymod$formula,parallel="multicore",ncpus=6)
       ci.res<- NULL
       for (z in 1:length(summary(mymod)$coefficients[, 1][-1])) {
         # calculate confidence intervals
         ci <- boot.ci(bm, type = "basic", t0 = bm$t0[z], t = bm$t[, z])
         # format output
         ci1 <- paste(round(ci$basic[-c(1:3)], digits = 3), collapse = ";")
         nci <- names(summary(mymod)$coefficients[, 1][-1])[z]
         names(ci1) <- nci
         cidf <- data.frame(response=str_split(mymod$formula,"~")[[2]],predictor=nci,ci=ci1)
         # bind output
        ci.res <- rbind(cidf, ci.res)
       }
       results<-rbind(ci.res,results)
    } # end of first if statement
    } # end of second if statement
    if (class(mymod)[1]==c("glm")){
      if(mymod$family$family==c("gaussian")){
        bm<- boot(data=indat, statistic = coefglm1, R =1000, formula =mymod$formula,parallel="multicore",ncpus=6)
        ci.res1<- NULL
        for (z in 1:length(summary(mymod)$coefficients[, 1][-1])) {
          # calculate confidence intervals
          ci <- boot.ci(bm, type = "basic", t0 = bm$t0[z], t = bm$t[, z])
          # format output
          ci1 <- paste(round(ci$basic[-c(1:3)], digits = 3), collapse = ";")
          nci <- names(summary(mymod)$coefficients[, 1][-1])[z]
          names(ci1) <- nci
          cidf <- data.frame(response=str_split(mymod$formula,"~")[[2]],predictor=nci,ci=ci1)
          # bind output
          ci.res1<- rbind(cidf, ci.res1)
        }
        results<-rbind(ci.res1,results)
      } # end of first if statement
    } # end of second if statement
      if (class(mymod)[1]==c("glmerMod")){
        bm<- boot(data=indat, statistic=coeflme4, R =1000, formula =formula(mymod),parallel="multicore",ncpus=6)
        ci.res2<- NULL
        for (z in 1:length(summary(mymod)$coefficients[, 1][-1])) {
          # calculate confidence intervals
          ci <- boot.ci(bm, type = "basic", t0 = bm$t0[z], t = bm$t[, z])
          # format output
          ci1 <- paste(round(ci$basic[-c(1:3)], digits = 3), collapse = ";")
          nci <- names(summary(mymod)$coefficients[, 1][-1])[z]
          names(ci1) <- nci
          cidf <- data.frame(response=str_split(formula(mymod),"~")[[2]],predictor=nci,ci=ci1)
          # bind output
          ci.res2<- rbind(cidf, ci.res2)
        }        
        results<-rbind(ci.res2,results)        
        
      }  # end of if statement 
  }    # end of i loop
  return(results)
} # end of function


