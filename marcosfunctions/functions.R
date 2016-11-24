library(doMC)
library(foreach)
registerDoMC(cores = 2)
library(MASS)

fitgls <- function(inputdf) {
    
    foreach(i = 1:dim(inputdf)[1], .errorhandling = c("remove")) %dopar% {
        
        # input parameters
        par <- inputdf[i, ]
        
        # if family is gaussian
        if (par$family == "gaussian") {
            
            # create formula
            form1 <- as.formula(as.character(par$formula))
            
            # dataset
            cols<-str_split(par$variables,",")[[1]]
            cols<-c(cols,"UTMx","UTMy")
            dat <- get(as.character(par$dataset))
            dat<-dat[,cols]
            dat<-na.exclude(dat)
            filenamedat<-paste("datmod",par$modID,".csv",sep = "")
            write.csv(dat,filenamedat,row.names=FALSE)
            
            # fit model
            modg<-gls(form1,correlation=corExp(form=~UTMx+UTMy,nugget=TRUE),data=dat)
            #modg <- gls(form1, data = dat)
            
            # model name
            filename <- paste("mod", par$modID,sep = "")
            
            # save model
            save(modg, file = filename)
            
        }        
        if (par$family == "poisson") {   
            dat <- get(as.character(par$dataset))
            # if statement for random effect
            if (par$random.effect == "yes") {
                
                # formula
                form1 <- as.formula(as.character(par$formula))
                form2 <- as.formula(paste("~ 1 |", par$pool, sep = ""))
                
                # dataset
                cols<-str_split(par$variables,",")[[1]]
                cols<-c(cols,"UTMx","UTMy")
                dat <- get(as.character(par$dataset))
                dat<-dat[,cols]
                dat<-na.exclude(dat)
                filenamedat<-paste("datmod",par$modID,".csv",sep = "")
                write.csv(dat,filenamedat,row.names=FALSE)
                 
                # fit model
                modranef<- glmmPQL(form1, form2, family = poisson, data = dat)
                #mod <- glm(form1, family = poisson, data = dat)
                # model name
                filename <- paste("mod",par$modID, sep = "")
                
                # save model
                save(modranef, file = filename)
                
            } else {
                
                # formula
                form1 <- as.formula(as.character(par$formula))
                # fit model
               # dataset
                cols<-str_split(par$variables,",")[[1]]
                cols<-c(cols,"UTMx","UTMy")
                dat <- get(as.character(par$dataset))
                dat<-dat[,cols]
                dat<-na.exclude(dat)
                filenamedat<-paste("datmod",par$modID,".csv",sep = "")
                dat$group<-"a"
                dat$group<-as.factor(dat$group)
                write.csv(dat,filenamedat,row.names=FALSE)
                                 
             modnoranef <- glmmPQL(form1,random=~ 1|group, correlation=corExp(form=~UTMx +UTMy,nugget=TRUE), family = poisson, data = dat)
 
                #modnoranef<- glmmPQL(form1, form2, family = poisson, data = dat)
                #mod <- glm(form1, family = poisson, data = dat)
               # model name
                filename <- paste("mod",par$modID, sep = "")
                
                # save model
                save(modnoranef, file = filename)
                
            }
            
            
            
        }
        
    }
} 

fitgls1<- function(inputdf) {

###    
    foreach(i = 1:dim(inputdf)[1], .errorhandling = c("pass")) %dopar% {
        
        # input parameters
        par <- inputdf[i, ]
        
        # if family is gaussian
        if (par$family == "gaussian") {
            
            # create formula
            form1 <- as.formula(as.character(par$formula))
            
            # dataset
            dat <- get(as.character(par$dataset))
            
            # fit model
            #modg<-gls(form1,correlation=corExp(form=~UTMx+UTMy,nugget=TRUE),data=dat)
            modg <- lm(form1, data = dat)
            
            # model name
            filename <- paste("/mnt/data1tb/Dropbox/Fagus/resultsSeptember1/mod", par$modID, 
                sep = "")
            
            # save model
            save(modg, file = filename)
            
        }
        
        if (par$family == "poisson" & par$random.effect == "yes") {
            
            dat <- get(as.character(par$dataset))
            
                # formula
                form1 <- as.formula(as.character(par$formula))
                form2 <- as.formula(paste("~ 1 |", par$pool, sep = ""))
                
                # fit model
                modglmre<- glmmPQL(form1, form2, family = poisson, data = dat)
#                modglmre<- glmmPQL(form1, form2, correlation=corExp(form=~UTMx #+UTMy,nugget=TRUE),family = poisson, data = dat)
                # model name
#                modglmre<-glm(form1,data=dat)
                filename <- paste("/mnt/data1tb/Dropbox/Fagus/resultsSeptember1/mod",par$modID, sep = "")
                
                # save model
                save(modglmre, file = filename)
               
            } 

        if (par$family == "poisson" & par$random.effect == "no") {
        
                #dat$group<-factor(rep("a",nrow(dat)))
                # formula
                form1 <- as.formula(as.character(par$formula))
                # fit model
#                mod <- glmmPQL(form1, form2, family = poisson, data = dat)
#              modglm <- glmmPQL(form1,random=~ 1|group, correlation=corExp(form=~UTMx +UTMy,nugget=TRUE), family = poisson, data = dat)
          modglm<-glm(form1,data=dat)
          # model name
                filename <- paste("/mnt/data1tb/Dropbox/Fagus/resultsSeptember1/mod",par$modID, sep = "")
                
                # save model
                save(modglm, file = filename)
                
                
            }
            
            
                    
    }
 }






# main bootstrap function
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


