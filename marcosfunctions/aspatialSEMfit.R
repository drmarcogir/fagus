#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Fit aspatial SEM models as 
# separate regressions 
# Script created by Marco Girardello
# 13/07/2016 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
fitsem_aspatial<-function(inputdf){ 
  len<-dim(inputdf)[1]   
  for (i in 1:len){        
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
      modg<-gls(form1,data=dat)          
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
        modranef<- glmmPQL(form1, form2, family = poisson,data = dat)
        #mod <- glm(form1, family = poisson, data = dat)
        # model name
        filename <- paste("mod",par$modID, sep = "")
        # save model
        save(modranef, file = filename)
      } 
    }
    if (par$family == "poisson") {   
      dat <- get(as.character(par$dataset))
      # if statement for random effect
      if (par$random.effect == "no") {
        # formula
        form1 <- as.formula(as.character(par$formula))
        # dataset
        cols<-str_split(par$variables,",")[[1]]
        cols<-c(cols,"UTMx","UTMy")
        dat <- get(as.character(par$dataset))
        dat<-dat[,cols]
        dat<-na.exclude(dat)
        filenamedat<-paste("datmod",par$modID,".csv",sep = "")
        write.csv(dat,filenamedat,row.names=FALSE)                   
        modnoranef <- glm(form1,data = dat,family=poisson)
        # model name
        filename <- paste("mod",par$modID, sep = "")
        # save model
        save(modnoranef, file = filename)
      }
    }
  }
}             

