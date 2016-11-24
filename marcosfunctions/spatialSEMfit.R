#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Fit aspatial SEM models as 
# separate regressions 
# Script created by Marco Girardello
# 14/09/2016 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


fitsem_aspatial<-function(inputdf){    
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
        # model name
        filename <- paste("mod",par$modID, sep = "")
        # save model
        save(modnoranef, file = filename)
      }
    }
  }
}  
