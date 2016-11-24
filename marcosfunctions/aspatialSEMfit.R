 

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

