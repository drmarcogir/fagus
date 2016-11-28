#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Supporting functions for Fagus
# data analysis project. Created
# by Marco Girardello 19/09/2016
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


####################################
# get dataset used for fitting 
# spatially explicit  SEMs
####################################

dataget <- function(inputdf, sac) {
        mod.no <- unique(inputdf$listno)    
    # list of lists
    allsem <- vector("list", length(mod.no))
    names(allsem) <- unique(inputdf$sem.name)    
# individual sem models
for (i in 1:length(mod.no)) { 
  moddf <- subset(inputdf, listno == mod.no[i])
  # results for individual sem model
  if (sac == "FALSE") {
    pathd <- paste(moddf[1, ]$nosacpath, "/datmod", moddf[1, ]$modID,".csv", sep = "")
    dat <- read.csv(pathd)
    allsem[[i]] <-dat  
} else {
    pathd <- paste(moddf[1, ]$sacpath, "/datmod", moddf[1, ]$modID,".csv", sep = "")
    dat <- read.csv(pathd)
    allsem[[i]] <-dat
    
  }
}  
    return(allsem)    
}

####################################
# get SEM models 
####################################

semget <- function(inputdf, sac) {    
    mod.no <- unique(inputdf$listno)   
    # list of lists
    allsem <- vector("list", length(mod.no))
    names(allsem) <- unique(inputdf$sem.name)
    # individual sem models
    for (i in 1:length(mod.no)) {
         moddf <- subset(inputdf, listno == mod.no[i]) 
        # results for individual sem model
        singlesem <- vector("list", dim(moddf)[1])
        if (sac == "FALSE") {
            for (x in 1:dim(moddf)[1]) {
                pathm <- paste(moddf[x, ]$nosacpath, "/mod", moddf[x, ]$modID, sep = "")
                mod <- get(load(pathm))
                singlesem[[x]] <- mod   
            }            
        } else { 
            for (x in 1:dim(moddf)[1]) { 
                pathm <- paste(moddf[x, ]$sacpath, "/mod", moddf[x, ]$modID, sep = "")
                mod <- get(load(pathm))
                singlesem[[x]] <- mod 
            }
        }
        allsem[[i]] <- singlesem 
    }
    return(allsem)
}



#######################################
# SEM bootstrapping support functions
######################################

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

################################
# function for splitting text
#################################

swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)

