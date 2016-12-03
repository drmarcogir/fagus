#################################
# Function for calculating
# Moran's I correlograms starting
# from piewise SEM models
################################

# calculate spatial autocorrelation 
sacmg <- function(inputdf, inpath) {
    results <- NULL
    for (i in 1:dim(inputdf)[1]) {
        # get data
        info <- inputdf[i, ]
        # get model
        filen <- paste(inpath, "/mod", info$modID, sep = "")
        tmpmod <- get(load(filen))
        # get data
        filedat <- paste(inpath, "/datmod", info$modID, ".csv", sep = "")
        dat <- read.csv(filedat)
        # coordinates
        mycoor <- as.matrix(dat[, c("UTMx", "UTMy")])
        # create correlogram
        cormg <- correlog(coords = mycoor, z = residuals(tmpmod), method = "Moran")
        # save correlogram
        filen <- paste("corr_mod", info$modID, sep = "")
        save(cormg, file = filen)
        # create final data frame for storing results
        tmpdf <- data.frame(cormg, title = info$formula_title,modID=info$modID,sem.name=info$sem.name2)
        tmpdf$title = swr(tmpdf$title)
        results <- rbind(tmpdf, results)
    }
    return(results)
    
}



