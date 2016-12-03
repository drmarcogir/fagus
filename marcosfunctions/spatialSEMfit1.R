#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Fit spatial SEM models as 
# separate regressions 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


fitsem_spatial1<-function(modlist,modform,neigh.l,coorn){
  # store results of correlogram
  results<-NULL 
  for (x in 1:dim(modlist)[1]){
    # formula
    info<-modlist[x,]
    form<-as.formula(as.character(info$formula))
    toprint<-paste("model",x)
    cat(toprint)  
    cat("\n")
    # get dataset
    tmpdat<-get(as.character(info$dataset))
    for (i in 1:length(neigh.l)){
      toprint1<-paste("neighbour",i)
      cat(toprint1)  
      cat("\n")
      # extract coordinates
      coor<-as.matrix(tmpdat[,coorn])
      IDs<-row.names(coor)
      # define neighbour
      step1<-knearneigh(coor, k=neigh.l[i])
      step2<-knn2nb(step1)
      # create weights
      nbw<-nb2listw(step2,style="W")
      # fit model
      mod<-errorsarlm(form,nbw,data=tmpdat,zero.policy=TRUE,method="eigen")
      # save model
      path<-paste(info$sem.name1,"_n",i,sep="")
      save(mod,file=path)
      # calculate correlogram
      cormg <- correlog(coords = coor, z = residuals(mod), method = "Moran")
      # store correlogram results
      cormgdf<-data.frame(cormg,neigh=neigh.l[i],title = info$formula_title,modID=info$modID,sem.name=info$sem.name2)
      results<-rbind(cormgdf,results)
    }
  }
  return(results)  
}
