cov.cook.par.data <- function(cdd.data.all) {
   # create data frame of the cook.par data
   cook.par.data <- cdd.data.all[ , grepl("^cook.par.", names(cdd.data.all))]
   cook.par.data <- subset(cook.par.data[-1,])
   row.names(cook.par.data) <- NULL
   # get names of all parameters
   parameters <- gsub("\\cook.par.", "",names(cook.par.data))
   
   # create data frame of the cov.par data
   cov.par.data.col <- array(0,c(nrow(cdd.data.all),length(parameters)))
   names.cov.par.data <- c()
   for (i in 1:length(parameters)) {
     # choose needed columns
     se.par.data.col <- as.vector(cdd.data.all[ , grep(paste0("^se",paste0(parameters[i],"$")), names(cdd.data.all))])
     # calculate covariances for each parameter
     cov.par.data.col[,i] <- as.vector(se.par.data.col/se.par.data.col[1])
     names.cov.par.data[i] <- paste0("cov.par.",parameters[i])
   }
   cov.par.data <- subset(as.data.frame(cov.par.data.col)[-1,])
   row.names(cov.par.data) <- NULL
   colnames(cov.par.data) <- c(names.cov.par.data)
   
return(list(cook.par.data=cook.par.data,cov.par.data=cov.par.data,parameters=parameters)) 
}

