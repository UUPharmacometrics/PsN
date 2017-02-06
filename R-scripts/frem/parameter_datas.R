# SORT NEEDED DATA FOR EACH PARAMETER
parameter_datas <- function(inTable_frem,pardata,covdata,covariate) {
  #input
  pardata <- read.csv(pardata, header = T, as.is = T, stringsAsFactors = F)
  inTable_frem <- read.csv(inTable_frem, header = T, as.is = T, stringsAsFactors = F)

  list_info <- list()
  for (j in 1:nrow(pardata)) {
    col.names <- c()
    DF <- data.frame('NA'=matrix(data=NA,nrow=nrow(inTable_frem),ncol=1))
    for (i in 1:length(covariate)) {
      if (covdata$is.categorical[i] != "1") {
        part_5th <- inTable_frem[ , grepl(paste0("^RATIO.par.",pardata$parname[j],".given.cov5th.",covariate[i],"$"), names(inTable_frem))]
        part_95th <- inTable_frem[ , grepl(paste0("^RATIO.par.",pardata$parname[j],".given.cov95th.",covariate[i],"$"), names(inTable_frem))]
        part <- cbind(part_5th,part_95th)
        name_new <- c(paste0(covariate[i],".cov5th"),paste0(covariate[i],".cov95th"))
      } else {
        part <- inTable_frem[ , grepl(paste0("RATIO.par.",pardata$parname[j],".given.other.",covariate[i],"$"), names(inTable_frem))]
        name_new <- paste0(covariate[i],".other")
      }
      col.names <- c(col.names,name_new)
      DF <- cbind(DF,part)
    }
    DF <- DF[,-1] # delete 1 column with NA values, it is not from the inTable_frem data frame
    colnames(DF) <- col.names
    list_info[[j]] <- DF
  } 
  return(list_info)
}
