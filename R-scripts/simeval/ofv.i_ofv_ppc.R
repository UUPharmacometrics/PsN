i_ofv_ppc <- function(all.iofv.file,samples,outlier_ID) {
if (length(outlier_ID) > 0) {
  # iOFV PPC
  all.iOFV_sim <- read.csv(all.iofv.file)
  outlier_data <- all.iOFV_sim[all.iOFV_sim$ID %in% outlier_ID,]
  rownames(outlier_data) <- NULL
  # make a list of data for each outlier
  list_i_ofv_ppc <- list()
  list_data <- list()
  for (i in 1:nrow(outlier_data)) {
    iOFV_sim <- outlier_data[i,3:(samples+2)]
    list_data$iOFV_sim <- iOFV_sim[!is.na(iOFV_sim)]
    list_data$iOFV_obs <- outlier_data[i,2]
    len <- length(list_data$iOFV_sim)
    list_data$sort_iOFV_sim <- sort(list_data$iOFV_sim)
    # max and min values on the x axes
    list_data$newxlim <- c(list_data$sort_iOFV_sim[1],list_data$sort_iOFV_sim[len]) 
    if(list_data$iOFV_obs > list_data$sort_iOFV_sim[len]){
      list_data$newxlim <- c(list_data$sort_iOFV_sim[1],list_data$iOFV_obs)
      }
    if(list_data$iOFV_obs < list_data$sort_iOFV_sim[1]){
      list_data$newxlim <- c(list_data$iOFV_obs,list_data$sort_iOFV_sim[len])
      }
    list_i_ofv_ppc[[i]] <- list_data
  }
  list_i_ofv_ppc$outlier_data <- outlier_data
  list_i_ofv_ppc$all.iOFV_sim <- all.iOFV_sim
  #output
  return(list_i_ofv_ppc)  
  }
}


