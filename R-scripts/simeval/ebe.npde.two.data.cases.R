two.data.cases <- function(ebenpde_tmp) {
  list_case <- list()
  ebenpde_tmp_indiv <- ebenpde_tmp[complete.cases(ebenpde_tmp),] # delete rows with NA values
  rownames(ebenpde_tmp_indiv) <- NULL
  ebenpde_tmp_ETA <- ebenpde_tmp[,!sapply(ebenpde_tmp,function(x) any(is.na(x)))] # delete columns with NA values
  rownames(ebenpde_tmp_ETA) <- NULL
  if((nrow(ebenpde_tmp_indiv) > 0) && (any(grepl("^ETA.",colnames(ebenpde_tmp_ETA))))) {
    list_ebenpde_tmp <- list(ebenpde_tmp_indiv,ebenpde_tmp_ETA)
    list_case <- list(individual_based='Individual based analysis',ETA_based='ETA based analysis')
  } else if((nrow(ebenpde_tmp_indiv) > 0) && !(any(grepl("^ETA.",colnames(ebenpde_tmp_ETA))))) {
    list_ebenpde_tmp <- list(ebenpde_tmp_indiv)
    list_case <- list(individual_based='Individual based analysis')
  } else if((nrow(ebenpde_tmp_indiv) == 0) && (any(grepl("^ETA.",colnames(ebenpde_tmp_ETA))))) {
    list_ebenpde_tmp <- list(ebenpde_tmp_ETA)
    list_case <- list(ETA_based='ETA based analysis')
  }
  
  list_data <- list()
  for (i in 1:length(list_ebenpde_tmp)) {
    ebenpde_tmp_data <- list_ebenpde_tmp[[i]]
    #get ETA names
    ETA.names <- grep("^ETA.",colnames(ebenpde_tmp_data),value=T)
    n.subjects <- nrow(ebenpde_tmp_data)
    # save needed ETA columns in separate data frame ebenpde_obs
    ebenpde_obs <- ebenpde_tmp_data[,ETA.names]
    if(class(ebenpde_obs) == "numeric") {
      ebenpde_obs <- as.data.frame(ebenpde_obs)
      colnames(ebenpde_obs) <- ETA.names
    }
    list_data[[i]] <- list(ebenpde_tmp=ebenpde_tmp_data,
                           n.subjects=n.subjects,
                           iiv.eta.names=ETA.names,
                           ebenpde_obs=ebenpde_obs,
                           case=list_case[[i]])
  }
  return(list_data)
}