which_resmod_folders <- function(directory,idv_name,quiet=F) {
  #get resmod folder suffix
  resmod_suffix <- list.files(directory) %>% 
    .[grep("^resmod_",.)] %>%
    sub('.*resmod_','',.)
  
  #order TIME,TAD,PRED
  order <- c(idv_name,"TAD","PRED")
  order <- unique(order)
  
  all_idvs <- c()
  j <- 1
  for (i in 1:length(order)) {
    if(any(resmod_suffix==order[i])) {
      all_idvs[j] <- order[i]
      j <- j + 1
    }
  }
  #print message if no resmod folders found
  if(is.null(all_idvs) && !quiet) {
    message("WARNING: No RESMOD run folders found in the directory '",directory,"'")
  }

  return(all_idvs)
}