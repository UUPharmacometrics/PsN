which_resmod_folders <- function(directory,idv_name) {
  #get resmod folder suffix
  resmod_suffix <- list.files(directory) %>% 
    .[grep("^resmod_",.)] %>%
    sub('.*resmod_','',.)
  
  #order TIME,TAD,PRED
  order <- c(idv_name,"TAD","PRED")
  vpc_captions_all <- c(paste0("VPC of observations (DV) vs. binned time (",idv_name,") before and after correcting for the estimated structural bias by ",idv_name," bin."),
                    "VPC of observations (DV) vs. binned time after dose (TAD) before and after correcting for the estimated structural bias by TAD bin.",
                    "VPC of observations (DV) vs. binned population predictions (PRED) before and after correcting for the estimated structural bias by PRED bin.")
  all_idvs <- c()
  vpc_captions <- c()
  j <- 1
  for (i in 1:length(order)) {
    if(any(resmod_suffix==order[i])) {
      all_idvs[j] <- order[i]
      vpc_captions[j] <- vpc_captions_all[i]
      j <- j + 1
    }
  }
  out <- list(all_idvs=all_idvs,
              vpc_captions=vpc_captions)
  return(out)
}