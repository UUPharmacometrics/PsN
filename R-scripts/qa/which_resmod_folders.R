which_resmod_folders <- function(directory) {
  #get resmod folder suffix
  resmod_suffix <- list.files(directory) %>% 
    .[grep("^resmod_",.)] %>%
    sub('.*resmod_','',.)
  
  return(resmod_suffix)
}