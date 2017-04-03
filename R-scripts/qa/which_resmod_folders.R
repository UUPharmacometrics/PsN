which_resmod_folders <- function(directory) {
  all_folders <- list.files(directory)
  structures <- c("idv","PRED","TAD")
  j <- 0
  resmod_suffix <- list()
  for(i in 1:length(structures)) {
    if(any(all_folders==paste0("resmod_",structures[i]))) {
      j <- j + 1
      resmod_suffix[[j]] <- structures[i]
    }
  }
  return(resmod_suffix)
}