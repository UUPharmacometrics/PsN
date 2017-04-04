get_structural_overview_table <- function(directory,idv) {
  structural_overview <- as.data.frame(array(0,c(length(idv),2)))
  colnames(structural_overview) <- c("","dofv")
  for(i in 1:length(idv)) {
    structural_overview[i,1] <- idv[i]
    structural_overview[i,2] <- get_resmod_structural_dofv(directory, idv[i])
  }
  return(structural_overview)
}