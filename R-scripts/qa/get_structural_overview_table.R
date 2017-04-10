get_structural_overview_table <- function(directory,idv,groups) {
  if(length(length(idv))!=0) {
    structural_overview <- as.data.frame(array(0,c(length(idv),3)))
    colnames(structural_overview) <- c("","dofv","Add.params")
    for(i in 1:length(idv)) {
      structural_overview[i,1] <- idv[i]
      structural_overview[i,2] <- get_resmod_structural_dofv(directory, idv[i])
      structural_overview[i,3] <- groups-1
    }
  } else {
    structural_overview <- error_table("RESMOD")
  }

  return(structural_overview)
}