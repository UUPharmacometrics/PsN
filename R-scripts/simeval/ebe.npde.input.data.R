input.data <- function(ebe.npde.file,n.eta,all.eta.names) {
  # read in 
  ebenpde_tmp <- read.csv(ebe.npde.file) # load csv file
  n.subjects <- nrow(ebenpde_tmp) # amount of samples
  # save all ETA columns in separate data frame ebenpde_obs
  ebenpde_obs <- ebenpde_tmp[,seq(3,n.eta+2)]
  if(class(ebenpde_obs) == "numeric") {
    ebenpde_obs <- as.data.frame(ebenpde_obs)
  }
  # output
  out <- list(ebenpde_tmp=ebenpde_tmp,
              n.subjects=n.subjects,
              ebenpde_obs=ebenpde_obs)
  return(out)
}
