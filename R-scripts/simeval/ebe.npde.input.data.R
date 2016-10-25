input.data <- function(ebe.npde.file,iiv.eta.names) {
  # read in data
  ebenpde_tmp <- read.csv(ebe.npde.file) # load csv file
  n.subjects <- nrow(ebenpde_tmp)
  
  # rename ETA(n) to ETA.n. because in dataframe names of ETA columns are with dots
  iiv.eta.names <- gsub("\\(",".",iiv.eta.names)
  iiv.eta.names <- gsub("\\)",".",iiv.eta.names)
  # save needed ETA columns in separate data frame ebenpde_obs
  ebenpde_obs <- ebenpde_tmp[,iiv.eta.names]
  if(class(ebenpde_obs) == "numeric") {
    ebenpde_obs <- as.data.frame(ebenpde_obs)
    colnames(ebenpde_obs) <- iiv.eta.names
  }

  #create ebenpde_tmp data frame
  ebenpde_tmp <- ebenpde_tmp[,-grep("ETA.",colnames(ebenpde_tmp))]
  ebenpde_tmp <- cbind(ebenpde_tmp,ebenpde_obs)
  # output
  out <- list(ebenpde_tmp=ebenpde_tmp,
              n.subjects=n.subjects,
              ebenpde_obs=ebenpde_obs,
              iiv.eta.names=iiv.eta.names,
              case='')
  return(out)
}
