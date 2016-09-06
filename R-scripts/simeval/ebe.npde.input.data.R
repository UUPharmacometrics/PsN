#' Simeval (input.data.R)
#' @param ebe.npde.file: The directory where the input data frame is saved.
#' @param n.eta: Amount of the ETA columns.
#' @return ebenpde_tmp: The input data frame.
#' @description The function input.data reads in a csv file and saves it in the argument ebenpde_tmp. All "ETA" columns are also saved in a separate data frame ebenpde_obs.
#' @example input.data(ebe.npde.file="../../simeval/ebe.npde, n.eta=2)
#' @export
input.data <- function(ebe.npde.file,n.eta,all.eta.names) {
  # read in 
  ebenpde_tmp <- read.csv(ebe.npde.file) # load csv file
  n.subjects <- nrow(ebenpde_tmp) # amount of samples
  # save all ETA columns in separate data frame ebenpde_obs
  ebenpde_obs <- ebenpde_tmp[,seq(3,n.eta+2)]
  # output
  out <- list(ebenpde_tmp=ebenpde_tmp,
              n.subjects=n.subjects,
              ebenpde_obs=ebenpde_obs)
  return(out)
}