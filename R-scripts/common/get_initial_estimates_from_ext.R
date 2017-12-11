#part can be "omega","theta","sigma" or "all"(means thetas, omegas and sigmas)
get_initial_estimates_from_ext <- function(filename,select="all",iteration=-1000000000,do.stop=TRUE) {
  init.est <- read.table(filename,header=TRUE,skip=1,stringsAsFactors = F) %>%
    dplyr::filter(ITERATION==iteration)
  if(select=="all") {
    init.est <- init.est %>% dplyr::select(grep("(^OMEGA|^THETA|^SIGMA)",colnames(.)))
  } else if(select=="omega") {
    init.est <- init.est %>% dplyr::select(grep("^OMEGA",colnames(.)))
  } else if(select=="theta") {
    init.est <- init.est %>% dplyr::select(grep("^THETA",colnames(.)))
  } else if(select=="sigma") {
    init.est <- init.est %>% dplyr::select(grep("^SIGMA",colnames(.)))
  } else {
    message("Argument 'select' can be set only to strings 'all', 'omega', 'theta' or 'sigma'.")
    if(do.stop) {
      stop()
    }
  }
  if(ncol(init.est)==0) {
    init.est <- data.frame()
  }
  return(init.est)  
}