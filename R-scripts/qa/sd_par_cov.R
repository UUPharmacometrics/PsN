sd_par_cov <- function(filename) {
  sd_parcov_file_exists <- file.exists(filename)
  if(sd_parcov_file_exists) {
    sd_tab_input <- read.csv(filename)
    # get parameter and covariate names
    sd_parcov_tab <- as.data.frame(array(NA,c(nrow(sd_tab_input),3)))
    colnames(sd_parcov_tab) <- c("Parameter","Covariate","sd")
    for(i in 1:nrow(sd_tab_input)) {
      param <- sub('.([^.]*)$','',sd_tab_input[i,1])
      covar <- sub('.*\\.','',sd_tab_input[i,1])
      sd_parcov_tab[i,1] <- param
      sd_parcov_tab[i,2] <- covar
      sd_parcov_tab[i,3] <- as.character(round(sd_tab_input[i,2],3))
    }
    out <- list(sd_parcov_file_exists=sd_parcov_file_exists,
                sd_parcov_tab=sd_parcov_tab)
  } else {
    out <- list(sd_parcov_file_exists=sd_parcov_file_exists)
  }
  
  return(out)
}