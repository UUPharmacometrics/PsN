get_all_covariates <- function(frem_directory) {
  frem_files_exists <- TRUE
  if(file.exists(file.path(frem_directory,"model2_modelfit_dir1/raw_results.csv")) &&
     file.exists(file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv"))) {
    
    ofv_frem_all_cov <- .get_rawres_ofv(file.path(frem_directory,"model2_modelfit_dir1/raw_results.csv"))
    ofv_frem_no_cov <- .get_rawres_ofv(file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv"))
    dofv_frem <-  ofv_frem_no_cov - ofv_frem_all_cov
    frem_table <- data.frame("ALL",dofv_frem)
    colnames(frem_table) <- c("","dofv")
    frem_table[,2] <- round(as.numeric(frem_table[,2]), 2)
  } else {
    frem_files_exists <- FALSE
    frem_table <- error_table("FREM")
  }
  
  return(list(frem_files_exists=frem_files_exists,
              frem_table=frem_table))
}