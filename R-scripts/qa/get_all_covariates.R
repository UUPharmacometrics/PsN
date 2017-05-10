get_all_covariates <- function(frem_directory,covariates,categorical,parameters,dofv_full_block) {
  if(length(categorical)!=0 || (length(covariates)!=0)) {
    frem_files_exists <- (file.exists(file.path(frem_directory,"model2_modelfit_dir1/raw_results.csv")) && 
                            file.exists(file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv")))
    if(frem_files_exists) {
      ofv_frem_all_cov <- .get_rawres_ofv(file.path(frem_directory,"model2_modelfit_dir1/raw_results.csv"))
      ofv_frem_no_cov <- .get_rawres_ofv(file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv"))
      dofv_frem <- ofv_frem_all_cov - ofv_frem_no_cov
      if(!is.na(dofv_full_block)) {
        dofv_frem <- dofv_frem - dofv_full_block
      }
      frem_table <- data.frame("ALL",dofv_frem,(length(parameters)*(length(categorical)+length(covariates))),stringsAsFactors = F)
      frem_table[,2] <- round(as.numeric(frem_table[,2]), 1)
    } else {
      frem_table <- cbind(error_table("FREM"),"",stringsAsFactors = F)
    }
  } else {
    frem_files_exists <- FALSE
    frem_table <- data.frame("FREM",NA,"",stringsAsFactors = F)
  }
  colnames(frem_table) <- c("","dofv","Add.params")
  return(list(frem_files_exists=frem_files_exists,
              frem_table=frem_table))
}