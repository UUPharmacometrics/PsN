get_all_covariates <- function(frem_directory,continuous,categorical,parameters,dofv_full_block,skip,quiet=F) {
  if(any(skip=="frem")) {
    frem_files_exists <- FALSE
    frem_table <- data.frame("FREM","SKIPPED","",stringsAsFactors = F)
  } else {
    if(length(categorical)!=0 || (length(continuous)!=0)) {
      frem_files_exists <- (file.exists(file.path(frem_directory,"model2_modelfit_dir1/raw_results.csv")) && 
                              file.exists(file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv")))
      if(frem_files_exists) {
        ofv_frem_all_cov <- .get_rawres_ofv(file.path(frem_directory,"model2_modelfit_dir1/raw_results.csv"))
        ofv_frem_no_cov <- .get_rawres_ofv(file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv"))
        if(all(!is.na(ofv_frem_all_cov) && !is.na(ofv_frem_no_cov))) {
          dofv_frem <- ofv_frem_all_cov - ofv_frem_no_cov
        } else {
          dofv_frem <- "ERROR"
        }
        if(class(dofv_full_block)!="character" && class(dofv_frem)!="character") {
          dofv_frem <- dofv_frem - dofv_full_block
        }
        frem_table <- data.frame("ALL",dofv_frem,(length(parameters)*(length(categorical)+length(continuous))),stringsAsFactors = F)
      } else {
        if(!file.exists(file.path(frem_directory,"model2_modelfit_dir1/raw_results.csv")) && !quiet) {
          message("WARNING: File ",file.path(frem_directory,"model2_modelfit_dir1/raw_results.csv")," not found!")
        }
        if(!file.exists(file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv")) && !quiet) {
          message("WARNING: File ",file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv")," not found!")
        }
        frem_table <- cbind(error_table("FREM"),"",stringsAsFactors = F)
      }
    } else {
      if(!quiet) {
        message("WARNING: Both continuous and categorical covariate vectors are empty!")
      }
      frem_files_exists <- FALSE
      frem_table <- data.frame("FREM","NA","",stringsAsFactors = F)
    }
  }
  colnames(frem_table) <- c("","dOFV","Add.params")
  return(list(frem_files_exists=frem_files_exists,
              frem_table=frem_table))
}