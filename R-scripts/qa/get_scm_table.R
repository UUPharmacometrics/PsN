get_scm_table <- function(scm_directory,parameters,continuous,categorical,skip,quiet=F){
  rawres_file <- file.path(scm_directory,"raw_results_scm.csv")
  scm_files_exists <- file.exists(rawres_file)
  if(any(skip=="scm")) {
    scm_table <- data.frame("SCM","SKIPPED",stringsAsFactors = F)
    colnames(scm_table) <- c("","dOFV")
    max_scm_table <- cbind(scm_table,"",stringsAsFactors = F)
  } else {
    if(length(parameters)!=0 && (length(categorical)!=0 || (length(continuous)!=0))) {
      if(scm_files_exists) {
        scm_table <- read.csv(rawres_file,stringsAsFactors = F) %>%
          dplyr::mutate(dOFV = ofv[step.number==0]-ofv) %>%
          dplyr::slice(-1) %>%
          dplyr::select(relation, dOFV)
        colnames(scm_table) <- c("","dOFV")
        #max_table
        if(all(is.na(scm_table$dOFV))) {
          if(!quiet) {
            message("WARNING: In file ",rawres_file," all dofv values are NA.")
          }
          max_scm_table <- cbind(error_table("SCM"),"",stringsAsFactors = F)
        } else {
          max_scm_table <- cbind(scm_table[which.max(scm_table$dOFV),],1)
        }
        
        # add coefficient
        scm_table_coef <- read.csv(rawres_file,stringsAsFactors = F) %>%
          dplyr::slice(-1)
        column_names <- paste0(scm_table_coef$relation,"-1")
        column_names <- gsub("\\-","\\.",column_names)
        coef_table <- scm_table_coef[,which(colnames(scm_table_coef) %in% column_names)]
        scm_table <- cbind(scm_table,"Coef"=as.numeric(as.character(rep(NA,nrow(scm_table)))))
        for(i in 1:length(coef_table)) {
          row_nr <- which(paste0(gsub("\\-","\\.",scm_table[,1]),".1") == colnames(coef_table[i]))
          if(!all(is.na(coef_table[i]))) {
            scm_table$Coef[row_nr] <- as.numeric(coef_table[!is.na(coef_table[i]),i])
          }
        }
        colnames(scm_table) <- c("","dOFV","Coef")
      } else {
        if(!quiet) {
          message("WARNING: File ",rawres_file," not found!")
        }
        scm_table <- error_table("SCM")
        max_scm_table <- cbind(scm_table,"",stringsAsFactors = F)
      }
    } else {
      if(!quiet) {
        message("WARNING: Parameters and covariates vectors are empty!")
      }
      scm_files_exists <- FALSE
      scm_table <- data.frame("SCM","NA",stringsAsFactors = F)
      colnames(scm_table) <- c("","dOFV")
      max_scm_table <- cbind(scm_table,"",stringsAsFactors = F)
    }
  }
  
  colnames(max_scm_table) <- c("","dOFV","Add.params")
  return(list(scm_files_exists=scm_files_exists,
              scm_table=scm_table,
              max_scm_table=max_scm_table))
}