get_scm_table <- function(rawres_file,parameters,covariates,categorical){
  scm_files_exists <- file.exists(rawres_file)
  if(scm_files_exists) {
    if(length(parameters)!=0 && (length(categorical)!=0 || (length(covariates)!=0))) {
      scm_table <- read.csv(rawres_file,stringsAsFactors = F) %>%
        mutate(dofv = ofv[step.number==0]-ofv) %>%
        slice(-1) %>%
        select(relation, dofv)
        colnames(scm_table) <- c("","dofv")
      #max_table
      scm_table[,2] <- round(as.numeric(scm_table[,2]), 1)
      max_scm_table <- cbind(scm_table[which.max(scm_table$dofv),],1)
      max_scm_table[,2] <- round(max_scm_table[,2],1)
    } else {
      scm_files_exists <- FALSE
      scm_table <- data.frame("SCM",NA,stringsAsFactors = F)
      colnames(scm_table) <- c("","dofv")
      max_scm_table <- cbind(scm_table,"")
    }
  } else {
    scm_table <- error_table("SCM")
    max_scm_table <- scm_table
    max_scm_table <- cbind(scm_table,"")
  }
  colnames(max_scm_table) <- c("","dofv","Add.params")
  return(list(scm_files_exists=scm_files_exists,
              scm_table=scm_table,
              max_scm_table=max_scm_table))
}