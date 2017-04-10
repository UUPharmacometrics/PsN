get_covariates_table <- function(frem_table,scm_table,max_scm_table,frem_files_exists,scm_files_exists) {
  if(scm_files_exists) {
    if(frem_files_exists) {
      covariates_extra_table <- rbind(scm_table,c("sum(SCMu)",sum(scm_table[,2])),c("FREM",frem_table[,2]))
      colnames(covariates_extra_table) <- c("Covariate","dofv")
    } else {
      covariates_extra_table <- rbind(scm_table,c("sum(SCMu)",sum(scm_table[,2])),frem_table[,-3])
      colnames(covariates_extra_table) <- c("Covariate","dofv")
    }
  } else {
    if(frem_files_exists) {
      covariates_extra_table <- rbind(scm_table,c("FREM",frem_table[,2]))
      colnames(covariates_extra_table) <- c("Covariate","dofv")
    } else {
      covariates_extra_table <- rbind(scm_table,frem_table[,-3])
      colnames(covariates_extra_table) <- c("Covariate","dofv")
    }
  }
  covariates_table <- rbind(frem_table,max_scm_table)
  return(list(covariates_table=covariates_table,
              covariates_extra_table=covariates_extra_table))
}