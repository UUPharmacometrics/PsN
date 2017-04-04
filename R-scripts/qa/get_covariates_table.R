get_covariates_table <- function(frem_table,scm_table,frem_files_exists,scm_files_exists) {
  if(scm_files_exists) {
    scm_table[,2] <- round(as.numeric(scm_table[,2]), 2)
    max_scm_table <- scm_table[which.max(scm_table$dofv),]
    if(frem_files_exists) {
      covariates_extra_table <- rbind(scm_table,c("sum(SCMu)",sum(scm_table[,2])),c("FREM",frem_table[,2]))
      colnames(covariates_extra_table) <- c("Covariate","dofv")
    } else {
      covariates_extra_table <- rbind(scm_table,c("sum(SCMu)",sum(scm_table[,2])),frem_table)
      colnames(covariates_extra_table) <- c("Covariate","dofv")
    }
  } else {
    max_scm_table <- scm_table
    if(frem_files_exists) {
      covariates_extra_table <- rbind(scm_table,c("FREM",frem_table[,2]))
      colnames(covariates_extra_table) <- c("Covariate","dofv")
    } else {
      covariates_extra_table <- rbind(scm_table,frem_table)
      colnames(covariates_extra_table) <- c("Covariate","dofv")
    }
  }
  covariates_table <- rbind(frem_table,max_scm_table)
  return(list(covariates_table=covariates_table,
              covariates_extra_table=covariates_extra_table))
}