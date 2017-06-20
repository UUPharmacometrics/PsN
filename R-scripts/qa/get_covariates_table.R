get_covariates_table <- function(frem_table,scm_table,max_scm_table) {
  if((scm_table[1,2] == "ERROR") || is.na(scm_table[1,2])) {
    covariates_extra_table <- rbind(scm_table,c("FREM",frem_table[,2]))
    colnames(covariates_extra_table) <- c("Covariate","dofv")
  } else {
    covariates_extra_table <- rbind(scm_table,c("sum(SCMu)",sum(scm_table[,2]),rep("",ncol(scm_table)-2)),c("FREM",frem_table[,2],rep("",ncol(scm_table)-2)))
    colnames(covariates_extra_table) <- c("Covariate","dofv","Coefficient")
  }
  covariates_table <- rbind(frem_table,max_scm_table)
  return(list(covariates_table=covariates_table,
              covariates_extra_table=covariates_extra_table))
}