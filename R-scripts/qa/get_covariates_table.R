get_covariates_table <- function(frem_table,scm_table,max_scm_table) {
  covariates_table <- rbind(c("FREM",frem_table[2:3]),max_scm_table,stringsAsFactors=F)
  if(scm_table$dOFV[1] == "ERROR" || scm_table$dOFV[1] == "NA") {
    covariates_extra_table <- rbind(scm_table,c("FREM",format(frem_table$dOFV,digits=1,nsmall = 1)),stringsAsFactors=F)
    colnames(covariates_extra_table) <- c("Covariate","dOFV")
  } else {
    sum_scm_dofv <- sum(as.numeric(scm_table$dOFV))
    scm_table$Coef <- format(scm_table$Coef,trim=T,digits=1,nsmall=2)
    covariates_extra_table <- rbind(scm_table,c("sum(SCMu)",sum_scm_dofv,rep("",ncol(scm_table)-2)),
                                    stringsAsFactors=F)
    colnames(covariates_extra_table) <- c("Covariate","dOFV","Coefficient")
    if(class(frem_table$dOFV)=="character") {
      covariates_extra_table$dOFV <- format(as.numeric(covariates_extra_table$dOFV),trim=T,digits=1,nsmall=1)
      covariates_extra_table <- rbind(covariates_extra_table,c("FREM",frem_table$dOFV,rep("",ncol(scm_table)-2)),
                                      stringsAsFactors=F)
    } else {
      covariates_extra_table <- rbind(covariates_extra_table,c("FREM",frem_table$dOFV,rep("",ncol(scm_table)-2)),
                                      stringsAsFactors=F)
      covariates_extra_table$dOFV <- format(as.numeric(covariates_extra_table$dOFV),trim=T,digits=1,nsmall=1)
    }

  }
  return(list(covariates_table=covariates_table,
              covariates_extra_table=covariates_extra_table))
}