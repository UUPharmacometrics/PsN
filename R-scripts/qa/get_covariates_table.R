get_covariates_table <- function(frem_table,scm_table,max_scm_table) {
  covariates_table <- rbind(c("FREM",frem_table[,2:3]),max_scm_table,stringsAsFactors=F)
  if(any(c("ERROR","NA","SKIPPED")==scm_table$dOFV[1]) && length(scm_table$dOFV)==1) {
    if(any(c("ERROR","NA","SKIPPED")==frem_table$dOFV)) {
      covariates_extra_table <- rbind(scm_table,c("FREM",frem_table$dOFV),stringsAsFactors=F)
    } else {
      covariates_extra_table <- rbind(scm_table,c("FREM",format(round(as.numeric(frem_table$dOFV),2),digits=1,nsmall = 1)),stringsAsFactors=F)
    }
    colnames(covariates_extra_table) <- c("Covariate","dOFV")
  } else {
    sum_scm_dofv <- sum(as.numeric(scm_table$dOFV[!is.na(scm_table$dOFV)]))
    scm_table$Coef <- format(round(scm_table$Coef,3),trim=T,digits=1,nsmall=2,scientific=F)
    # if(any(colnames(scm_table)=="Coef_sd")) {
    #   scm_table$Coef_sd <- format(round(scm_table$Coef_sd,3),trim=T,digits=1,nsmall=2,scientific=F)
    # }
    covariates_extra_table <- rbind(scm_table,c("sum(SCMu)",sum_scm_dofv,rep("",ncol(scm_table)-2)),
                                    stringsAsFactors=F)
    # if(any(colnames(scm_table)=="Coef_sd")) {
    #   colnames(covariates_extra_table) <- c("Covariate","dOFV","Coefficient","Coefficient/SD")
    # } else {
      colnames(covariates_extra_table) <- c("Covariate","dOFV","Coefficient")
    # }

    if(class(frem_table$dOFV)=="character") {
      covariates_extra_table$dOFV <- format(round(as.numeric(covariates_extra_table$dOFV),2),trim=T,digits=1,nsmall=1,scientific=F)
      covariates_extra_table <- rbind(covariates_extra_table,c("FREM",frem_table$dOFV,rep("",ncol(scm_table)-2)),
                                      stringsAsFactors=F)
    } else {
      covariates_extra_table <- rbind(covariates_extra_table,c("FREM",frem_table$dOFV,rep("",ncol(scm_table)-2)),
                                      stringsAsFactors=F)
      covariates_extra_table$dOFV <- format(round(as.numeric(covariates_extra_table$dOFV),2),trim=T,digits=1,nsmall=1,scientific=F)
    }

  }
  return(list(covariates_table=covariates_table,
              covariates_extra_table=covariates_extra_table))
}