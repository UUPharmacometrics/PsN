get_overview_table <- function(structural_overview,param_var_overview,covariates_overview,resmod_ruv_overview,infl_indiv_overview,outliers_overview) {
  overview_list <- list(structural_overview,param_var_overview,covariates_overview,resmod_ruv_overview,infl_indiv_overview,outliers_overview)
  rgroup_names <- c("Structural Model","Parameter Variability Model","Covariates","Residual Error Model","Influential Individuals","Outliers")
  n.rgroup <- c()
  for(i in 1:length(overview_list)) {
    if(ncol(overview_list[[i]])!=3) {
      overview_list[[i]] <- cbind(overview_list[[i]],rep("",nrow(overview_list[[i]])))
      colnames(overview_list[[i]]) <- c("","dOFV","Add.params")
    }
    n.rgroup[i] <- nrow(overview_list[[i]])
    if(i == 1) {
      overview_table <- overview_list[[i]]
    } else {
      overview_table <- rbind(overview_table,overview_list[[i]])
    }
  }
  rownames(overview_table) <- NULL
  colnames(overview_table) <- c("","dOFV","Additional parameters")
  
  #get only numeric values
  j <- 0
  dofv_values <- c()
  for(i in 1:nrow(overview_table)){
    if(overview_table$dOFV[i]!="ERROR" && overview_table$dOFV[i]!="NA" && overview_table$dOFV[i]!="") {
      j <- j + 1
      dofv_values[j] <- as.numeric(overview_table$dOFV[i])
    }
  }
  if(length(dofv_values)>0) {
    dofv_values <- format(round(dofv_values,2),trim=T,digits=1,nsmall=1,scientific = F)
  }
  # format all dofv values in overview table
  j <- 0
  for(i in 1:nrow(overview_table)){
    if(overview_table$dOFV[i]!="ERROR" && overview_table$dOFV[i]!="NA" && overview_table$dOFV[i]!="") {
      j <- j + 1
      overview_table$dOFV[i] <- dofv_values[j]
    }
  }
  
  return(list(overview_table=overview_table,
              n.rgroup=n.rgroup,
              rgroup_names=rgroup_names))
}