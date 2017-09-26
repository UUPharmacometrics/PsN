get_boxcox_lambda_table <- function(directory,dofv_boxcox) {
  #get lambda values (extra table for box-cox transformation)
  if(file.exists(file.path(directory,"modelfit_run/boxcox.ext")) && file.exists(file.path(directory,"linearize_run/scm_dir1/derivatives.ext"))) {
    ext_file <- read.table((file.path(directory,"modelfit_run/boxcox.ext")),header=TRUE,skip=1,stringsAsFactors = F) %>%
      dplyr::filter(ITERATION==-1000000000)
    lambdas <- ext_file[grep("^THETA[0-9]+$",colnames(ext_file))]
    new_omega_values <- ext_file[,grep("^OMEGA",colnames(ext_file))]
    #new_omega_values <- new_omega_values[,which(new_omega_values!=0)]
    
    # omega values from original model
    derivatives_table <-read.table((file.path(directory,"linearize_run/scm_dir1/derivatives.ext")),header=TRUE,skip=1,stringsAsFactors = F) %>%
      dplyr::filter(ITERATION==-1000000000)
    deriv_omega_values <- derivatives_table[,grep("^OMEGA",colnames(derivatives_table))]
    #deriv_omega_values <- deriv_omega_values[,which(deriv_omega_values!=0)]
    
    boxcox_lambdas_table <- as.data.frame(array(0,c(length(lambdas),4)))
    colnames(boxcox_lambdas_table) <- c("","Lambda","New SD","Old SD")
    for(i in 1:length(lambdas)) {
      nr <- sub('*THETA','',colnames(lambdas)[i])
      boxcox_lambdas_table[i,1] <- paste0("ETA(",nr,")")
      boxcox_lambdas_table[i,2] <- lambdas[i]
      boxcox_lambdas_table[i,3] <- sqrt(new_omega_values[,which(colnames(new_omega_values)==paste0("OMEGA.",nr,".",nr,"."))])
      boxcox_lambdas_table[i,4] <- sqrt(deriv_omega_values[,which(colnames(deriv_omega_values)==paste0("OMEGA.",nr,".",nr,"."))])
    }
    boxcox_lambdas_orig <- boxcox_lambdas_table
    colnames(boxcox_lambdas_orig) <- c("ETA","Lambda","New SD","Old SD")
    boxcox_lambdas_table[,2] <- format(as.numeric(boxcox_lambdas_table[,2]),digits=1,trim=T,scientific = F,nsmall=2)
    boxcox_lambdas_table[,3] <- format(as.numeric(boxcox_lambdas_table[,3]),digits=1,trim=T,scientific = F,nsmall=2)
    boxcox_lambdas_table[,4] <- format(as.numeric(boxcox_lambdas_table[,4]),digits=1,trim=T,scientific = F,nsmall=2)
    if(class(dofv_boxcox)!="character") {
      boxcox_lambdas_table <- rbind(boxcox_lambdas_table,c("dOFV",format(dofv_boxcox,digits=1,scientific=F,nsmall=1),"",""))
    }
    
  } else {
    boxcox_lambdas_table <- error_table(col=1)
    boxcox_lambdas_orig <- boxcox_lambdas_table
  }
  out <- list(boxcox_lambdas_table=boxcox_lambdas_table,
              boxcox_lambdas_orig=boxcox_lambdas_orig)
  return(out)
}


