get_param_var_tables <- function(directory,n.eta) {
  #for overview table
  param_var_file_exists <- TRUE
  if(file.exists(paste0(directory,"linearize_run/",sub('.mod.*','',model.filename),"_linbase.ext")) &&
     file.exists(paste0(directory,"modelfit_run/raw_results.csv"))) {
    
    linbase_ofv <- .get_ext_ofv(paste0(directory,"linearize_run/",sub('.mod.*','',model.filename),"_linbase.ext"))
    linblock_ofv <- .get_rawres_ofv(paste0(directory,"modelfit_run/raw_results.csv"))
    dofv_block <- linbase_ofv-linblock_ofv
    linbox_ofv <- .get_rawres_ofv(paste0(directory,"modelfit_run/raw_results.csv"),row=2)
    dofv_box <- linbase_ofv - linbox_ofv
    linaddeta_ofv <- .get_rawres_ofv(paste0(directory,"modelfit_run/raw_results.csv"),row=3)
    dofv_additional_eta <- linbase_ofv - linaddeta_ofv
    par_var_models <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA"), 
                                 c(dofv_block, dofv_box, dofv_additional_eta)
    )
    colnames(par_var_models) <- c("","dofv")
    par_var_models$dofv <- round(as.numeric(par_var_models[,2]), 2)
  } else {
    param_var_file_exists <- FALSE
    par_var_models <- error_table(c("Full OMEGA Block","Box-cox Transformation","Additional ETA"))
  }
  
  #get lambda values (extra table for box-cox transformation)
  rr_file_exists <- file.exists(paste0(directory,"modelfit_run/raw_results.csv"))
  if(rr_file_exists) {
    boxcox_raw_results <- read.csv(paste0(directory,"modelfit_run/raw_results.csv"))
    col_nr <- which(names(boxcox_raw_results)=="ofv")
    lambdas <- boxcox_raw_results[2,c((col_nr+1):(col_nr+n.eta))] # second row is a boxcox results
    boxcox_lambdas_table <- as.data.frame(array(0,c(length(lambdas),2)))
    colnames(boxcox_lambdas_table) <- c("","Lambda")
    for(i in 1:length(lambdas)) {
      boxcox_lambdas_table[i,1] <- paste0("ETA(",i,")")
      boxcox_lambdas_table[i,2] <- lambdas[i]
    }
    if(param_var_file_exists) {
      boxcox_lambdas_table <- rbind(boxcox_lambdas_table,c("dofv",dofv_box))
    }
    boxcox_lambdas_table[,2] <- round(as.numeric(boxcox_lambdas_table[,2]), 2)
    
  } else {
    boxcox_lambdas_table <- error_table(col=1)
  }
  
  #get full omega block extra table
  if(rr_file_exists) {
    full_block_table <- read.csv(paste0(directory,"modelfit_run/raw_results.csv")) %>%
      slice(1) 
    omega_values <- full_block_table[,grep("^OMEGA",colnames(full_block_table))]
    
    # create a table
    full_omega_block_table <- as.data.frame(array(NA,c(length(omega_values),2)))
    colnames(full_omega_block_table) <- c("","Value")
    for(i in 1:length(omega_values)) {
      numeration <- sub('.*OMEGA.','',colnames(omega_values[i]))
      numeration <- substr(numeration, 1, nchar(numeration)-1) # delete last element in string
      first <- sub('\\..*','',numeration)
      second <- sub('.*\\.','',numeration)
      if(first==second) {
        full_omega_block_table[i,1] <- paste0("sd(",first,")")
      } else {
        
        full_omega_block_table[i,1] <- paste0("corr(",min(first,second),",",max(first,second),")")
      }
      full_omega_block_table[i,2] <- omega_values[i]
    }
    full_omega_block_table[,2] <- round(as.numeric(full_omega_block_table[,2]),2)
    
    if(param_var_file_exists) {
      full_omega_block_table <- rbind(full_omega_block_table,c("dofv",round(dofv_block,2)))
    }
    
  } else {
    full_omega_block_table <- error_table(col=1)
  }
  
  return(list(param_var_file_exists=param_var_file_exists,
              rr_file_exists=rr_file_exists,
              par_var_models=par_var_models,
              boxcox_lambdas_table=boxcox_lambdas_table,
              full_omega_block_table=full_omega_block_table))
}