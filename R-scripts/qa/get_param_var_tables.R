get_param_var_tables <- function(directory) {
  #for overview table
  param_var_file_exists <- TRUE
  if(file.exists(paste0(directory,"linearize_run/",sub('.mod.*','',model.filename),"_linbase.ext")) &&
     file.exists(paste0(directory,"modelfit_run"))) {
    
    linbase_ofv <- .get_ext_ofv(paste0(directory,"linearize_run/",sub('.mod.*','',model.filename),"_linbase.ext"))

    #full omega block
    if(file.exists(paste0(directory,"modelfit_run/NM_run1/psn.ext"))) {
      linblock_ofv <- .get_ext_ofv(paste0(directory,"modelfit_run/NM_run1/psn.ext"))
      dofv_block <- linbase_ofv-linblock_ofv
      # how many omega cov omegas were added
      boxcox_omegas <- get_omega_values(paste0(directory,"modelfit_run/NM_run1/psn.ext"))$omegas_cov
      linbase_omegas <- get_omega_values(paste0(directory,"linearize_run/",sub('.mod.*','',model.filename),"_linbase.ext"))$omegas_cov
      add.par_block <- length(setdiff(colnames(boxcox_omegas),colnames(linbase_omegas)))
    } else {
      dofv_block <- NA
      add.par_block <- NA
    }
    
    #boxcox transformation
    if(file.exists(paste0(directory,"modelfit_run/NM_run2/psn.ext"))) {
      linbox_ofv <- .get_ext_ofv(paste0(directory,"modelfit_run/NM_run2/psn.ext"))
      dofv_box <- linbase_ofv - linbox_ofv
      #get nr TH+d
      ext_file <- read.table((paste0(directory,"modelfit_run/NM_run2/psn.ext")),header=TRUE,skip=1,stringsAsFactors = F) %>%
        filter(ITERATION==-1000000000)
      TH_values <- ext_file[grep("^THETA+[0-9]$",colnames(ext_file))]
      add.par_box <- length(TH_values[!is.na(TH_values)])
    } else {
      dofv_box <- NA
      add.par_box <- NA
    }
        
    # additional etas
    if(file.exists(paste0(directory,"modelfit_run/NM_run3/psn.ext"))) {
      linaddeta_ofv <- .get_ext_ofv(paste0(directory,"modelfit_run/NM_run3/psn.ext"))
      dofv_additional_eta <- linbase_ofv - linaddeta_ofv
      addetas_omegas <- get_omega_values(paste0(directory,"modelfit_run/NM_run3/psn.ext"))$omegas_var
      linbase_omegas <- get_omega_values(paste0(directory,"linearize_run/",sub('.mod.*','',model.filename),"_linbase.ext"))$omegas_var
      add.par_additional_eta <- length(setdiff(colnames(addetas_omegas),colnames(linbase_omegas)))
    } else {
      dofv_additional_eta <- NA
      add.par_additional_eta <- NA
    }

    
    par_var_models <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA"), 
                                 c(dofv_block, dofv_box, dofv_additional_eta),
                                 c(add.par_block, add.par_box, add.par_additional_eta),stringsAsFactors = F)
    colnames(par_var_models) <- c("","dofv","Add.params")
    par_var_models$dofv <- round(as.numeric(par_var_models[,2]), 1)
  } else {
    param_var_file_exists <- FALSE
    par_var_models <- error_table(c("Full OMEGA Block","Box-cox Transformation","Additional ETA"))
    dofv_block <- NA
    dofv_box <- NA
  }
  
  return(list(param_var_file_exists=param_var_file_exists,
              par_var_models=par_var_models,
              dofv_block=dofv_block,
              dofv_box=dofv_box))
}