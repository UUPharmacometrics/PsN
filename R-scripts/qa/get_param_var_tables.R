get_param_var_tables <- function(directory,model.filename,skip) {
  #for overview table
  fullblock_mod <- FALSE
  boxcox_mod <- FALSE
  add_etas_mod <- FALSE
  tdist_mod <- FALSE
  if(file.exists(file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")))) {
    
    linbase_ofv <- .get_ext_ofv(file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")))

    #full omega block
    if(file.exists(file.path(directory,"modelfit_run/fullblock.mod"))) {
      fullblock_mod <- TRUE
      if(file.exists(file.path(directory,"modelfit_run/fullblock.ext"))) {
        linblock_ofv <- .get_ext_ofv(file.path(directory,"modelfit_run/fullblock.ext"))
        dofv_block <- as.numeric(linbase_ofv-linblock_ofv)
        # how many omega cov omegas were added
        boxcox_omegas <- get_omega_values(file.path(directory,"modelfit_run/fullblock.ext"),"cov")
        linbase_omegas <- get_omega_values(file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")),"cov")
        add.par_block <- length(setdiff(colnames(boxcox_omegas),colnames(linbase_omegas)))
      } else {
        dofv_block <- "ERROR"
        add.par_block <- ''
      }
    } else {
      dofv_block <- "NA"
      add.par_block <- ''
    }

    #boxcox transformation
    if(file.exists(file.path(directory,"modelfit_run/boxcox.mod"))) {
      boxcox_mod <- TRUE
      if(file.exists(file.path(directory,"modelfit_run/boxcox.ext"))) {
        linbox_ofv <- .get_ext_ofv(file.path(directory,"modelfit_run/boxcox.ext"))
        dofv_box <- as.numeric(linbase_ofv - linbox_ofv)
        #get nr TH+d
        ext_file <- read.table(file.path(directory,"modelfit_run/boxcox.ext"),header=TRUE,skip=1,stringsAsFactors = F) %>%
          dplyr::filter(ITERATION==-1000000000)
        TH_values <- ext_file[grep("^THETA+[0-9]$",colnames(ext_file))]
        add.par_box <- length(TH_values[!is.na(TH_values)])
      } else {
        dofv_box <- "ERROR"
        add.par_box <- ''
      }
    } else {
      dofv_box <- "NA"
      add.par_box <- ''
    }
        
    # additional etas
    if(file.exists(file.path(directory,"modelfit_run/add_etas.mod"))) {
      add_etas_mod <- TRUE
      if(file.exists(file.path(directory,"modelfit_run/add_etas.ext"))) {
        linaddeta_ofv <- .get_ext_ofv(file.path(directory,"modelfit_run/add_etas.ext"))
        dofv_additional_eta <- as.numeric(linbase_ofv - linaddeta_ofv)
        addetas_omegas <- get_omega_values(file.path(directory,"modelfit_run/add_etas.ext"),"var")
        linbase_omegas <- get_omega_values(file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")),"var")
        add.par_additional_eta <- length(setdiff(colnames(addetas_omegas),colnames(linbase_omegas)))
      } else {
        dofv_additional_eta <- "ERROR"
        add.par_additional_eta <- ''
      }
    } else {
      dofv_additional_eta <- "NA"
      add.par_additional_eta <- ''
    }
    
    # t-distribution
    if(file.exists(file.path(directory,"modelfit_run/tdist.mod"))) {
      tdist_mod <- TRUE
      if(file.exists(file.path(directory,"modelfit_run/tdist.ext"))) {
        lintdist_ofv <- .get_ext_ofv(file.path(directory,"modelfit_run/tdist.ext"))
        dofv_tdist <- as.numeric(linbase_ofv - lintdist_ofv)
        #get nr TH+d
        ext_file <- read.table(file.path(directory,"modelfit_run/tdist.ext"),header=TRUE,skip=1,stringsAsFactors = F) %>%
          dplyr::filter(ITERATION==-1000000000)
        TH_values <- ext_file[grep("^THETA+[0-9]$",colnames(ext_file))]
        add.par_tdist <- length(TH_values[!is.na(TH_values)])
      } else {
        dofv_tdist <- "ERROR"
        add.par_tdist <- ''
      }
    } else {
      dofv_tdist <- "NA"
      add.par_tdist <- ''
    }
    
    # iov
    if(file.exists(file.path(directory,"modelfit_run/iov.mod"))) {
      iov_mod <- TRUE
      if(file.exists(file.path(directory,"modelfit_run/iov.ext"))) {
        liniov_ofv <- .get_ext_ofv(file.path(directory,"modelfit_run/iov.ext"))
        dofv_iov <- as.numeric(linbase_ofv - liniov_ofv)
        #get nr of var omegas in linearizes model because iov will add same amount of omegas ass it was in the beginning 
        #BLOCK SAME should not be included
        add.par_iov <- ncol(get_omega_values(file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")),"var"))
      } else {
        dofv_iov <- "ERROR"
        add.par_iov <- ''
      }
    } else {
      dofv_iov <- "NA"
      add.par_iov <- ''
    }

    
    par_var_models <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution","Interoccasion variability"), 
                                 c(dofv_block,dofv_box,dofv_additional_eta,dofv_tdist,dofv_iov),
                                 c(add.par_block, add.par_box, add.par_additional_eta,add.par_tdist,add.par_iov),stringsAsFactors = F)
    colnames(par_var_models) <- c("","dOFV","Add.params")
    
    # check if etas run was skipped
    if(any(skip=="transform")) {
      par_var_models <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution","Interoccasion variability"), 
                                   c(rep("SKIPPED",5)),stringsAsFactors = F)
      colnames(par_var_models) <- c("","dOFV")
    }
  } else {
    par_var_models <- error_table(c("Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution","Interoccasion variability"))
    dofv_block <- "NA"
    dofv_box <- "NA"
    dofv_tdist <- "NA"
  }
  
  return(list(par_var_models=par_var_models,
              dofv_block=dofv_block,
              dofv_box=dofv_box,
              dofv_tdist=dofv_tdist,
              fullblock_mod=fullblock_mod,
              boxcox_mod=boxcox_mod,
              add_etas_mod=add_etas_mod,
              tdist_mod=tdist_mod))
}