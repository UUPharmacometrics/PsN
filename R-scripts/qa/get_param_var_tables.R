get_param_var_tables <- function(directory,base_model,skip,quiet=F) {
  #for overview table
  fullblock_mod <- file.exists(file.path(directory,"modelfit_run/fullblock.mod"))
  boxcox_mod <- file.exists(file.path(directory,"modelfit_run/boxcox.mod"))
  add_etas_mod <- file.exists(file.path(directory,"add_etas_run/add_etas_linbase.mod"))
  tdist_mod <- file.exists(file.path(directory,"modelfit_run/tdist.mod"))
  iov_mod <- file.exists(file.path(directory,"modelfit_run/iov.mod"))
  
  #base model ext filename
  base_ext_file <- sub("(\\.[^.]+)$",".ext",base_model)
  
  if(file.exists(base_ext_file)) {
    base_ofv <- .get_ext_ofv(base_ext_file)

    #full omega block
    if(fullblock_mod) {
      if(file.exists(file.path(directory,"modelfit_run/fullblock.ext"))) {
        fullblock_ofv <- .get_ext_ofv(file.path(directory,"modelfit_run/fullblock.ext"))
        dofv_block <- as.numeric(base_ofv-fullblock_ofv)
        # how many omega cov omegas were added
        fullbock_omegas <- get_omega_values(file.path(directory,"modelfit_run/fullblock.ext"),"cov")
        base_omegas <- get_omega_values(base_ext_file,"cov")
        add.par_block <- length(setdiff(colnames(fullbock_omegas),colnames(base_omegas)))
      } else {
        if(!quiet) {
          message("WARNING: File ",file.path(directory,"modelfit_run/fullblock.ext")," not found!")
        }
        dofv_block <- "ERROR"
        add.par_block <- ''
      }
    } else {
      dofv_block <- "NA"
      add.par_block <- ''
    }

    #boxcox transformation
    if(boxcox_mod) {
      if(file.exists(file.path(directory,"modelfit_run/boxcox.ext"))) {
        boxcox_ofv <- .get_ext_ofv(file.path(directory,"modelfit_run/boxcox.ext"))
        dofv_box <- as.numeric(base_ofv - boxcox_ofv)
        #get nr TH+d
        boxcox_thetas <- count_thetas(filename=file.path(directory,"modelfit_run/boxcox.ext"))
        base_thetas <- count_thetas(filename=base_ext_file)
        add.par_box <- boxcox_thetas - base_thetas
      } else {
        if(!quiet) {
          message("WARNING: File ",file.path(directory,"modelfit_run/boxcox.ext")," not found!")
        }
        dofv_box <- "ERROR"
        add.par_box <- ''
      }
    } else {
      dofv_box <- "NA"
      add.par_box <- ''
    }
        
    # additional etas
    if(add_etas_mod) {
      if(file.exists(file.path(directory,"add_etas_run/add_etas_linbase.ext"))) {
        addeta_ofv <- .get_ext_ofv(file.path(directory,"add_etas_run/add_etas_linbase.ext"))
        dofv_additional_eta <- as.numeric(base_ofv - addeta_ofv)
        addetas_omegas <- get_omega_values(file.path(directory,"add_etas_run/add_etas_linbase.ext"),"var")
        base_omegas <- get_omega_values(base_ext_file,"var")
        add.par_additional_eta <- length(setdiff(colnames(addetas_omegas),colnames(base_omegas)))
      } else {
        if(!quiet) {
          message("WARNING: File ",file.path(directory,"add_etas_run/add_etas_linbase.ext")," not found!")
        }
        dofv_additional_eta <- "ERROR"
        add.par_additional_eta <- ''
      }
    } else {
      dofv_additional_eta <- "NA"
      add.par_additional_eta <- ''
    }
    
    # t-distribution
    if(tdist_mod) {
      if(file.exists(file.path(directory,"modelfit_run/tdist.ext"))) {
        tdist_ofv <- .get_ext_ofv(file.path(directory,"modelfit_run/tdist.ext"))
        dofv_tdist <- as.numeric(base_ofv - tdist_ofv)
        #get nr TH+d
        tdist_thetas <- count_thetas(filename=file.path(directory,"modelfit_run/tdist.ext"))
        base_thetas <- count_thetas(filename=base_ext_file)
        add.par_tdist <-  tdist_thetas - base_thetas
      } else {
        if(!quiet) {
          message("WARNING: File ",file.path(directory,"modelfit_run/tdist.ext")," not found!")
        }
        dofv_tdist <- "ERROR"
        add.par_tdist <- ''
      }
    } else {
      dofv_tdist <- "NA"
      add.par_tdist <- ''
    }
    
    # iov
    if(iov_mod) {
      if(file.exists(file.path(directory,"modelfit_run/iov.ext"))) {
        iov_ofv <- .get_ext_ofv(file.path(directory,"modelfit_run/iov.ext"))
        dofv_iov <- as.numeric(base_ofv - iov_ofv)
        #get nr of var omegas in linearizes model because iov will add same amount of omegas ass it was in the beginning 
        #BLOCK SAME should not be included
        add.par_iov <- ncol(get_omega_values(base_ext_file,"var"))
      } else {
        if(!quiet) {
          message("WARNING: File ",file.path(directory,"modelfit_run/iov.ext")," not found!")
        }
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
    
  } else {
    if(!quiet) {
      message("WARNING: File ",base_ext_file," not found!")
    }
    if(fullblock_mod) {
      dofv_block <- "ERROR"
    } else {
      dofv_block <- "NA"
    }
    if(boxcox_mod) {
      dofv_box <- "ERROR"
    } else {
      dofv_box <- "NA"
    }
    if(tdist_mod) {
      dofv_tdist <- "ERROR"
    } else {
      dofv_tdist <- "NA"
    }
    if(add_etas_mod) {
      dofv_additional_eta <- "ERROR"
    } else {
      dofv_additional_eta <- "NA"
    }
    if(iov_mod) {
      dofv_iov <- "ERROR"
    } else {
      dofv_iov <- "NA"
    }
    par_var_models <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution","Interoccasion variability"), 
                                 c(dofv_block,dofv_box,dofv_additional_eta,dofv_tdist,dofv_iov),stringsAsFactors = F)
    colnames(par_var_models) <- c("","dOFV")
  }
  # check if etas run was skipped
  if(any(skip=="transform")) {
    par_var_models <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution","Interoccasion variability"), 
                                 c(rep("SKIPPED",5)),stringsAsFactors = F)
    colnames(par_var_models) <- c("","dOFV")
  }
  
  return(list(par_var_models=par_var_models,
              dofv_block=dofv_block,
              dofv_box=dofv_box,
              dofv_tdist=dofv_tdist,
              dofv_add_etas=dofv_additional_eta,
              dofv_iov=dofv_iov,
              fullblock_mod=fullblock_mod,
              boxcox_mod=boxcox_mod,
              add_etas_mod=add_etas_mod,
              tdist_mod=tdist_mod,
              iov_mod=iov_mod))
}