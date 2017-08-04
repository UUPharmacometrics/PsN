get_param_var_tables <- function(directory,model.filename) {
  #for overview table
  fullblock_mod <- FALSE
  boxcox_mod <- FALSE
  add_etas_mod <- FALSE
  if(file.exists(file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")))) {
    
    linbase_ofv <- .get_ext_ofv(file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")))

    #full omega block
    if(file.exists(file.path(directory,"fullblock.mod"))) {
      fullblock_mod <- TRUE
      if(file.exists(file.path(directory,"fullblock.ext"))) {
        linblock_ofv <- .get_ext_ofv(file.path(directory,"fullblock.ext"))
        dofv_block <- as.numeric(linbase_ofv-linblock_ofv)
        # how many omega cov omegas were added
        boxcox_omegas <- get_omega_values(file.path(directory,"fullblock.ext"),"cov")
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
    if(file.exists(file.path(directory,"boxcox.mod"))) {
      boxcox_mod <- TRUE
      if(file.exists(file.path(directory,"boxcox.ext"))) {
        linbox_ofv <- .get_ext_ofv(file.path(directory,"boxcox.ext"))
        dofv_box <- as.numeric(linbase_ofv - linbox_ofv)
        #get nr TH+d
        ext_file <- read.table(file.path(directory,"boxcox.ext"),header=TRUE,skip=1,stringsAsFactors = F) %>%
          filter(ITERATION==-1000000000)
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
    if(file.exists(file.path(directory,"add_etas.mod"))) {
      add_etas_mod <- TRUE
      if(file.exists(file.path(directory,"add_etas.ext"))) {
        linaddeta_ofv <- .get_ext_ofv(file.path(directory,"add_etas.ext"))
        dofv_additional_eta <- as.numeric(linbase_ofv - linaddeta_ofv)
        addetas_omegas <- get_omega_values(file.path(directory,"add_etas.ext"),"var")
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

    
    par_var_models <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA"), 
                                 c(dofv_block,dofv_box,dofv_additional_eta),
                                 c(add.par_block, add.par_box, add.par_additional_eta),stringsAsFactors = F)
    colnames(par_var_models) <- c("","dOFV","Add.params")
  } else {
    par_var_models <- error_table(c("Full OMEGA Block","Box-Cox Transformation","Additional ETA"))
    dofv_block <- "NA"
    dofv_box <- "NA"
    
  }
  
  return(list(par_var_models=par_var_models,
              dofv_block=dofv_block,
              dofv_box=dofv_box,
              fullblock_mod=fullblock_mod,
              boxcox_mod=boxcox_mod,
              add_etas_mod=add_etas_mod))
}