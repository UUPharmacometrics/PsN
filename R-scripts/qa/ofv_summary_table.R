ofv_summary_table <- function(working.directory,model.filename,quiet=F) {
  if(file.exists(file.path(working.directory,"linearize_run/scm_dir1/derivatives.ext"))) {
    ofv_nonlin_base_mod <- .get_ext_ofv(file.path(working.directory,"linearize_run/scm_dir1/derivatives.ext"))
  } else {
    ofv_nonlin_base_mod <- "ERROR"
    if(!quiet) {
      message("WARNING: File ",file.path(working.directory,"linearize_run/scm_dir1/derivatives.ext")," not found!")
    }
  }
  if(file.exists(file.path(working.directory,"linearize_run/scm_dir1/",paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")))) {
    ofv_lin_base_mod_before_est <- .get_ext_ofv(file.path(working.directory,"linearize_run/scm_dir1/",paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")),0)
  } else {
    ofv_lin_base_mod_before_est <- "ERROR"
    if(!quiet) {
      message("WARNING: File ",file.path(working.directory,"linearize_run/scm_dir1/",paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext"))," not found!")
    }
  }
  if(file.exists(file.path(working.directory,"linearize_run/scm_dir1/",paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")))) {
    ofv_lin_base_mod <- .get_ext_ofv(file.path(working.directory,"linearize_run/scm_dir1/",paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")))
  } else {
    ofv_lin_base_mod <- "ERROR"
    if(!quiet) {
      message("WARNING: File ",file.path(working.directory,"linearize_run/scm_dir1/",paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext"))," not found!")
    }
  }
  if(file.exists(file.path(working.directory,"linearize_run/scm_dir1/",paste0(sub('.([^.]*)$','',model.filename),"_linbase.phi")))) {
    linbase_phi_table <- read.table(file.path(working.directory,"linearize_run/scm_dir1/",paste0(sub('.([^.]*)$','',model.filename),"_linbase.phi")),skip=1,header=T,stringsAsFactors = F)
    linbase_phi_ofv <- sum(linbase_phi_table$OBJ)
  } else {
    linbase_phi_ofv <- "ERROR"
    if(!quiet) {
      message("WARNING: File ",file.path(working.directory,"linearize_run/scm_dir1/",paste0(sub('.([^.]*)$','',model.filename),"_linbase.phi"))," not found!")
    }
  }
  
  
  ofv_table <- data.frame(c("Nonlinear base model","Linearized base model before estimation","Linearized base model after estimation","Sum of individual OFV values"),
                          as.character(c(ofv_nonlin_base_mod,ofv_lin_base_mod_before_est,ofv_lin_base_mod,linbase_phi_ofv)),
                          stringsAsFactors = F)
  colnames(ofv_table) <- c('','OFV')
  return(ofv_table)
}