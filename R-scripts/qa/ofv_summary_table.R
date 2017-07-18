ofv_summary_table <- function(working.directory,model.filename) {
  ofv_nonlin_base_mod <- .get_ext_ofv(paste0(working.directory,"linearize_run/scm_dir1/derivatives.ext"))
  ofv_lin_base_mod_before_est <- .get_ext_ofv(paste0(working.directory,"linearize_run/",paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")),0)
  ofv_lin_base_mod <- .get_ext_ofv(paste0(working.directory,"linearize_run/",paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")))
  linbase_phi_table <- read.table(paste0(working.directory,"linearize_run/",paste0(sub('.([^.]*)$','',model.filename),"_linbase.phi")),skip=1,header=T,stringsAsFactors = F)
  linbase_phi_ofv <- sum(linbase_phi_table$OBJ)
  ofv_table <- data.frame(c("Nonlinear base model","Linearized base model before estimation","Linearized base model after estimation","Sum of individual OFV values"),
                          as.character(c(ofv_nonlin_base_mod,ofv_lin_base_mod_before_est,ofv_lin_base_mod,linbase_phi_ofv)),
                          stringsAsFactors = F)
  colnames(ofv_table) <- c('','OFV')
  return(ofv_table)
}