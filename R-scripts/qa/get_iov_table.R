get_iov_table <- function(directory,iov_etas,dofv_iov) {
  # calculate how many omegas goes to each block (which goes to iiv and which to iov)
  if(length(iov_etas)>0 && 
     file.exists(file.path(directory,"modelfit_run/iov.ext")) && 
     file.exists(file.path(directory,"linearize_run/scm_dir1/derivatives.ext"))) { # we need this table only if we have iov
    
    omegas <- colnames(get_omega_values(ext_file=file.path(directory,"linearize_run/scm_dir1/derivatives.ext"),omegas="var"))
    iiv_etas_nr <- as.data.frame(strsplit(omegas,split="\\."),stringsAsFactors = F) %>%
      dplyr::slice(2) %>%
      as.numeric()
    new_omega_values <- get_omega_values(ext_file=file.path(directory,"modelfit_run/iov.ext"),omegas="var")
    #which omega columns to iiv
    iiv_omegas <- new_omega_values %>%
      dplyr::select((grep(paste(paste0("^OMEGA\\.",iiv_etas_nr,"\\.",iiv_etas_nr,"\\.$"),collapse = "|"),colnames(.)))) %>%
      as.numeric() %>%
      sqrt()
    iov_omegas <- new_omega_values %>%
      dplyr::select(grep(paste(paste0("^OMEGA\\.",iov_etas[[1]],"\\.",iov_etas[[1]],"\\.$"),collapse = "|"),colnames(.))) %>%
      as.numeric() %>%
      sqrt()
    
    old_omega_values <- get_omega_values(ext_file=file.path(directory,"linearize_run/scm_dir1/derivatives.ext"),omegas="var") %>%
      as.numeric() %>%
      sqrt()
    # make table
    iov_table <- data.frame(etas=paste0("ETA(",c(iiv_etas_nr),")"),stringsAsFactors = F) %>%
      dplyr::mutate(IIV=iiv_omegas,IOV=iov_omegas,Old=old_omega_values)
    
    for(i in 2:ncol(iov_table)) {
      iov_table[,i] <- format(round(iov_table[,i],2),digits=1,trim=T,scientific = F,nsmall=2)
    }
    colnames(iov_table) <- c("Linked to","IIV","IOV","Old SD")
    if(class(dofv_iov)!="character") {
      iov_table <- rbind(iov_table,c("dOFV",format(round(dofv_iov,2),digits=1,scientific=F,nsmall=1),"",""))
    }
    iov_error <- FALSE
    
  } else {
    iov_error <- TRUE
    iov_table <- error_table(col=1)
  }
  return(list(iov_table=iov_table,
              iov_error=iov_error))
}