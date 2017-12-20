get_iov_table <- function(original_max0_model,iov_model,iov_etas,dofv_iov,quiet=F) {
  original_ext_file <- sub("(\\.[^.]+)$",".ext",original_max0_model)
  iov_ext_file <- sub("(\\.[^.]+)$",".ext",iov_model)
  # calculate how many omegas goes to each block (which goes to iiv and which to iov)
  if(length(iov_etas)>0 && file.exists(iov_ext_file) && file.exists(original_ext_file)) { # we need this table only if we have iov
    
    omegas <- colnames(get_omega_values(ext_file=original_ext_file,omegas="var"))
    iiv_etas_nr <- as.data.frame(strsplit(omegas,split="\\."),stringsAsFactors = F) %>%
      dplyr::slice(2) %>%
      as.numeric()
    new_omega_values <- get_omega_values(ext_file=iov_ext_file,omegas="var")
    #which omega columns to iiv
    iiv_omegas <- new_omega_values %>%
      dplyr::select((grep(paste(paste0("^OMEGA\\.",iiv_etas_nr,"\\.",iiv_etas_nr,"\\.$"),collapse = "|"),colnames(.)))) %>%
      as.numeric() %>%
      sqrt()
    iov_omegas <- new_omega_values %>%
      dplyr::select(grep(paste(paste0("^OMEGA\\.",iov_etas[[1]],"\\.",iov_etas[[1]],"\\.$"),collapse = "|"),colnames(.))) %>%
      as.numeric() %>%
      sqrt()
    
    old_omega_values <- get_omega_values(ext_file=original_ext_file,omegas="var") %>%
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
    if(!file.exists(iov_ext_file) && !quiet) {
      message("WARNING: File ",iov_ext_file," not found!")
    }
    if(!file.exists(original_ext_file) && !quiet) {
      message("WARNING: File ",original_ext_file," not found!")
    }
    if(length(iov_etas)==0 && !quiet) {
      message("WARNING: iov_etas vector is empty!")
    }
    iov_error <- TRUE
    iov_table <- error_table(col=1)
  }
  return(list(iov_table=iov_table,
              iov_error=iov_error))
}