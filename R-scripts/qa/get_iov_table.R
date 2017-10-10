get_iov_table <- function(directory,iov_etas,dofv_iov) {
  # calculate how many omegas goes to each block (which goes to iiv and which to iov)
  if(length(iov_etas)>0) { # we need this table only if we have iov
    nr_iiv_etas <- iov_etas[[1]][1]-1
    occ_1 <- iov_etas[[1]]
    block_nr <- c()
    if(length(occ_1)>1) {
      n <- 1
      nr <- 1
      for(i in 2:length(occ_1)) {
        if(occ_1[i-1]+1==occ_1[i]) {
          n <- n + 1
          if(i == length(occ_1)) {
            block_nr[nr] <- n
          }
        } else {
          block_nr[nr] <- n
          n <- 1
          nr <- nr + 1
          if(i == length(occ_1)) {
            block_nr[nr] <- n
          }
        }
      }
    } else {
      block_nr <- 1
    }
    
    if(file.exists(file.path(directory,"modelfit_run/iov.ext")) && file.exists(file.path(directory,"linearize_run/scm_dir1/derivatives.ext"))) {
      new_omega_values <- get_omega_values(ext_file=file.path(directory,"modelfit_run/iov.ext"),omegas="var")
      #which omega columns to iiv
      iiv_omegas <- new_omega_values %>%
        dplyr::select((grep(paste(1:nr_iiv_etas,collapse = "|"),colnames(.)))) %>%
        as.numeric() %>%
        sqrt()
      #which omega columns to iov
      for(i in 1:length(block_nr)) {
        if(i == 1) {
          iov_omega_col_nr <- 1:block_nr[1]
        } else {
          iov_omega_col_nr <- c(iov_omega_col_nr,(block_nr[i-1]*length(block_nr)+1):(block_nr[i]-1))
        }
      }
      iov_omegas <- new_omega_values %>%
        dplyr::select(-(grep(paste(1:nr_iiv_etas,collapse = "|"),colnames(.)))) %>%
        dplyr::select(iov_omega_col_nr) %>%
        as.numeric() %>%
        sqrt()
      
      old_omega_values <- get_omega_values(ext_file=file.path(directory,"linearize_run/scm_dir1/derivatives.ext"),omegas="var") %>%
        as.numeric() %>%
        sqrt()
      # make table
      iov_table <- data.frame(etas=paste0("ETA(",c(1:nr_iiv_etas),")"),stringsAsFactors = F) %>%
        dplyr::mutate(IIV=iiv_omegas,IOV=iov_omegas,Old=old_omega_values)
      
      for(i in 2:ncol(iov_table)) {
        iov_table[,i] <- format(iov_table[,i],digits=1,trim=T,scientific = F,nsmall=2)
      }
      colnames(iov_table) <- c("Linked to","IIV","IOV","Old SD")
      if(class(dofv_iov)!="character") {
        iov_table <- rbind(iov_table,c("dOFV",format(dofv_iov,digits=1,scientific=F,nsmall=1),"",""))
      }
    } else {
      iov_table <- error_table(col=1)
    }
    return(iov_table)
  }
}