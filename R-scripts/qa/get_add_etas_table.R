get_add_etas_table <- function(directory,added_etas,dofv_add.etas) {
  if(length(added_etas)>0 && 
     file.exists(file.path(directory,"add_etas_run/add_etas_linbase.ext")) && 
     file.exists(file.path(directory,"linearize_run/scm_dir1/derivatives.ext"))) {
    #old SD
    old_omega_values <- get_omega_values(ext_file=file.path(directory,"linearize_run/scm_dir1/derivatives.ext"),omegas="var")
    #get nr for etas
    eta_nr <- strsplit(colnames(old_omega_values),"[.]")
    #new SD
    new_omega_values <- get_omega_values(ext_file=file.path(directory,"add_etas_run/add_etas_linbase.ext"),omegas="var")
    
    #create a table
    add_etas_table <- as.data.frame(array(0,c(length(eta_nr),4)))
    colnames(add_etas_table) <- c("","Added","New SD","Old SD")
    for(i in 1:length(eta_nr)) {
      add_etas_table[i,1] <- paste0("ETA(",eta_nr[[i]][2],")")
      add_etas_table[i,2] <- "No"
      add_etas_table[i,3] <- sqrt(new_omega_values[,grep(paste0(".",eta_nr[[i]][2],"."),colnames(new_omega_values))])
      add_etas_table[i,4] <- sqrt(old_omega_values[,grep(paste0(".",eta_nr[[i]][2],"."),colnames(old_omega_values))])
    }
    add_etas_table[,4] <- format(add_etas_table[,4],digits=1,trim=T,scientific = F,nsmall=2)
    # order added_eta values
    added_etas[sapply(added_etas,is.null)] <- NA
    added_etas <- added_etas[order(unlist(added_etas))]
    
    #which omegas goes to added etas on parameters
    j <- nrow(add_etas_table)
    for (i in 1:length(added_etas)) {
      j <- j + 1
      if(any(added_etas %in% j)) {
        add_etas_table[j,1] <- names(added_etas)[[i]]
        add_etas_table[j,2] <- "Yes"
        add_etas_table[j,3] <- sqrt(new_omega_values[,grep(paste0(".",j,"."),colnames(new_omega_values))])
        add_etas_table[j,4] <- ""
      } else {
        add_etas_table[,3] <- format(add_etas_table[,3],digits=1,trim=T,scientific = F,nsmall=2)
        add_etas_table[j,1] <- names(added_etas)[[i]]
        add_etas_table[j,2] <- "Not found"
        add_etas_table[j,3] <- ""
        add_etas_table[j,4] <- ""
      }
    }
    if(all(!is.na(added_etas))) {
      add_etas_table[,3] <- format(add_etas_table[,3],digits=1,trim=T,scientific = F,nsmall=2)
    }

    if(class(dofv_add.etas)!="character") {
      add_etas_table <- rbind(add_etas_table,c("dOFV",format(dofv_add.etas,digits=1,scientific=F,nsmall=1),"",""))
    }
    return(add_etas_table)
  }
}