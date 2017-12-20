get_full_omega_block <- function(original_max0_model,fullblock_model,dofv_block,quiet=F) {
  full_omega_block_error <- FALSE
  original_ext_file <- sub("(\\.[^.]+)$",".ext",original_max0_model)
  fullblock_ext_file <- sub("(\\.[^.]+)$",".ext",fullblock_model)
  
  #get full omega block extra table
  if(file.exists(fullblock_ext_file) && file.exists(original_ext_file)) {
    # omega values from fullblock model
    omega_values <- get_omega_values(ext_file=fullblock_ext_file,omegas="all")
    # omega values from original model
    deriv_omega_values <- get_omega_values(ext_file=original_ext_file,omegas="all")
    
    # create a table
    full_omega_block_table <- as.data.frame(array(0,c(length(omega_values),3)))
    colnames(full_omega_block_table) <- c("","New","Old")
    for(i in 1:length(omega_values)) {
      numeration <- sub('.*OMEGA\\.','',colnames(omega_values[i]))
      numeration <- substr(numeration, 1, nchar(numeration)-1) # delete last element in string
      first <- sub('\\..*','',numeration)
      second <- sub('.*\\.','',numeration)
      if(first==second) {
        full_omega_block_table[i,1] <- paste0("sd(",first,")")
        full_omega_block_table[i,2] <- sqrt(as.numeric(omega_values[i]))
        
        #add original omega values column
        if(any(colnames(deriv_omega_values)==colnames(omega_values)[i])) {
          col_nr <- which(colnames(deriv_omega_values)==colnames(omega_values)[i])
          full_omega_block_table[i,3] <- sqrt(as.numeric(deriv_omega_values[col_nr]))
        } else {
          full_omega_block_table[i,3] <- as.numeric(NA)
        }
      
      } else {
        full_omega_block_table[i,1] <- paste0("corr(",max(first,second),",",min(first,second),")")
        if(any(colnames(omega_values)==paste0("OMEGA.",max(first,second),".",min(first,second),"."))) {
          cov_xy <- as.numeric(omega_values[,grep(paste0("OMEGA\\.",max(first,second),"\\.",min(first,second),"\\."),colnames(omega_values))])
        } else {
          cov_xy <- as.numeric(omega_values[,grep(paste0("OMEGA\\.",min(first,second),"\\.",max(first,second),"\\."),colnames(omega_values))])
        }
        var_x <- as.numeric(omega_values[,grep(paste0("OMEGA\\.",min(first,second),"\\.",min(first,second),"\\."),colnames(omega_values))])
        var_y <- as.numeric(omega_values[,grep(paste0("OMEGA\\.",max(first,second),"\\.",max(first,second),"\\."),colnames(omega_values))])
        full_omega_block_table[i,2] <- cov_xy/(sqrt(var_x)*sqrt(var_y))
        
        #add original omega values column
        if(any(colnames(deriv_omega_values)==colnames(omega_values)[i])) {
          col_nr <- which(colnames(deriv_omega_values)==colnames(omega_values)[i])
          if(any(colnames(deriv_omega_values)==paste0("OMEGA.",max(first,second),".",min(first,second),"."))) {
            cov_xy <- as.numeric(deriv_omega_values[,grep(paste0("OMEGA\\.",max(first,second),"\\.",min(first,second),"\\."),colnames(deriv_omega_values))])
          } else {
            cov_xy <- as.numeric(deriv_omega_values[,grep(paste0("OMEGA\\.",min(first,second),"\\.",max(first,second),"\\."),colnames(deriv_omega_values))])
          }
          var_x <- as.numeric(deriv_omega_values[,grep(paste0("OMEGA\\.",min(first,second),"\\.",min(first,second),"\\."),colnames(deriv_omega_values))])
          var_y <- as.numeric(deriv_omega_values[,grep(paste0("OMEGA\\.",max(first,second),"\\.",max(first,second),"\\."),colnames(deriv_omega_values))])
          full_omega_block_table[i,3] <- cov_xy/(sqrt(var_x)*sqrt(var_y))
        } else {
          full_omega_block_table[i,3] <- as.numeric(NA)
        }
      }
      
    }
    full_omega_block_table[,2] <- format(round(as.numeric(full_omega_block_table[,2]),2),digits=1,trim=T,nsmall=2,scientific = F)
    full_omega_block_table[,3] <- format(round(as.numeric(full_omega_block_table[,3]),2),digits=1,trim=T,nsmall=2,scientific = F)
    
    if(class(dofv_block)!="character") {
      full_omega_block_table <- rbind(full_omega_block_table,c("dOFV",format(round(dofv_block,2),digits=1,scientific=F,nsmall=1),""))
    }
    
  } else {
    if(!file.exists(fullblock_ext_file) && !quiet) {
      message("WARNING: File ",fullblock_ext_file," not found!")
    }
    if(!file.exists(original_ext_file) && !quiet) {
      message("WARNING: File ",original_ext_file," not found!")
    }
    full_omega_block_table <- error_table(col=1)
    full_omega_block_error <- TRUE
  }

  return(list(full_omega_block_table=full_omega_block_table,
              full_omega_block_error=full_omega_block_error))
}