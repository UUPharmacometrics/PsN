get_full_omega_block <- function(directory,param_var_file_exists,dofv_block) {
  #get full omega block extra table
  if(file.exists(paste0(directory,"/fullblock.ext")) && file.exists(paste0(directory,"/linearize_run/scm_dir1/derivatives.ext"))) {
    full_block_table <-read.table((paste0(directory,"/fullblock.ext")),header=TRUE,skip=1,stringsAsFactors = F) %>%
      filter(ITERATION==-1000000000)
    omega_values <- full_block_table[,grep("^OMEGA",colnames(full_block_table))]
    omega_values <- omega_values[,which(omega_values!=0)]
    
    # omega values from original model
    derivatives_table <-read.table((paste0(directory,"/linearize_run/scm_dir1/derivatives.ext")),header=TRUE,skip=1,stringsAsFactors = F) %>%
      filter(ITERATION==-1000000000)
    deriv_omega_values <- derivatives_table[,grep("^OMEGA",colnames(derivatives_table))]
    deriv_omega_values <- deriv_omega_values[,which(deriv_omega_values!=0)]
    
    # create a table
    full_omega_block_table <- as.data.frame(array(NA,c(length(omega_values),3)))
    colnames(full_omega_block_table) <- c("","New","Old")
    for(i in 1:length(omega_values)) {
      numeration <- sub('.*OMEGA.','',colnames(omega_values[i]))
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
          full_omega_block_table[i,3] <- NA
        }
      
      } else {
        full_omega_block_table[i,1] <- paste0("corr(",min(first,second),",",max(first,second),")")
        if(any(colnames(omega_values)==paste0("OMEGA.",max(first,second),".",min(first,second),"."))) {
          cov_xy <- as.numeric(omega_values[,grep(paste0("OMEGA.",max(first,second),".",min(first,second),"."),colnames(omega_values))])
        } else {
          cov_xy <- as.numeric(omega_values[,grep(paste0("OMEGA.",min(first,second),".",max(first,second),"."),colnames(omega_values))])
        }
        var_x <- as.numeric(omega_values[,grep(paste0("OMEGA.",min(first,second),".",min(first,second),"."),colnames(omega_values))])
        var_y <- as.numeric(omega_values[,grep(paste0("OMEGA.",max(first,second),".",max(first,second),"."),colnames(omega_values))])
        full_omega_block_table[i,2] <- cov_xy/(sqrt(var_x)*sqrt(var_y))
        
        #add original omega values column
        if(any(colnames(deriv_omega_values)==colnames(omega_values)[i])) {
          col_nr <- which(colnames(deriv_omega_values)==colnames(omega_values)[i])
          if(any(colnames(deriv_omega_values)==paste0("OMEGA.",max(first,second),".",min(first,second),"."))) {
            cov_xy <- as.numeric(deriv_omega_values[,grep(paste0("OMEGA.",max(first,second),".",min(first,second),"."),colnames(deriv_omega_values))])
          } else {
            cov_xy <- as.numeric(deriv_omega_values[,grep(paste0("OMEGA.",min(first,second),".",max(first,second),"."),colnames(deriv_omega_values))])
          }
          var_x <- as.numeric(deriv_omega_values[,grep(paste0("OMEGA.",min(first,second),".",min(first,second),"."),colnames(deriv_omega_values))])
          var_y <- as.numeric(deriv_omega_values[,grep(paste0("OMEGA.",max(first,second),".",max(first,second),"."),colnames(deriv_omega_values))])
          full_omega_block_table[i,3] <- cov_xy/(sqrt(var_x)*sqrt(var_y))
        } else {
          full_omega_block_table[i,3] <- NA
        }
      }
      
    }
    full_omega_block_table[,2] <- round(as.numeric(full_omega_block_table[,2]),2)
    full_omega_block_table[,3] <- round(as.numeric(full_omega_block_table[,3]),2)
    
    if(param_var_file_exists) {
      full_omega_block_table <- rbind(full_omega_block_table,c("dOFV",dofv_block,""))
    }
    
  } else {
    full_omega_block_table <- error_table(col=1)
  }
  
  return(full_omega_block_table)
}


