get_full_omega_block <- function(directory,param_var_file_exists,dofv_block) {
  #get full omega block extra table
  if(file.exists(paste0(directory,"/fullblock.ext"))) {
    full_block_table <-read.table((paste0(directory,"/fullblock.ext")),header=TRUE,skip=1,stringsAsFactors = F) %>%
      filter(ITERATION==-1000000000)
    omega_values <- full_block_table[,grep("^OMEGA",colnames(full_block_table))]
    omega_values <- omega_values[,which(omega_values!=0)]
    
    # create a table
    full_omega_block_table <- as.data.frame(array(NA,c(length(omega_values),2)))
    colnames(full_omega_block_table) <- c("","Value")
    for(i in 1:length(omega_values)) {
      numeration <- sub('.*OMEGA.','',colnames(omega_values[i]))
      numeration <- substr(numeration, 1, nchar(numeration)-1) # delete last element in string
      first <- sub('\\..*','',numeration)
      second <- sub('.*\\.','',numeration)
      if(first==second) {
        full_omega_block_table[i,1] <- paste0("sd(",first,")")
        full_omega_block_table[i,2] <- sqrt(as.numeric(omega_values[i]))
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
      }
      
    }
    full_omega_block_table[,2] <- round(as.numeric(full_omega_block_table[,2]),2)
    
    if(param_var_file_exists) {
      full_omega_block_table <- rbind(full_omega_block_table,c("dOFV",dofv_block))
    }
    
  } else {
    full_omega_block_table <- error_table(col=1)
  }
  
  return(full_omega_block_table)
}


