get_boxcox_lambda_table <- function(directory,param_var_file_exists,dofv_box) {
  #get lambda values (extra table for box-cox transformation)
  if(file.exists(paste0(directory,"boxcox.ext"))) {
    ext_file <- read.table((paste0(directory,"boxcox.ext")),header=TRUE,skip=1,stringsAsFactors = F) %>%
      filter(ITERATION==-1000000000)
    lambdas <- ext_file[grep("^THETA+[0-9]$",colnames(ext_file))]
    boxcox_lambdas_table <- as.data.frame(array(0,c(length(lambdas),2)))
    colnames(boxcox_lambdas_table) <- c("","Lambda")
    for(i in 1:length(lambdas)) {
      boxcox_lambdas_table[i,1] <- paste0("ETA(",i,")")
      boxcox_lambdas_table[i,2] <- lambdas[i]
    }
    boxcox_lambdas_table[,2] <- as.character(round(as.numeric(boxcox_lambdas_table[,2]), 1))
    if(param_var_file_exists) {
      boxcox_lambdas_table <- rbind(boxcox_lambdas_table,c("dOFV",dofv_box))
    }
    
  } else {
    boxcox_lambdas_table <- error_table(col=1)
  }
  
  return(boxcox_lambdas_table)
}


