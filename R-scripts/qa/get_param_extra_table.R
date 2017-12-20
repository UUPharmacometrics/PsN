get_param_extra_table <- function(original_max0_model,param_model,dofv,quiet=F) {
  if(grepl("boxcox",param_model)) {
    table_col_name <- "Lambda"
  }
  if(grepl("tdist",param_model)) {
    table_col_name <- "Degrees of freedom"
  }
  original_ext_file <- sub("(\\.[^.]+)$",".ext",original_max0_model)
  param_model_ext_file <- sub("(\\.[^.]+)$",".ext",param_model)
  
  if(file.exists(param_model_ext_file) && file.exists(original_ext_file)) {
    param_extra_table_error <- FALSE
    new_omega_values <- get_omega_values(ext_file=param_model_ext_file,omegas="var") # get variances only
    
    #get needed numers of var omegas to filter right theta values
    needed_nr <- gsub("(OMEGA\\.\\d+\\.)","",colnames(new_omega_values)) %>% gsub("\\.","",.)
 
    # omega values from original model
    deriv_omega_values <- get_initial_estimates_from_ext(filename=original_ext_file,select="omega")
    
    # in nonlinear run in the file param_model_ext_file there will be old thetas and the new ones will be at the end. We are interessted only in the new ones.
    # get old thetas values (if exist)
    if(!grepl("derivatives.ext$",original_ext_file)) {
      orig_theta_values <- get_initial_estimates_from_ext(filename=original_ext_file,select="theta")
      added_theta_start <- NCOL(orig_theta_values) + 1
    } else {
      added_theta_start <- 1
    }
    #get new theta values
    THETA_values <- get_initial_estimates_from_ext(filename=param_model_ext_file,select="theta") %>%
      dplyr::select(added_theta_start:(added_theta_start+length(new_omega_values)-1))
    
    #create a table
    param_extra_table <- as.data.frame(array(0,c(length(THETA_values),4)))
    colnames(param_extra_table) <- c("",table_col_name,"New SD","Old SD")
    for(i in 1:length(needed_nr)) {
      nr <- needed_nr[i]
      param_extra_table[i,1] <- paste0("ETA(",nr,")")
      param_extra_table[i,2] <- THETA_values[,i]
      param_extra_table[i,3] <- sqrt(new_omega_values[,which(colnames(new_omega_values)==paste0("OMEGA.",nr,".",nr,"."))])
      param_extra_table[i,4] <- sqrt(deriv_omega_values[,which(colnames(deriv_omega_values)==paste0("OMEGA.",nr,".",nr,"."))])
    }
    param_extra_table_orig <- param_extra_table
    colnames(param_extra_table_orig) <- c("ETA",table_col_name,"New SD","Old SD")
    #format values in the table for printing
    param_extra_table[,2] <- format(round(as.numeric(param_extra_table[,2]),2),digits=1,trim=T,scientific = F,nsmall=2)
    param_extra_table[,3] <- format(round(as.numeric(param_extra_table[,3]),2),digits=1,trim=T,scientific = F,nsmall=2)
    param_extra_table[,4] <- format(round(as.numeric(param_extra_table[,4]),2),digits=1,trim=T,scientific = F,nsmall=2)
    #add dofv value to the table
    if(class(dofv)!="character") {
      param_extra_table <- rbind(param_extra_table,c("dOFV",format(round(dofv,2),digits=1,scientific=F,nsmall=1),"",""))
    }
    
  } else {
    if(!file.exists(param_model_ext_file) && !quiet) {
      message("WARNING: File ",param_model_ext_file," not found!")
    }
    if(!file.exists(original_ext_file) && !quiet) {
      message("WARNING: File ",original_ext_file," not found!")
    }
    param_extra_table <- error_table(col=1)
    param_extra_table_orig <- param_extra_table
    param_extra_table_error <- TRUE
  }
  out <- list(param_extra_table=param_extra_table,
              param_extra_table_orig=param_extra_table_orig,
              param_extra_table_error=param_extra_table_error)
  return(out)
}
