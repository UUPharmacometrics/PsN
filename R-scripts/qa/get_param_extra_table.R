get_param_extra_table <- function(original_max0_model,param_model,dofv) {
  if(grepl("boxcox",param_model)) {
    table_col_name <- "Lambda"
  }
  if(grepl("tdist",param_model)) {
    table_col_name <- "Degrees of freedom"
  }
  original_ext_file <- sub("(\\.[^.]+)$",".ext",original_max0_model)
  param_model_ext_file <- sub("(\\.[^.]+)$",".ext",param_model)
  
  #get THETA values
  if(file.exists(param_model_ext_file) && file.exists(original_ext_file)) {
    param_extra_table_error <- FALSE
    new_omega_values <- get_omega_values(ext_file=param_model_ext_file,omegas="var")
    col_names <- colnames(new_omega_values)[which(new_omega_values!=0)]
    new_omega_values <- as.data.frame(new_omega_values[,which(new_omega_values!=0)])
    colnames(new_omega_values) <- col_names
    
    #get needed numers of var omegas to filter right theta values
    needed_nr <- c()
    numeration <- sub("OMEGA\\.","",col_names)
    numeration <- substr(numeration, 1, nchar(numeration)-1)
    first <- sub('\\..*','',numeration)
    second <- sub('.*\\.','',numeration)
    nr <- 1
    for(i in 1:length(numeration)) {
      if(first[i]==second[i]) {
        needed_nr[nr] <- as.numeric(first[i])
        nr <- nr + 1
      }
    }
    
    # omega values from original model
    deriv_omega_values <-read.table(original_ext_file,header=TRUE,skip=1,stringsAsFactors = F) %>%
      dplyr::filter(ITERATION==-1000000000) %>%
      dplyr::select(grep("^OMEGA",colnames(.)))
    
    #get theta values
    THETA_values <- read.table((param_model_ext_file),header=TRUE,skip=1,stringsAsFactors = F) %>%
      dplyr::filter(ITERATION==-1000000000) %>%
      dplyr::select(grep("^THETA\\d+$",colnames(.))) %>%
      dplyr::select(1:length(new_omega_values))
    
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
    param_extra_table[,2] <- format(round(as.numeric(param_extra_table[,2]),2),digits=1,trim=T,scientific = F,nsmall=2)
    param_extra_table[,3] <- format(round(as.numeric(param_extra_table[,3]),2),digits=1,trim=T,scientific = F,nsmall=2)
    param_extra_table[,4] <- format(round(as.numeric(param_extra_table[,4]),2),digits=1,trim=T,scientific = F,nsmall=2)
    if(class(dofv)!="character") {
      param_extra_table <- rbind(param_extra_table,c("dOFV",format(round(dofv,2),digits=1,scientific=F,nsmall=1),"",""))
    }
    
  } else {
    param_extra_table <- error_table(col=1)
    param_extra_table_orig <- param_extra_table
    param_extra_table_error <- TRUE
  }
  out <- list(param_extra_table=param_extra_table,
              param_extra_table_orig=param_extra_table_orig,
              param_extra_table_error=param_extra_table_error)
  return(out)
}
