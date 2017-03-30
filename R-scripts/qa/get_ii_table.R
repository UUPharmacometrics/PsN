get_ii_table <- function(cdd_directory,cutoff){
  cdd_files_exist <- TRUE
  if(file.exists(file.path(cdd_directory, "raw_results.csv")) && file.exists(file.path(cdd_directory, "skipped_individuals1.csv"))) {
    data_full <- create.data.full(file.path(cdd_directory, "raw_results.csv"),
                                  file.path(cdd_directory, "skipped_individuals1.csv"))
    cdd.data <- data_full$cdd.data.all
    cdd.data <- cdd.data %>% select(c(ID,cdd.delta.ofv)) %>% slice(-1)
    colnames(cdd.data) <- c("id", "dofv")
    
    #find negative delta ofv values, if exist
    fail_ID <- c()
    if (any(cdd.data$dofv < 0)) {
      negat.delta.row <- which(cdd.data$dofv < 0)
      fail_ID <- cdd.data$ID[negat.delta.row]
      cdd.data <- cdd.data[-negat.delta.row,]
    }
    
    #get individual with the highest dofv
    cdd_highest_dofv <- cdd.data[which.max(cdd.data$dofv),]
    cdd_highest_dofv[,2] <- round(as.numeric(cdd_highest_dofv[,2]), 2)
    cdd_highest_dofv <- cdd_highest_dofv %>%
      mutate(id=paste("Subject",id))
    colnames(cdd_highest_dofv) <- c("","dofv")
    
    # find influential individuals, where delta ofv values are bigger than cutoffs
    if(any(cdd.data$dofv > cutoff)) {
      ii_table <- subset(cdd.data,dofv > cutoff)
      ii_table <- ii_table[order(ii_table$dofv,decreasing = T),]
      ii_table[,2] <- round(as.numeric(ii_table[,2]), 2)
      ii_table <- ii_table %>%
        mutate(id=paste("Subject",id))
      colnames(ii_table)[which(colnames(ii_table)=="id")] <- "Subjects"
    } else {
      ii_table <- data.frame(c("No influential individuals detected"))
      colnames(ii_table) <- ""
    }
  } else {
    cdd_files_exist <- FALSE
    ii_table <- error_table(col=1)
    cdd_highest_dofv <- error_table("CDD")
    cdd.data <- error_table(col=1)
  }
  return(list(cdd_files_exist=cdd_files_exist,
              cdd.data=cdd.data,
              cdd_highest_dofv=cdd_highest_dofv,
              ii_table=ii_table))
}