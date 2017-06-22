get_ii_table <- function(raw.results.file,skipped.id.file,cutoff){
  cdd_files_exist <- TRUE
  if(file.exists(raw.results.file) && file.exists(skipped.id.file)) {
    data_full <- create.data.full(raw.results.file,skipped.id.file)
    cdd.data <- data_full$cdd.data.all
    if(any(colnames(cdd.data)=="cdd.delta.ofv")) {
      cdd.data <- cdd.data %>% select(c(ID,cdd.delta.ofv)) %>% slice(-1)
      colnames(cdd.data) <- c("id", "dOFV")
      
      #find negative delta ofv values, if exist
      fail_ID <- c()
      if (any(cdd.data$dOFV < 0)) {
        negat.delta.row <- which(cdd.data$dOFV < 0)
        fail_ID <- cdd.data$ID[negat.delta.row]
        cdd.data <- cdd.data[-negat.delta.row,]
      }
      
      if(nrow(cdd.data)!=0) {
        #get individual with the highest dofv
        cdd_highest_dofv <- cdd.data[which.max(cdd.data$dOFV),]
        cdd_highest_dofv[,2] <- round(as.numeric(cdd_highest_dofv[,2]), 1)
        cdd_highest_dofv <- cdd_highest_dofv %>%
          mutate(id=paste("Subject",id))
        colnames(cdd_highest_dofv) <- c("","dOFV")
        
        
        # find influential individuals, where delta ofv values are bigger than cutoffs
        if(any(cdd.data$dOFV > cutoff)) {
          ii_table <- as.data.frame(subset(cdd.data,dOFV > cutoff))
          ii_table <- ii_table[order(ii_table$dOFV,decreasing = T),]
          ii_table[,2] <- round(as.numeric(ii_table[,2]), 1)
          ii_table <- ii_table %>%
            mutate(id=paste("Subject",id))
          colnames(ii_table)[which(colnames(ii_table)=="id")] <- "Subjects"
        } else {
          ii_table <- data.frame(c("No influential individuals detected"),stringsAsFactors = F)
          colnames(ii_table) <- ""
          cdd_highest_dofv <- data.frame("None","",stringsAsFactors = F)
          colnames(cdd_highest_dofv) <- c("","dOFV")
        }
      } else {
        cdd_highest_dofv <- data.frame("All dOFV values are negative","",stringsAsFactors = F)
        colnames(cdd_highest_dofv) <- c("","dOFV")
        ii_table <- data.frame(c("All dOFV values are negative"),stringsAsFactors = F)
        colnames(ii_table) <- ""
      }
    } else {
      cdd_files_exist <- FALSE
      ii_table <- error_table(col=1)
      cdd_highest_dofv <- error_table("CDD")
      cdd.data <- error_table(col=1)
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