get_outliers_table <- function(simeval_directory,cdd.data,skip,quiet=F) {
  outlier_ids <- c()
  simeval_files_exist <- file.exists(file.path(simeval_directory, "raw_all_iofv.csv"))
  if(simeval_files_exist && all(skip!="simeval")) {
    iofv_res <- i_ofv_res(file.path(simeval_directory, "raw_all_iofv.csv"),show.warning=F)
    outlier_ids <- iofv_res$outlier_ID
    if(length(outlier_ids)!=0) {
      if(ncol(cdd.data)!=1) {
        outliers_table <- as.data.frame(array(0,c(length(outlier_ids),2)))
        colnames(outliers_table) <- c("Subjects","dOFV")
        for(i in 1:length(outlier_ids)) {
          outliers_table[i,1] <- paste("Subject",outlier_ids[i])
          outliers_table[i,2] <- ifelse(any(cdd.data$id %in% outlier_ids[i]),cdd.data$dOFV[which(cdd.data$id==outlier_ids[i])],NA)
        }
        outliers_table <- outliers_table[order(outliers_table$dOFV,decreasing = T),]
        if(nrow(outliers_table)>10) {
          outlier_ids <- as.numeric(sub("Subject ","",outliers_table$Subjects[1:10]))
          outliers_table <- outliers_table[1:10,]
        }
        if(!all(is.na(outliers_table$dOFV))) {
          max_outlier_table <- outliers_table[which.max(outliers_table$dOFV),]
          colnames(max_outlier_table) <- c("","dOFV")
          rownames(max_outlier_table) <- NULL
        } else {
          max_outlier_table <- data.frame("No dOFV values found","",stringsAsFactors = F)
          colnames(max_outlier_table) <- c("","dOFV")
        }
        outliers_table$dOFV <- format(round(as.numeric(outliers_table$dOFV), 1),trim=T,digits=1,nsmall=1)
        rownames(outliers_table) <- NULL
      } else {
        if(length(outlier_ids)>10) {
          outlier_ids <- outlier_ids[1:10]
        }
        if(as.character(cdd.data[1,1])=="SKIPPED") {
          outliers_table <- data.frame("Subjects"=paste("Subject",outlier_ids),"dOFV"=rep("",length(outlier_ids)),stringsAsFactors = F)
          max_outlier_table <- data.frame("No dOFV values found (skipped CDD)","",stringsAsFactors = F)
          colnames(max_outlier_table) <- c("","dOFV")
        } else {
          outliers_table <- data.frame("Subjects"=paste("Subject",outlier_ids),"dOFV"=rep("ERROR",length(outlier_ids)),stringsAsFactors = F)
          max_outlier_table <- data.frame("No dOFV values found","",stringsAsFactors = F)
          colnames(max_outlier_table) <- c("","dOFV")
        }
      }
    } else {
      outliers_table <- data.frame(c("No outliers detected"),stringsAsFactors = F)
      colnames(outliers_table) <- NULL
      max_outlier_table <- data.frame("No outliers detected","",stringsAsFactors = F)
      colnames(max_outlier_table) <- c("","dOFV")
    }
    
  } else {
    if(any(skip=="simeval")) {
      max_outlier_table <- data.frame("SIMEVAL","SKIPPED",stringsAsFactors = F)
      colnames(max_outlier_table) <- c("","dOFV")
    } else {
      if(!quiet) {
        message("WARNING: File ",file.path(simeval_directory, "raw_all_iofv.csv")," not found!")
      }
      max_outlier_table <- error_table("SIMEVAL")
    }
    outliers_table <- error_table(col=1)
  }
  
  if(length(outlier_ids)<=3) {
    fig_height_outl <- 5
  } else if (length(outlier_ids)<=6){
    fig_height_outl <- 7
  } else {
    fig_height_outl <- 15
  }
  
  return(list(simeval_files_exist=simeval_files_exist,
              outliers_table=outliers_table,
              max_outlier_table=max_outlier_table,
              outlier_ids=outlier_ids,
              fig_height_outl=fig_height_outl))
}