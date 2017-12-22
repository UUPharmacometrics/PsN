get_ii_table <- function(cdd_directory,model.filename,cutoff,max_rows,skip,nonlinear,quiet=F){
  skipped.id.file <- file.path(cdd_directory,"skipped_individuals1.csv")
  if(!nonlinear) {
    raw.results.file <- file.path(cdd_directory,paste0("raw_results_",sub('.([^.]*)$','',model.filename),"_linbase.csv"))
  } else {
    raw.results.file <- file.path(cdd_directory,paste0("raw_results_",sub('.([^.]*)$','',model.filename),".csv"))
  }
  
  cdd_files_exist <- TRUE
  infl_id <- c()
  all_dofv <- c()
  if(file.exists(raw.results.file) && file.exists(skipped.id.file) && all(skip!="cdd")) {
    data_full <- create.data.full(raw.results.file,skipped.id.file)
    cdd.data.all <- data_full$cdd.data.all
    if(any(colnames(cdd.data.all)=="cdd.delta.ofv")) {
      all_dofv <- cdd.data.all$cdd.delta.ofv[-1]
      cdd.data.all <- cdd.data.all %>% dplyr::select(c(ID,cdd.delta.ofv)) %>% slice(-1)
      colnames(cdd.data.all) <- c("id", "dOFV")
      
      #find negative delta ofv values, if exist
      fail_ID <- c()
      if(!all(is.na(cdd.data.all$dOFV))) {
        if (any(cdd.data.all$dOFV < 0)) {
          negat.delta.row <- which(cdd.data.all$dOFV < 0)
          fail_ID <- cdd.data.all$id[negat.delta.row]
          cdd.data <- as.data.frame(cdd.data.all[-negat.delta.row,])
          if(nrow(cdd.data)==0) {
            cdd.data <- data.frame()
          }
        } else {
          cdd.data <- as.data.frame(cdd.data.all)
        }
      } else {
        if(!quiet) {
          message("ERROR: In the file ",raw.results.file," all dofv values are NA!")
        }
        cdd.data <- data.frame()
      }
      
      if(nrow(cdd.data)!=0) {
        #get individual with the highest dofv
        cdd_highest_dofv <- cdd.data[which.max(cdd.data$dOFV),]
        cdd_highest_dofv <- cdd_highest_dofv %>%
          dplyr::mutate(id=paste("Subject",id))
        colnames(cdd_highest_dofv) <- c("","dOFV")
        
        
        # find influential individuals, where delta ofv values are bigger than cutoffs
        if(any(cdd.data$dOFV > cutoff)) {
          ii_table <- as.data.frame(subset(cdd.data,dOFV > cutoff))
          ii_table <- ii_table[order(ii_table$dOFV,decreasing = T),]
          ii_table[,2] <- format(round(as.numeric(ii_table[,2]),1),trim=T,digits=1,nsmall=1)
          infl_id <- as.numeric(ii_table$id)
          ii_table <- ii_table %>%
            dplyr::mutate(id=paste("Subject",id))
          colnames(ii_table)[which(colnames(ii_table)=="id")] <- "Subjects"
          
          # only cdd max rows
          if(nrow(ii_table) > max_rows) {
            ii_table <- ii_table[1:max_rows,]
          }
        } else {
          ii_table <- data.frame(c("No influential individuals detected"),stringsAsFactors = F)
          colnames(ii_table) <- NULL
          cdd_highest_dofv <- data.frame("None","",stringsAsFactors = F)
          colnames(cdd_highest_dofv) <- c("","dOFV")
        }
      } else {
        cdd_highest_dofv <- data.frame("All dOFV values are negative","",stringsAsFactors = F)
        colnames(cdd_highest_dofv) <- c("","dOFV")
        ii_table <- data.frame(c("All dOFV values are negative"),stringsAsFactors = F)
        colnames(ii_table) <- NULL
      }
    } else {
      if(!quiet) {
        message("WARNING: No column 'cdd.delta.ofv' found in the file ",raw.results.file,"!")
      }
      ii_table <- error_table(col=1)
      cdd_highest_dofv <- error_table("CDD")
      cdd.data <- error_table(col=1)
    }
  } else {
    cdd_files_exist <- FALSE
    if(any(skip=="cdd")) {
      cdd_highest_dofv <- data.frame("CDD","SKIPPED",stringsAsFactors = F)
      colnames(cdd_highest_dofv) <- c("","dOFV")
      cdd.data <- data.frame("SKIPPED",stringsAsFactors = F)
      colnames(cdd.data) <- NULL
    } else {
      if(!file.exists(raw.results.file) && !quiet) {
        message("WARNING: File ",raw.results.file," not found!")
      }
      if(!file.exists(skipped.id.file) && !quiet) {
        message("WARNING: File ",skipped.id.file," not found!")
      }
      cdd_highest_dofv <- error_table("CDD")
      cdd.data <- error_table(col=1)
    }
    ii_table <- error_table(col=1)
  }
  
  if(length(infl_id)<=3) {
    fig_height_infl <- 5
  } else if(length(infl_id)<=6) {
    fig_height_infl <- 7
  } else {
    fig_height_infl <- 15
  }
  
  return(list(cdd_files_exist=cdd_files_exist,
              all_dofv=all_dofv,
              cdd.data=cdd.data,
              cdd_highest_dofv=cdd_highest_dofv,
              ii_table=ii_table,
              infl_id=infl_id,
              fig_height_infl=fig_height_infl))
}