resmod_structural_details_tables <- function(working.directory,base_dataset,original_max0_model,extra_table,idv_all,dvid_name,nonlinear,quiet=F) {
  resmod_structural_details_list <- list()
  if(length(idv_all)!=0) {
    #check if dvid exist
    dvid_nr_idv <- list()
    resmod_file_exists_idv <- list()
    resmod_table <- list()
    for (i in 1:length(idv_all)) {
      resmod_table_list <- get_resmod_table(directory=working.directory, idv_all[i])
      resmod_file_exists_idv[i] <- resmod_table_list$resmod_file_exists
      resmod_table[[i]] <- resmod_table_list$resmod_table
      dvid_nr_idv[[i]] <- find_dvid_values(working.directory,idv_all[i],dvid_name)
    }
    
    k <- 0
    for(i in 1:length(idv_all)) {
      for (j in 1:length(dvid_nr_idv[[i]])) {  
        k <- k + 1
        idv <- idv_all[i]
        if(length(dvid_nr_idv[[i]])==1 && dvid_nr_idv[[i]]=='NA') {
          idv_text <- idv_all[i]
        } else {
          idv_text <- paste0(idv_all[i]," (",dvid_name,"=",dvid_nr_idv[[i]][j],")")
        }
        dOFV = get_resmod_structural_dofv(working.directory, idv,dvid=dvid_nr_idv[[i]][j])
        first_table = data.frame(c("DV","IDV","dOFV"),c("CWRES",idv,dOFV),stringsAsFactors = F)
        colnames(first_table) <- NULL
        
        orig_ext_file <- sub("(\\.[^.]+)$",".ext",original_max0_model)
        if(file.exists(orig_ext_file) && file.exists(base_dataset) && file.exists(extra_table) &&
          resmod_file_exists_idv[i]==TRUE &&
          !all(resmod_table[[i]]$parameters=="NA") &&
          nonlinear==FALSE) {
            
          table = get_resmod_structural_details(directory=working.directory, suffix = idv, dvid=dvid_nr_idv[[i]][j]) %>%
            .calc_and_add_shift_from_cwres(orig_ext_file,base_dataset,extra_table,idv,dvid=dvid_nr_idv[[i]][j],dvid_name)
            
          second_table = data.frame(C1=paste0(format(table$bin_min,nsmall=2),"  :  ",format(table$bin_max,nsmall=2)),C2=as.character(format(round(table$value,2),nsmall=2)),
                                    stringsAsFactors = F)
          if(any(is.na(table$relative_shift))) {
            second_table <- cbind(second_table,C3=as.character(format(round(table$shift,2),nsmall=2)),stringsAsFactors = F)
            colnames(second_table) <- c("Bin","CWRES","CPRED")
          } else {
            second_table <- cbind(second_table,C3=as.character(round(table$relative_shift,0)),stringsAsFactors = F)
            colnames(second_table) <- c("Bin","CWRES","%CPRED")
          }
            
        } else {
          second_table = data.frame(C1="ERROR",C2="ERROR",C3="ERROR",stringsAsFactors = F)
          colnames(second_table) <- c("Bin","CWRES","%CPRED")
          table <- error_table(col=1)
        }
          
        perc <- FALSE
        if(any(colnames(second_table)=="%CPRED")) {
          perc <- TRUE
        }
          
        resmod_structural_details_list[[k]] <- list(idv = idv,
                                                    dvid=dvid_nr_idv[[i]][j],
                                                    idv_text=idv_text,
                                                    dOFV = dOFV,
                                                    first_table = first_table,
                                                    second_table = second_table,
                                                    table=table,
                                                    perc=perc)
      }
      #print a message
      if(resmod_file_exists_idv[i]==TRUE && all(resmod_table[[i]]$parameters=="NA") && !quiet) {
        message("WARNING: In the file ",file.path(working.directory, paste0("resmod_", idv_all[[i]]), "results.csv")," all parameter values are 'NA'!")
      }
    }
    #print messages if file not exist
    if(!file.exists(orig_ext_file) && !quiet) {
      message("WARNING: File orig_ext_file=",orig_ext_file," not found!")
    }
    if(!file.exists(base_dataset) && !quiet) {
      message("WARNING: File base_dataset=",base_dataset," not found!")
    }
    if(!file.exists(extra_table) && !quiet) {
      message("WARNING: File extra_table=",extra_table," not found!")
    }
    
    #organize results
    dvid_nr_unique <- unique(unlist(dvid_nr_idv))
    if(length(dvid_nr_unique)!=1 && dvid_nr_unique!='NA') {
      resmod_structural_details <- list()
      k <- 1
      for(i in 1:length(dvid_nr_unique)) {
        for(j in 1:length(resmod_structural_details_list)) {
          if(grepl(paste0("(",dvid_name,"=",dvid_nr_unique[i],")"),resmod_structural_details_list[[j]]$idv_text)) {
            resmod_structural_details[[k]] <- resmod_structural_details_list[[j]]
            k <- k + 1
          }
        }
      }
    } else {
      resmod_structural_details <- resmod_structural_details_list
    }

    
  } else {
    resmod_structural_details <- error_table(col=1)
  }
  
  
  return(resmod_structural_details)
}