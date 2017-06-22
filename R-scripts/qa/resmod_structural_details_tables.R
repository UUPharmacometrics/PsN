resmod_structural_details_tables <- function(working.directory,model.filename,CWRES_table,idv_all,idv_name) {
  resmod_structural_details <- list()
  if(length(idv_all)!=0) {
    #check if dvid exist
    resmod_file_exists_idv <- c()
    dvid_nr_idv <- list()
    for (n in 1:length(idv_all)) {
      resmod_file_exists <- get_resmod_table(directory=working.directory, idv_all[n])$resmod_file_exists
      resmod_file_exists_idv[n] <- resmod_file_exists
      if(resmod_file_exists) {
        resmod_table <- get_resmod_table(directory=working.directory, idv_all[n])$resmod_table
        if(any(resmod_table$dvid!="NA")) {
          dvid_nr <- unique(resmod_table$dvid)
          dvid_nr <- as.numeric(dvid_nr[-which(dvid_nr=="sum")])
        } else {
          dvid_nr <- 'NA'
        }
      } else {
        dvid_nr <- 'NA'
      }
      dvid_nr_idv[[n]] <- dvid_nr
    }

    k <- 0
    for (j in 1:length(dvid_nr)) {
      for(i in 1:length(idv_all)) {
        k <- k + 1
        idv <- idv_all[i]
        if(length(dvid_nr)==1 && dvid_nr=='NA') {
          idv_text <- idv_all[i]
        } else {
          idv_text <- paste0(idv_all[i]," (DVID=",dvid_nr[j],")")
        }
        dOFV = get_resmod_structural_dofv(working.directory, idv,dvid=dvid_nr[j])
        first_table = data.frame(C1=c("DV","IDV","dOFV"),C2=c("CWRES",idv,dOFV),stringsAsFactors = F)
        if(file.exists(paste0(working.directory, "linearize_run/scm_dir1/derivatives.ext")) &&
          file.exists(file.path(working.directory, paste0(sub('.([^.]*)$','',model.filename),"_linbase.dta"))) &&
          file.exists(CWRES_table) &&
          resmod_file_exists_idv[i]==TRUE) {
            
          table = get_resmod_structural_details(directory=working.directory, suffix = idv, dvid=dvid_nr[j]) %>%
            .calc_and_add_shift_from_cwres(working.directory,model.filename,CWRES_table,idv,idv_name)
            
          second_table = data.frame(C1=paste0(format(table$bin_min,nsmall=2),"  :  ",format(table$bin_max,nsmall=2)),C2=as.character(format(round(table$value,2),nsmall=2)),
                                    stringsAsFactors = F)
          if(any(is.na(table$relative_shift))) {
            second_table = data.frame(C1=paste0(format(table$bin_min,nsmall=2),"  :  ",format(table$bin_max,nsmall=2)),C2=as.character(format(round(table$value,2),nsmall=2)),
                                      C3=as.character(format(round(table$shift,2),nsmall=2)),
                                        stringsAsFactors = F)
            colnames(second_table) <- c("Bin","CWRES","CPRED")
          } else {
            second_table = data.frame(C1=paste0(format(table$bin_min,nsmall=2),"  :  ",format(table$bin_max,nsmall=2)),C2=as.character(format(round(table$value,2),nsmall=2)),
                                      C3=as.character(round(table$relative_shift,0)),
                                      stringsAsFactors = F)
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
          
        resmod_structural_details[[k]] <- list(idv = idv,
                                                idv_text=idv_text,
                                                dOFV = dOFV,
                                                first_table = first_table,
                                                second_table = second_table,
                                                table=table,
                                                perc=perc)
      }
    }

    
  } else {
    resmod_structural_details <- error_table(col=1)
  }
  
  
  return(resmod_structural_details)
}