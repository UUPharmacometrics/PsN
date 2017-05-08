resmod_structural_details_tables <- function(working.directory,model.directory,model.filename,CWRES_table,resmod_suffix,idv_name) {
  resmod_structural_details <- list()
  if(length(resmod_suffix)!=0) {
    for(i in 1:length(resmod_suffix)) {
      idv <- resmod_suffix[i]
      dofv = get_resmod_structural_dofv(working.directory, idv)
      first_table = data.frame(C1=c("DV","IDV","dOFV"),C2=c("CWRES",idv,dofv),stringsAsFactors = F)
      if(file.exists(file.path(model.directory, paste0(sub('.mod.*','',model.filename),".ext"))) &&
         file.exists(file.path(working.directory, paste0(sub('.mod.*','',model.filename),"_linbase.dta"))) &&
         file.exists(CWRES_table)) {
        
        table = .get_resmod_structural_details(working.directory, suffix = idv) %>%
          .calc_and_add_shift_from_cwres(working.directory,model.directory,model.filename,CWRES_table,idv,idv_name)

        second_table = data.frame(C1=paste0(format(table$bin_min,nsmall=2),"  :  ",format(table$bin_max,nsmall=2)),C2=as.character(format(round(table$value,2),nsmall=2)),
                                    C3=as.character(round(table$relative_shift,0)),stringsAsFactors = F)

        colnames(second_table) <- c("Bin","CWRES","%IPRED")
        
      } else {
        second_table = data.frame(C1="ERROR",C2="ERROR",C3="ERROR",stringsAsFactors = F)
        colnames(second_table) <- c("Bin","CWRES","%IPRED")
      }
      resmod_structural_details[[i]] <- list(idv = idv,
                                             dofv = dofv,
                                             first_table = first_table,
                                             second_table = second_table,
                                             table=table)
    }
  } else {
    resmod_structural_details <- error_table(col=1)
  }

  return(resmod_structural_details)
}