resmod_structural_details_tables <- function(working.directory,model.directory,model.filename,CWRES_table,resmod_suffix,idv_name) {
  resmod_structural_details <- list()
  for(i in 1:length(resmod_suffix)) {
    idv <- resmod_suffix[i]
    dofv = get_resmod_structural_dofv(working.directory, idv)
    first_table = data.frame(C1=c("DV","IDV","dOFV"),C2=c("CWRES",idv,dofv),stringsAsFactors = F)
    if(file.exists(file.path(model.directory, paste0(sub('.mod.*','',model.filename),".ext"))) &&
       file.exists(file.path(working.directory, paste0(sub('.mod.*','',model.filename),"_linbase.dta"))) &&
       file.exists(CWRES_table)) {
      table = .get_resmod_structural_details(working.directory, suffix = idv) %>%
        .calc_and_add_shift_from_cwres(working.directory=working.directory, model.directory=model.directory, 
                                       model.filename = model.filename,CWRES_table=CWRES_table,idv = idv,idv_name = idv_name)
      second_table = data.frame(C1=paste0(table$bin_min,"-",table$bin_max),C2=round(table$value,2),
                                C3=round(table$relative_shift,2),stringsAsFactors = F)
      colnames(second_table) <- c("Bin","CWRES","IPRED")

    } else {
      second_table = data.frame(C1="ERROR",C2="ERROR",C3="ERROR",stringsAsFactors = F)
      colnames(second_table) <- c("Bin","CWRES","IPRED")
    }
    resmod_structural_details[[i]] <- list(idv = idv,
                                           dofv = dofv,
                                           first_table = first_table,
                                           second_table = second_table)
  }
  return(resmod_structural_details)
}