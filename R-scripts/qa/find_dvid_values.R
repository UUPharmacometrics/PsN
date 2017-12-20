find_dvid_values <- function(directory,idv,dvid_name) {
  #check if dvid exist
  resmod_table_list <- get_resmod_table(directory, idv)
  resmod_file_exists_idv <- resmod_table_list$resmod_file_exists
  if(resmod_file_exists_idv && dvid_name!='') {
    resmod_table <- resmod_table_list$resmod_table
    if(any(resmod_table$dvid!="NA")) {
      dvid_nr <- unique(resmod_table$dvid)
      if(any(dvid_nr=="sum")) {
        dvid_nr <- as.numeric(dvid_nr[-which(dvid_nr=="sum")])
      } else {
        dvid_nr <- as.numeric(dvid_nr)
      }
    } else {
      dvid_nr <- 'NA'
    }
  } else {
    dvid_nr <- 'NA'
  }
  return(dvid_nr)
}