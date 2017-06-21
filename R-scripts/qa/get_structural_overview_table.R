get_structural_overview_table <- function(directory,idv,groups) {
  if(length(length(idv))!=0) {
    #check if dvid exist
    resmod_file_exists <- get_resmod_table(directory, idv[1])$resmod_file_exists
    if(resmod_file_exists) {
      resmod_table <- get_resmod_table(directory, idv[1])$resmod_table
      if(any(resmod_table$dvid!="NA")) {
        dvid_nr <- unique(resmod_table$dvid)
        dvid_nr <- as.numeric(dvid_nr[-which(dvid_nr=="sum")])
      } else {
        dvid_nr <- 'NA'
      }
    } else {
      dvid_nr <- 'NA'
    }
    
    structural_overview <- as.data.frame(array(0,c((length(idv)*length(dvid_nr)),3)))
    colnames(structural_overview) <- c("","dofv","Add.params")
    k <- 0
    for(j in 1:length(dvid_nr)) {
      for(i in 1:length(idv)) {
        k <- k + 1
        if(length(dvid_nr) > 0 && dvid_nr != 'NA') {
          structural_overview[k,1] <- paste0(idv[i]," (DVID=",dvid_nr[j],")")
        } else {
          structural_overview[i,1] <- idv[i]
        }
        structural_overview[k,2] <- get_resmod_structural_dofv(directory, idv=idv[i],dvid=dvid_nr[j])
        structural_overview[k,3] <- groups-1
      }
    }
  } else {
    structural_overview <- error_table("RESMOD")
  }

  return(structural_overview)
}