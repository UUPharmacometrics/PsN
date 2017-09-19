get_structural_overview_table <- function(directory,idv,dvid_name,skip) {
  if(length(idv)!=0) {
    #check if dvid exist
    resmod_file_exists <- get_resmod_table(directory, idv[1])$resmod_file_exists
    if(resmod_file_exists) {
      resmod_table <- get_resmod_table(directory, idv[1])$resmod_table
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
    
    if(length(dvid_nr) == 1 && dvid_nr=="NA"){
      structural_overview <- as.data.frame(array(0,c(length(idv),3)))
    } else {
      structural_overview <- as.data.frame(array(0,c((length(idv)*length(dvid_nr)+length(dvid_nr)),3)))
    }
    colnames(structural_overview) <- c("","dOFV","Add.params")
    
    k <- 1
    for(j in 1:length(dvid_nr)) {
      if(length(dvid_nr) > 0 && dvid_nr != 'NA') {
        structural_overview[k,1] <- paste0("(",dvid_name," = ",dvid_nr[j],")")
        structural_overview[k,2] <- ''
        structural_overview[k,3] <- ''
        k <- k + 1
      }
      for(i in 1:length(idv)) {
        structural_overview[k,1] <- idv[i]
        ofv_value <- get_resmod_structural_dofv(directory, idv=idv[i],dvid=dvid_nr[j])
        if(is.na(ofv_value)) {
          structural_overview[k,2] <- 'NA'
        } else {
          structural_overview[k,2] <- ofv_value
        }
        structural_overview[k,3] <- added_structural_param(directory, idv=idv[i], dvid=dvid_nr[j])
        k <- k + 1
      }
    }
  } else {
    if(any(skip=="resmod")) {
      structural_overview <- data.frame("RESMOD","SKIPPED",stringsAsFactors = F)
      colnames(structural_overview) <- c("","dOFV")
    } else{
      structural_overview <- error_table("RESMOD")
    }
  }

  return(structural_overview)
}