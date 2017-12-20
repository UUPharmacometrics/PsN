# get resmod result.csv files (can't open like usually, besause there are more column than column names)
get_resmod_table <- function(directory, idv,quiet=F){
  resmod_file_exists <- file.exists(file.path(directory, paste0("resmod_", idv), "results.csv"))
  if(resmod_file_exists) {
    path <- file.path(directory, paste0("resmod_", idv), "results.csv") 
    con <- file(path)
    lines <- readLines(con)
    close(con)
    fields <- stringr::str_split(lines, ",")
    #delete empty values ("") in each list, if exists
    for(i in 1:length(fields)) {
      if(any(fields[[i]] == "")) {
        fields[[i]] <- fields[[i]][which(fields[[i]]!="")]
      }
    }
    header <- fields[[1]]
    fields[[1]] <- NULL
  
    for(i in 1:length(fields)) {
      if(length(fields[[i]])<length(header)) {
        fields[[i]] <- c(fields[[i]],rep("NA",(length(header)-length(fields[[i]]))))
      }
    }
    
    resmod_table <- plyr::ldply(fields, function(l){
      fields_with_header <- l[seq_along(header)]
      names(fields_with_header) <- tolower(header)
      fields_with_header[[length(header)]] <-  paste0(l[length(header):length(l)], collapse=",")
      fields_with_header
    }) %>%
      rename(dOFV=dofv)
    
    new_dofv <- c()
    for(i in 1:nrow(resmod_table)) {
      #if have "NA" in dOFV
      if(resmod_table$dOFV[i]=='NA') {
        new_dofv[i] <- as.numeric(NA)
      } else {
        new_dofv[i] <- abs(as.numeric(resmod_table$dOFV[i]))
      }
      # if have DVID values
      if(resmod_table$dvid[i]!="sum" && resmod_table$dvid[i]!="NA") {
        resmod_table$dvid[i] <- as.character(as.numeric(resmod_table$dvid[i]))
      } 
    }
    resmod_table$dOFV <- new_dofv
    return(list(resmod_file_exists=resmod_file_exists,
                resmod_table=resmod_table))
  } else {
    if(!quiet) {
      message("WARNING: File ",file.path(directory, paste0("resmod_", idv), "results.csv")," not found!")
    }
    return(list(resmod_file_exists=resmod_file_exists))
  }
}