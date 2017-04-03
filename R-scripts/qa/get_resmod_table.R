# get resmod result.csv files (can't open like usually, besause there are more column than column names)
get_resmod_table <- function(directory, suffix){
  resmod_file_exists <- file.exists(file.path(directory, paste0("resmod_", suffix), "results.csv"))
  if(resmod_file_exists) {
    path <- file.path(directory, paste0("resmod_", suffix), "results.csv") 
    con <- file(path)
    lines <- readLines(con)
    close(con)
    fields <- stringr::str_split(lines, ",")
    # delete empty values ("") in each list, if exists
    for(i in 1:length(fields)) {
      if(any(fields[[i]] == "")) {
        fields[[i]] <- fields[[i]][which(fields[[i]]!="")]
      }
    }
    header <- fields[[1]]
    fields[[1]] <- NULL
    resmod_table <- plyr::ldply(fields, function(l){
      fields_with_header <- l[seq_along(header)]
      names(fields_with_header) <- tolower(header)
      fields_with_header[[length(header)]] <-  paste0(l[length(header):length(l)], collapse=",")
      fields_with_header
    }) %>%
      mutate(dofv = -scan(text=paste0(dofv)))
    return(list(resmod_file_exists=resmod_file_exists,
                resmod_table=resmod_table))
  } else {
    return(list(resmod_file_exists=resmod_file_exists))
  }
}