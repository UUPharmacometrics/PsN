get_resmod_structural_dofv <- function(directory, idv, dvid) {
  resmod_file_exists <- get_resmod_table(directory, idv)$resmod_file_exists
  if(resmod_file_exists) {
    if(missing(dvid) || dvid=='NA') {
      resmod_structural_dofv <- get_resmod_table(directory, idv)$resmod_table %>%
        filter(model=="idv_varying_theta") %>%
        select(dofv) %>%
        round(1) %>%
        as.character() # so we can see NA values
    } else {
      resmod_structural_table <- get_resmod_table(directory, idv)$resmod_table
      resmod_structural_table <- resmod_structural_table[which(resmod_structural_table$dvid==dvid),]
      resmod_structural_dofv <- resmod_structural_table %>% filter(model=="idv_varying_theta") %>%
        select(dofv) %>%
        round(1) %>%
        as.character() # so we can see NA values
    }
  } else {
    resmod_structural_dofv <- "ERROR"
  }
  return(resmod_structural_dofv)
}