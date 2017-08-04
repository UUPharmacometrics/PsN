get_resmod_structural_dofv <- function(directory, idv, dvid) {
  resmod_file_exists <- get_resmod_table(directory, idv)$resmod_file_exists
  if(resmod_file_exists) {
    if(missing(dvid) || dvid=='NA') {
      resmod_structural_dofv <- get_resmod_table(directory, idv)$resmod_table %>%
        filter(model=="idv_varying_theta") %>%
        select(dOFV) %>%
        as.numeric()
    } else {
      resmod_structural_dofv <- get_resmod_table(directory, idv)$resmod_table %>%
        filter(dvid==!!dvid,model=="idv_varying_theta") %>%
        select(dOFV) %>%
        as.numeric()
    }
  } else {
    resmod_structural_dofv <- "ERROR"
  }
  return(resmod_structural_dofv)
}