get_resmod_structural_dofv <- function(directory, suffix) {
  resmod_file_exists <- get_resmod_table(directory, suffix)$resmod_file_exists
  if(resmod_file_exists) {
    resmod_structural_dofv <- get_resmod_table(directory, suffix)$resmod_table %>%
      filter(model=="idv_varying_theta") %>%
      select(dofv) %>%
      round(1) %>%
      as.character() # so we can see NA values
  } else {
    resmod_structural_dofv <- "ERROR"
  }
  return(resmod_structural_dofv)
}