get_scm_table <- function(rawres_file){
  scm_files_exists <- file.exists(rawres_file)
  if(scm_files_exists) {
    scm_table <- read.csv(rawres_file,stringsAsFactors = F) %>%
      mutate(dofv = ofv[step.number==0]-ofv) %>%
      slice(-1) %>%
      select(relation, dofv)
    colnames(scm_table) <- c("","dofv")
  } else {
    scm_table <- error_table("SCM")
  }
  return(list(scm_files_exists=scm_files_exists,
              scm_table=scm_table))
}