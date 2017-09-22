added_structural_param <- function(directory, idv, dvid) {
  resmod_file_exists <- get_resmod_table(directory, idv)$resmod_file_exists
  if(resmod_file_exists) {
    resmod_table <- get_resmod_table(directory, idv)$resmod_table
    if(missing(dvid) || dvid=='NA') {
      parameters <- resmod_table %>%
        filter(model=="idv_varying_theta")
    } else {
      parameters <- resmod_table %>%
        filter(dvid==!!dvid,model=="idv_varying_theta")
    }
    if(!is.na(parameters$parameters) && parameters$parameters != "NA") {
      added_param <- data.frame(str=unlist(stringr::str_split(parameters$parameters, ","))) %>%
        tidyr::separate(str, c("variable", "value"), "=" ) %>% 
        filter(!grepl("_", variable)) %>%
        select(value) %>%
        unique() %>%
        nrow()
    } else {
      added_param <- ""
    }
  } else {
    added_param <- ""
  }
  return(added_param)
}