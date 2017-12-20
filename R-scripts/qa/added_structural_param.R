added_structural_param <- function(directory, idv, dvid="NA") {
  resmod_table_list <- get_resmod_table(directory, idv)
  resmod_file_exists <- resmod_table_list$resmod_file_exists
  if(resmod_file_exists) {
    resmod_table <- resmod_table_list$resmod_table
    if(dvid=='NA') {
      parameters <- resmod_table %>%
        dplyr::filter(model=="idv_varying_theta")
    } else {
      parameters <- resmod_table %>%
        dplyr::filter(dvid==!!dvid,model=="idv_varying_theta")
    }
    if(!is.na(parameters$parameters) && parameters$parameters != "NA") {
      added_param <- data.frame(str=unlist(stringr::str_split(parameters$parameters, ","))) %>%
        tidyr::separate(str, c("variable", "value"), "=" ) %>% 
        dplyr::filter(!grepl("_", variable)) %>%
        dplyr::select(value) %>%
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