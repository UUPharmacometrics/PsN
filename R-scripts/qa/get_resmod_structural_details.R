get_resmod_structural_details <- function(directory, suffix, dvid) {
  if(!missing("dvid") || dvid!='NA') {
    resmod_structural_table <- get_resmod_table(directory, suffix)$resmod_table
    resmod_structural_table <- resmod_structural_table[which(resmod_structural_table$dvid==dvid),]
  } else {
    resmod_structural_table <- get_resmod_table(directory, suffix)$resmod_table
  }
  type_labels <- c(th = "mean", sdeps = "sd")
  resmod_structural_table <- resmod_structural_table %>%
    dplyr::filter(model == 'idv_varying_theta') %>%
    dplyr::mutate(model = 'theta',idv = suffix) %>%
    dplyr::group_by(model) %>%
    dplyr::do(
      {
        all_params <- data.frame(str=unlist(stringr::str_split(.$parameters, ","))) %>%
          tidyr::separate(str, c("variable", "value"), "=" )
        bin_boundaries <- all_params %>%
          dplyr::filter(!grepl("_", variable)) %>%
          dplyr::rename(bin_value = value)
        all_params %>%
          dplyr::filter(grepl("_", variable)) %>%
          tidyr::separate(variable, c("type","bin"), "_") %>%
          tidyr::extract(bin, into=c("bin_min","bin_max"), '(.*)\\-([^-]+)$') %>%
          dplyr::left_join(bin_boundaries, c(bin_min = "variable")) %>%
          dplyr::mutate(bin_value = ifelse(is.na(bin_value), paste0(bin_min,".00 "), bin_value)) %>%
          dplyr::rename(bin_min_value = bin_value) %>%
          dplyr::left_join(bin_boundaries, c(bin_max = "variable")) %>%
          dplyr::mutate(bin_value = ifelse(is.na(bin_value), paste0(bin_max,".00 "), bin_value),
                 type = type_labels[type]) %>%
          dplyr::rename(bin_max = bin_value,
                 bin_min = bin_min_value)
      })%>%
    dplyr::ungroup() %>%
    dplyr::mutate_at(c("value", "bin_min", "bin_max"),funs(as.numeric)) %>%
    dplyr::mutate_at(c("bin_min", "bin_max"),funs(round(.,2)))
  
  return(resmod_structural_table)
}