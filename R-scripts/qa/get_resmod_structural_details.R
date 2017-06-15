.get_resmod_structural_details <- function(directory, suffix) {
  type_labels <- c(th = "mean", sdeps = "sd")
  #model_names <- c("idv_varying_combined"="combined","idv_varying_theta"="theta", "idv_varying_RUV"="sigma")
  get_resmod_table(directory, suffix)$resmod_table %>%
    filter(model == 'idv_varying_theta') %>%
    #mutate(model = model_names[model],idv = suffix) %>%
    mutate(model = 'theta',idv = suffix) %>%
    group_by(model) %>%
    do(
      {
        all_params <- data.frame(str=unlist(stringr::str_split(.$parameters, ","))) %>%
          tidyr::separate(str, c("variable", "value"), "=" )
        bin_boundaries <- all_params %>%
          filter(!grepl("_", variable)) %>%
          rename(bin_value = value)
        all_params %>%
          filter(grepl("_", variable)) %>%
          tidyr::separate(variable, c("type","bin"), "_") %>%
          tidyr::extract(bin, into=c("bin_min","bin_max"), '(.*)\\-([^-]+)$') %>%
          left_join(bin_boundaries, c(bin_min = "variable")) %>%
          mutate(bin_value = ifelse(bin_min=='0', 0, ifelse(bin_min=='-inf', -Inf ,bin_value))) %>%
          rename(bin_min_value = bin_value) %>%
          left_join(bin_boundaries, c(bin_max = "variable")) %>%
          mutate(bin_value = ifelse(bin_max=='inf', Inf, bin_value),
                 type = type_labels[type]) %>%
          rename(bin_max = bin_value,
                 bin_min = bin_min_value)
      })%>%
    ungroup() %>%
    mutate_each(funs(as.numeric), value, bin_min, bin_max) %>%
    mutate_each(funs(round(.,2)), bin_min, bin_max)
}