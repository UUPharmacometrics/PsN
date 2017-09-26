individual_plots <- function(file_name,ID_nr) {
  if(file.exists(file_name) && (length(ID_nr)!=0) ) {
    p <- xpose::xpose_data(file=file_name) %>%
      xpose::set_var_types(ipred="OPRED") %>% 
      dplyr::filter(ID %in% !!ID_nr) %>%
      xpose::ind_plots(nrow=4, ncol=3, title="Individual plots",subtitle="",caption="") + labs(color = NULL, linetype = NULL, alpha = NULL) + theme_bw()
    return(p)
  }
}

