individual_plots <- function(file_name,ID_nr,quiet=F) {
  if(file.exists(file_name) && (length(ID_nr)!=0) ) {
    p <- xpose::xpose_data(file=file_name) %>%
      xpose::mutate(IPRED=OPRED) %>% 
      xpose::set_var_types(ipred="IPRED") %>% 
      dplyr::filter(ID %in% !!ID_nr) %>%
      xpose::ind_plots(nrow=4, ncol=3, title="Individual plots",subtitle="",caption="") + labs(color = NULL, linetype = NULL, alpha = NULL) + theme_bw()
    return(p)
  } else {
    if(!file.exists(file_name) && !quiet) {
      message("WARNING: File ",file_name," not found!")
    }
    if(length(ID_nr)==0 && !quiet) {
      message("WARNING: Individual plots will not be made, ID_nr vector is empty!")
    }
  }
}

