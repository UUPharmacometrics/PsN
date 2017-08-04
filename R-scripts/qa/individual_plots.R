individual_plots <- function(file_name,ID_nr) {
  if(file.exists(file_name) && (length(ID_nr)!=0) ) {
    p <- xpose_data(file=file_name) %>% 
      set_vars_type(ipred="OPRED") %>% 
      filter(ID %in% !!ID_nr) %>% 
      ind_plots(nrow=4,ncol=3,title="Individual plots",subtitle="",caption="") + theme(legend.title=element_blank()) + theme_bw()
    return(p)
  }
}