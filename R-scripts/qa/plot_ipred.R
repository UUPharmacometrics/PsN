plot_ipred <- function(table,idv) {
  if(any(is.na(table$relative_shift))) {
    ipred_plot <- ggplot(table, aes(bin_mean, shift))+
      ylab("Estimated bias (CPRED)")
  } else {
    ipred_plot <- ggplot(table, aes(bin_mean, relative_shift))+
      ylab("Estimated bias (%CPRED)")
  }
  ipred_plot <- ipred_plot + 
    geom_vline(aes(xintercept=bin_max), color="darkgray")+
    geom_hline(yintercept=0, linetype="dashed")+
    geom_line()+
    geom_point(color="blue",aes(size=nobs),show.legend = F)+
    xlab(toupper(idv))+
    theme_bw()
  
  return(ipred_plot)
}