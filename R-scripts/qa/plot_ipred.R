plot_ipred <- function(table,idv) {
  ipred_plot <- ggplot(table, aes(bin_mean, relative_shift)) +
    geom_vline(aes(xintercept=bin_max), color="darkgray")+
    geom_hline(yintercept=0, linetype="dashed")+
    geom_line()+
    geom_point(color="darkred")+
    xlab(toupper(idv))+
    ylab("Estimated bias (%IPRED)")+
    theme_bw()
  
  return(ipred_plot)
}