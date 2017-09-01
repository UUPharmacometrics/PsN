plot_ipred <- function(table,idv) {
  table$bin_mean[length(table$bin_mean)] <- Inf
  table$bin_max[length(table$bin_max)] <- Inf
  if(any(is.na(table$relative_shift))) {
    ipred_plot <- ggplot(table, aes(bin_mean, shift))+
      ylab("Estimated bias (CPRED)")
  } else {
    ipred_plot <- ggplot(table, aes(bin_mean, relative_shift))+
      ylab("Estimated bias (%CPRED)")
  }
  
  vertik_lines <- table$bin_max[-length(table$bin_max)]
  ipred_plot <- ipred_plot +
    geom_vline(xintercept=vertik_lines, color="darkgray")+
    geom_hline(yintercept=0, linetype="dashed")+
    geom_line()+
    geom_point(color="blue",aes(size=nobs),show.legend = F)+
    xlab(toupper(idv))+
    theme_bw() 
  
  return(ipred_plot)
}