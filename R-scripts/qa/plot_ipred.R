plot_ipred <- function(table,idv) {
  table$bin_mean[length(table$bin_mean)] <- Inf
  table$bin_max[length(table$bin_max)] <- Inf
  if(any(is.na(table$relative_shift))) {
    ipred_plot <- ggplot(table, aes(bin_mean, shift))+
      ylab("Estimated bias (CPRED)")
    if(any(table$shift > 100) || any(table$shift < -100)) {
      ipred_plot <- ipred_plot +
        coord_cartesian(ylim=c(-100,100))
    }
  } else {
    ipred_plot <- ggplot(table, aes(bin_mean, relative_shift))+
      ylab("Estimated bias (%CPRED)")
    if(any(table$relative_shift > 100) || any(table$relative_shift < -100)) {
      ipred_plot <- ipred_plot +
        coord_cartesian(ylim=c(-100,100))
    }
  }
  
  vertik_lines <- table$bin_max[-length(table$bin_max)]
  ipred_plot <- ipred_plot +
    geom_vline(xintercept=vertik_lines, color="darkgray")+
    geom_hline(yintercept=0, linetype="dashed")+
    geom_line()+
    geom_point(aes(size=nobs),color="blue",show.legend = F)+
    scale_size_identity()+
    xlab(toupper(idv))+
    theme_bw() 
  
  return(ipred_plot)
}