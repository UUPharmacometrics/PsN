plot.npsupp.nofv <- function(raw_nonparametric,n.indiv,n.eta) {

  ofv <- paste0("Parametric OFV value = ",round(raw_nonparametric$ofv[1],3))
  plot.title <- "NONPARAMETRIC ESTIMATION"
  plot.subtitle <- paste0("Parametric OFV value = ",round(raw_nonparametric$ofv[1],3),
                            ". Number of ETAs = ",n.eta)
    
  # Construct plot
  p <- ggplot(raw_nonparametric, aes(npsupp,npofv)) +
    geom_point(aes(size = 1),show.legend = FALSE) +
    geom_vline(xintercept = n.indiv, colour = "red") +
    labs(x = "NPSUPP values", y="Nonparametric OFV values") +
    theme_bw() +
    scale_x_continuous(breaks=c(raw_nonparametric$npsupp,n.indiv)) +
    scale_y_continuous(breaks=c(round(raw_nonparametric$npofv,3))) +
    theme(legend.position = "none",
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          plot.margin = unit(c(1,1,1,1), "cm")) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme(plot.title = element_text(size = 20,colour="black", vjust = 0))
  print(p)
}