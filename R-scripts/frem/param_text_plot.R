param_text_plot <- function(outTable_text) {
  # create plot of text table
  data_table <- ggplot(outTable_text,aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
    geom_text(size = 4, hjust = 0, vjust = 0.2) + theme_bw() +
    geom_hline(aes(yintercept = c(nrow(outTable) + 0.5))) +
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size = 12, colour = "white"),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(1,1,1,0), "cm")) +
    labs(x="",y="") +
    coord_cartesian(xlim = c(1,5))
  return(data_table)
}
