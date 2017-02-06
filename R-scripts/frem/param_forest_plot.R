param_forest_plot <- function(outTable,pardata) {
  outTable$y <- factor(c(1:nrow(outTable)), levels=c(nrow(outTable):1))
  # MAKE FOREST PLOT --------------------------------------------------------
  p <- ggplot(outTable, aes(mean,y)) +
    geom_point(aes(color = group, shape = group),size = 2) +
    geom_text(aes(label = points, color = group),size = 4, vjust = 0, nudge_y = 0.1) +
    geom_errorbarh(aes(xmax = ci_high, xmin = ci_low, color = group, height = 0.15)) +
    geom_vline(xintercept = 0, linetype = "longdash") +
    labs(x = "Effect size in percentage (%)", y="") +
    theme_bw() +
    theme(legend.position = "none",
          panel.border = element_rect(),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.line = element_line(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.margin = unit(c(1,0.1,1,1), "cm"))
  if ((2 %in% c(1:ncol(pardata))) & (3 %in% c(1:ncol(pardata)))) {
    if ((is.na(pardata[j,2]) == FALSE) & (is.na(pardata[j,3]) == FALSE)) {
      p <- p + coord_cartesian(xlim = c(pardata[j,2],pardata[j,3]))
    }
  }
  return(p)
}
