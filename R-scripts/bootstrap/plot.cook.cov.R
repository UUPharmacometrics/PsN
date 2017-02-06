plot_cook_cov <- function(data_plots,failed_cov_ID) {
  #need ggplot2 and grid libraries
  #create plot object
  p_1 <- ggplot(data_plots,aes(x=cook.scores,y=cov.ratios,label=ID))+
    geom_text()
  
  #add text if no information on some ID
  if (length(failed_cov_ID) > 0) {
    #plot text
    p_1 <- p_1 + theme(plot.margin = unit(c(1,1,2,1), "cm")) +
      annotation_custom(grob = textGrob(paste("No information about ID numbers:",paste(as.character(failed_cov_ID),collapse = ", "))),  
                        xmin = min(data_plots$cook.scores), 
                        xmax = max(data_plots$cook.scores), 
                        ymin = (min(data_plots$cov.ratios)-(max(data_plots$cov.ratios)-min(data_plots$cov.ratios))/5), 
                        ymax = (min(data_plots$cov.ratios)-(max(data_plots$cov.ratios)-min(data_plots$cov.ratios))/5))
  }
  
  gt <- ggplotGrob(p_1)
  gt$layout$clip[gt$layout$name=="panel"] <- "off"
  #plot
  grid.draw(gt)
}