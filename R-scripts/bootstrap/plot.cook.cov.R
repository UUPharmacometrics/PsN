plot_cook_cov <- function(data_plots,failed_cov_ID,samples,estimation_failures) {
  #need ggplot2 and grid libraries
  #create plot object
  ggplot2::theme_set(ggplot2::theme_bw(base_size=20)) #set usual background
  p_1 <- ggplot2::ggplot(data_plots,ggplot2::aes(x=cook.scores,y=cov.ratios,label=ID))+
    ggplot2::geom_text()
  
  #add text if no information on some ID
  if (length(failed_cov_ID) > 0) {
    #plot text
    p_1 <- p_1 + ggplot2::theme(plot.margin = ggplot2::unit(c(0.5,0.5,1,0.5), "cm")) +
      ggplot2::annotation_custom(grob = grid::textGrob(paste("No information about ID numbers:",paste(as.character(failed_cov_ID),collapse = ", "))),  
                        xmin = min(data_plots$cook.scores), 
                        xmax = max(data_plots$cook.scores), 
                        ymin = (min(data_plots$cov.ratios)-(max(data_plots$cov.ratios)-min(data_plots$cov.ratios))/4.7), 
                        ymax = (min(data_plots$cov.ratios)-(max(data_plots$cov.ratios)-min(data_plots$cov.ratios))/4.7))
  } else {
    p_1 <- p_1 + ggplot2::theme(plot.margin = ggplot2::unit(c(0.5,0.5,0.5,0.5), "cm"))
  }
  
  gt <- ggplot2::ggplotGrob(p_1)
  gt$layout$clip[gt$layout$name=="panel"] <- "off"
  
  return(gt)
}