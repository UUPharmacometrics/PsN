plot_param_infl_outl_data <- function(table_for_plot,ID,row,row_outl_not_infl,row_infl_not_outl,cutoff_cook,outlying_criteria) {
  if (length(row)>0 || length(row_outl_not_infl)>0 || length(row_infl_not_outl)>0) {
    row_ID_nr <- c(row,row_outl_not_infl,row_infl_not_outl)
    x_simeval_EBE_NPDE <- table_for_plot$simeval_EBE_NPDE[-row_ID_nr]
    y_cdd_cook_scores <- table_for_plot$cdd_cook_scores[-row_ID_nr]
  } else {
    simeval_EBE_NPDE <- table_for_plot$simeval_EBE_NPDE
    cdd_cook_scores <- table_for_plot$cdd_cook_scores
  }
  
  # add more space around the plot
  par(oma=c(0,1,2,1))
  # plot
  ylab_text <- paste0("CDD based influential individuals, cook scores (cutoff = ",cutoff_cook," )")
  xlab_text <- paste0("Simeval based outliers, EBE_NPDE (outlying_criteria = ",outlying_criteria," )")
  plot (x_simeval_EBE_NPDE,y_cdd_cook_scores,
        type="p",
        ylab=ylab_text,
        xlab=xlab_text,
        ylim=c(min(table_for_plot$cdd_cook_scores, na.rm=T),max(table_for_plot$cdd_cook_scores, na.rm=T)),
        xlim=c(min(table_for_plot$simeval_EBE_NPDE,na.rm=T),max(table_for_plot$simeval_EBE_NPDE,na.rm=T))
  )
  abline(h=cutoff_cook, lwd=2, lty=3, col="black")
  abline(v=outlying_criteria, lwd=2, lty=3, col="black")
  title("Parameter based CDD influential individuals and simeval outliers",line=3)
  if (length(row) > 0) {
    text(table_for_plot$simeval_EBE_NPDE[row],table_for_plot$cdd_cook_scores[row], labels=ID,cex=.8, col="red")
  }
  if (length(row_outl_not_infl) > 0) {
    text(table_for_plot$simeval_EBE_NPDE[row_outl_not_infl],table_for_plot$cdd_cook_scores[row_outl_not_infl], labels=table_for_plot$ID[row_outl_not_infl],cex=.8, col="black")
  }
  if (length(row_infl_not_outl) > 0) {
    text(table_for_plot$simeval_EBE_NPDE[row_infl_not_outl],table_for_plot$cdd_cook_scores[row_infl_not_outl], labels=table_for_plot$ID[row_infl_not_outl],cex=.8, col="black")
  }
  mtext("In red color are ID numbers of the individuals which are both: outliers and influential individuals",side=3,line=1,col = "red")
}
