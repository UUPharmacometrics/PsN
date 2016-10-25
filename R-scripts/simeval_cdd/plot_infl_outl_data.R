plot_infl_outl_data <- function(table_for_plot,ID,row) {
  par(oma=c(0,1,2,1))
  plot (table_for_plot$simeval_iofv_res[-row],table_for_plot$cdd_delta.ofv[-row],
        type="p",
        ylab="CDD",
        xlab="Simeval",
        ylim=c(min(table_for_plot$cdd_delta.ofv, na.rm=T),max(table_for_plot$cdd_delta.ofv, na.rm=T)),
        xlim=c(min(table_for_plot$simeval_iofv_res,na.rm=T),max(table_for_plot$simeval_iofv_res,na.rm=T))
  )
  title("Cdd influential individuals and simeval outliers",line=3)
  if (length(row) > 0) {
    text(table_for_plot$simeval_iofv_res[row],table_for_plot$cdd_delta.ofv[row], labels=ID,cex=.8, col="red")
  }
  mtext("In red color are ID numbers of the individuals which are both: outliers and influential individuals",side=3,line=1,col = "red")
}