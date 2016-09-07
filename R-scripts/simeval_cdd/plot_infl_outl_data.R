plot_infl_outl_data <- function(outl_data,infl_data,ID,row_simeval,row_cdd) {
  par(oma=c(0,1,0,1))
  plot (outl_data$iofv_res[-row_simeval],infl_data$delta.ofv[-row_cdd],
        type="p",
        ylab="CDD",
        xlab="Simeval",
        ylim=c(min(infl_data$delta.ofv, na.rm=T),max(infl_data$delta.ofv, na.rm=T)),
        xlim=c(min(outl_data$iofv_res,na.rm=T),max(outl_data$iofv_res,na.rm=T))
  )
  title("Cdd influential individuals and simeval outliers")
  if (length(row_cdd) > 0) {
    text(outl_data$iofv_res[row_simeval],infl_data$delta.ofv[row_cdd], labels=ID,cex=.8, col="red")
  }
  legend("top",bty='n',xpd=NA,c("ID numbers of the individuals which are both: outliers and inluencial individuals"),text.col = c("red"))
}