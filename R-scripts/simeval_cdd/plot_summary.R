plot_summary <- function(infl_outl,infl_not_outl,outl_not_infl,not_outl_not_infl) {
  plot(c(0:4),c(0:4),type="n",xlab="Outliers (Simeval)",ylab="Influential individuals (CDD)",
       xlim=c(0,4),ylim=c(0,4),
       xaxt="n",yaxt="n")
  axis(1, at=c(1,3), labels=c("NO","YES"))
  axis(2, at=c(1,3), labels=c("NO","YES"))
  abline(h = 2, v = 2)
  text(1,1, labels=length(not_outl_not_infl),cex=6)
  text(3,1, labels=length(outl_not_infl),cex=6)
  text(1,3, labels=length(infl_not_outl),cex=6)
  text(3,3, labels=length(infl_outl),cex=6)
}
