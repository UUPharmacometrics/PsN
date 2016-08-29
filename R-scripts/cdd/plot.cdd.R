plot.cdd <- function(cdd.data,cdd.pt,cdd.txt,legend,fail,cdd.warn) {

  count_lines <- 1
  for (i in 1:length(fail)) {
    if (fail[i] > 0) {
      count_lines <- count_lines + 1
    }
  }  

if (legend) {
 par(xpd=NA,mar=par()$mar + c(count_lines,0,0,0)) # change margins to add text on the bottom
}
# Construct plot  
plot (cdd.pt$cook.scores, cdd.pt$cov.ratios,
      type="p",
      xlab="Cook score",
      ylab="Covariance ratio",
      main="Case-deletion diagnostics",
      xlim=c(0,max(cdd.data$cook.scores, na.rm=T)),
      ylim=c(0,max(cdd.data$cov.ratio, na.rm=T))
)
# plot id numbers on the plot
if (nrow(cdd.txt) != 0) {
  text(cdd.txt$cook.scores, cdd.txt$cov.ratios, labels=as.character(cdd.txt$ID),cex=.8, col=2)
}
if (!missing("cdd.warn"))  {
  if (nrow(cdd.warn) != 0) {
    text(cdd.warn$cook.scores, cdd.warn$cov.ratios, labels=as.character(cdd.warn$ID),cex=.8, col=4)
  }
}
# add text (if needed set legend=TRUE)
if (legend) {
  name <- c("Minimization failed: ","Covariance step failed: ",
            "Covariance step warnings: ","Estimate near boundary: ")
  nr <- nrow(cdd.data)
  l <- 4
  for (i in 1:length(fail)) {
    if (fail[i] > 0) {
      l <- l + 1
      mtext(paste0(name[i],fail[i]," of ",nr),side=1, line=l, adj=1)
    }
  }
}  

}