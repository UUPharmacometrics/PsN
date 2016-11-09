plot.cdd <- function(cdd.data,cutoff_cook,cutoff_cov,cdd.pt,cdd.txt,legend,fail,cdd.warn) {
  #find rows of data which are influential(cutoffs)
  row_red <- c()
  row <- c()
  for (i in 1:nrow(cdd.pt)) {
    if ((cdd.pt$cook.scores[i] > cutoff_cook) || ((cdd.pt$cov.ratios[i] >= max(cutoff_cov)) || (cdd.pt$cov.ratios[i] <= min(cutoff_cov)))) {
      row <- c(row,i)
    }
  }
  for (i in 1:nrow(cdd.pt)) {
    if ((cdd.pt$cook.scores[i] > cutoff_cook) && ((cdd.pt$cov.ratios[i] >= max(cutoff_cov)) || (cdd.pt$cov.ratios[i] <= min(cutoff_cov)))) {
      row_red <- c(row_red,i)
    }
  }
  count_lines <- 1
  for (i in 1:length(fail)) {
    if (fail[i] > 0) {
      count_lines <- count_lines + 1
    }
  }  
  if (legend) {
  par(xpd=NA,mar=par()$mar + c(count_lines,0,0,0)) # change margins to add text on the bottom
  }
  #text on axis
  xlab_text <- paste0("Cook score (cutoff: ",cutoff_cook,")")
  ylab_text <- paste0("Covariance ratio (cutoffs: ",round(min(cutoff_cov),3)," and ",round(max(cutoff_cov),3),")")
  if(length(row)>0) {
    # Construct plot  
    plot (cdd.pt$cook.scores[-row], cdd.pt$cov.ratios[-row],
          type="p",
          xlab=xlab_text,
          ylab=ylab_text,
          main="Case-deletion diagnostics",
          xlim=c(0,max(cdd.data$cook.scores, na.rm=T)),
          ylim=c(0,max(cdd.data$cov.ratio, na.rm=T))
    )
  } else {
    plot (cdd.pt$cook.scores, cdd.pt$cov.ratios,
          type="p",
          xlab=xlab_text,
          ylab=ylab_text,
          main="Case-deletion diagnostics",
          xlim=c(0,max(cdd.data$cook.scores, na.rm=T)),
          ylim=c(0,max(cdd.data$cov.ratio, na.rm=T))
    )
  }
  if (length(row)>0) {
    text(cdd.pt$cook.scores[row], cdd.pt$cov.ratios[row], labels=as.character(cdd.pt$ID[row]),cex=.8, col="black")
  }
  if(length(row_red)>0) {
    text(cdd.pt$cook.scores[row_red], cdd.pt$cov.ratios[row_red], labels=as.character(cdd.pt$ID[row_red]),cex=.8, col="red")
  }
  abline(h=max(cutoff_cov), lwd=2, lty=3, col="black")
  abline(h=min(cutoff_cov), lwd=2, lty=3, col="black")
  abline(v=cutoff_cook, lwd=2, lty=3, col="black")
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