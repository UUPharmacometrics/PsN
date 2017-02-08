plot.cdd <- function(cov.cook.data,cutoff_cook,cutoff_cov,ID_failed_cov,legend,fail,cdd.warn) {

  #find rows of data which are influential(cutoffs)
  row_red <- c()
  row <- c()
  for (i in 1:nrow(cov.cook.data)) {
    if ((cov.cook.data$cook.scores[i] > cutoff_cook) || ((cov.cook.data$cov.ratios[i] >= max(cutoff_cov)) || (cov.cook.data$cov.ratios[i] <= min(cutoff_cov)))) {
      row <- c(row,i)
    }
  }
  for (i in 1:nrow(cov.cook.data)) {
    if ((cov.cook.data$cook.scores[i] > cutoff_cook) && ((cov.cook.data$cov.ratios[i] >= max(cutoff_cov)) || (cov.cook.data$cov.ratios[i] <= min(cutoff_cov)))) {
      row_red <- c(row_red,i)
    }
  }
  count_lines <- 1
  for (i in 1:length(fail)) {
    if (fail[i] > 0) {
      count_lines <- count_lines + 1
    }
  } 
  #add text if cov=0 or/and legend is true
  if(length(ID_failed_cov)>0) {
    if (legend) {
      text_lines <- count_lines + 2
      par(oma=c(text_lines,0,0,0))
    } else {
      par(oma=c(3,0,0,0))
    }
  } else {
    if (legend) {
      par(oma=c(count_lines,0,0,0))
    }
  }
  #text on axis
  xlab_text <- paste0("Cook score (cutoff: ",cutoff_cook,")")
  ylab_text <- paste0("Covariance ratio (cutoffs: ",round(min(cutoff_cov),3)," and ",round(max(cutoff_cov),3),")")
  if(length(row)>0) {
    # Construct plot  
    plot (cov.cook.data$cook.scores[-row], cov.cook.data$cov.ratios[-row],
          type="p",
          xlab=xlab_text,
          ylab=ylab_text,
          main="Case-deletion diagnostics",
          xlim=c(0,max(cov.cook.data$cook.scores, na.rm=T)),
          ylim=c(0,max(cov.cook.data$cov.ratio, na.rm=T))
    )
  } else {
    plot (cov.cook.data$cook.scores, cov.cook.data$cov.ratios,
          type="p",
          xlab=xlab_text,
          ylab=ylab_text,
          main="Case-deletion diagnostics",
          xlim=c(0,max(cov.cook.data$cook.scores, na.rm=T)),
          ylim=c(0,max(cov.cook.data$cov.ratio, na.rm=T))
    )
  }
  if (length(row)>0) {
    text(cov.cook.data$cook.scores[row], cov.cook.data$cov.ratios[row], labels=as.character(cov.cook.data$ID[row]),cex=.8, col="black")
  }
  if(length(row_red)>0) {
    text(cov.cook.data$cook.scores[row_red], cov.cook.data$cov.ratios[row_red], labels=as.character(cov.cook.data$ID[row_red]),cex=.8, col="red")
  }
  abline(h=max(cutoff_cov), lwd=2, lty=3, col="black")
  abline(h=min(cutoff_cov), lwd=2, lty=3, col="black")
  abline(v=cutoff_cook, lwd=2, lty=3, col="black")
  # plot id numbers on the plot in blue color if any of min.failed,cov.failed,cov.warnings,boundary were set to TRUE
  if (!missing("cdd.warn"))  {
    if (nrow(cdd.warn) != 0) {
      text(cdd.warn$cook.scores, cdd.warn$cov.ratios, labels=as.character(cdd.warn$ID),cex=.8, col="blue")
    }
  }
  
  #add text about failed covariance steps for exluded ID numbers
  l <- 5
  if(length(ID_failed_cov) > 0) {
    mtext(paste("Excluded ID numbers:",paste(ID_failed_cov,collapse = ", ")),side=1, line=l, adj=0)
    l <- l + 1
  }
  # add text (if needed set legend=TRUE)
  if (legend) {
    name <- c("Minimization failed: ","Covariance step failed: ",
              "Covariance step warnings: ","Estimate near boundary: ")
    nr <- nrow(cov.cook.data) + length(ID_failed_cov)
    for (i in 1:length(fail)) {
      if (fail[i] > 0) {
        l <- l + 1
        mtext(paste0(name[i],fail[i]," of ",nr),side=1, line=l, adj=1)
      }
    }
  }  

}