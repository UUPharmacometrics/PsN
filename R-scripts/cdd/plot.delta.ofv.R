plot.delta.ofv <- function(data_plot,row_infl,fail_ID,row_outl,row_outl_infl,cutoff_delta.ofv) {
  # Default for outlier_ID
  if (!missing(row_outl)) {
    outl_legend <- TRUE
  }
  if (!missing(row_outl) && !missing(row_outl_infl)) {
    row <- c(row_infl,row_outl,row_outl_infl)
  } else {
    row <- row_infl
  }
  if(length(row) > 0) {
    cook.score_black <- data_plot$cook.scores[-row]
    delta.ofv_black <- data_plot$cdd.delta.ofv[-row]
    ID_black <- data_plot$ID[-row]
  } else {
    cook.score_black <- data_plot$cook.scores
    delta.ofv_black <- data_plot$cdd.delta.ofv
    ID_black <- data_plot$ID
  }
  
  if(length(fail_ID) > 0) {
    par(oma=c(2,1,0,5))
  } else {
    par(oma=c(0,1,0,5))
  }
  plot (cook.score_black,delta.ofv_black,
        type="n",
        # col="gray46",
        # xaxt="n",
        ylab="OFV(cdd-i,orig)-OFV(cdd-i,est)",
        xlab="Cook scores",
        ylim=c(min(data_plot$cdd.delta.ofv, na.rm=T),max(data_plot$cdd.delta.ofv, na.rm=T)),
        xlim=c(min(data_plot$cook.scores, na.rm=T),max(data_plot$cook.scores, na.rm=T))
  )
  abline(h=cutoff_delta.ofv, lwd=2, lty=3, col="blue")
  text(cook.score_black,delta.ofv_black, labels=as.character(ID_black),cex=.8, col="gray46")
  # title(xlab = "Excluded individuals", line = 1) # move x axis label closer to the x axis
  if (!exists("outl_legend")) {
    #legend(par('usr')[2], par('usr')[4],bty='n',xpd=NA,c("Influential ID"),text.col = c("blue"))
    title(main="Influential individuals")
  } else {
    legend(par('usr')[2], par('usr')[4],bty='n',xpd=NA,c("Influential ID","Outlier ID", "\nInfluential\n + \noutlier ID"),text.col = c("blue", "purple", "green2"))
    title(main="Outliers and influential individuals")
  }
  if (length(row_infl) > 0) {
    text(data_plot$cook.scores[row_infl],data_plot$cdd.delta.ofv[row_infl], labels=as.character(data_plot$ID[row_infl]),cex=.8, col="blue")
  }
  if (!missing("row_outl")) {
    if (length(row_outl) > 0) {
      text(data_plot$cook.scores[row_outl],data_plot$cdd.delta.ofv[row_outl], labels=as.character(data_plot$ID[row_outl]),cex=.8, col="purple")
    }
  }
  if (!missing("row_outl_infl")) {
    if (length(row_outl_infl) > 0) {
      text(data_plot$cook.scores[row_outl_infl],data_plot$cdd.delta.ofv[row_outl_infl], labels=as.character(data_plot$ID[row_outl_infl]),cex=.8, col="green2")
    }
  }
  # add text if Nonmem failed in estimation
  if (length(fail_ID) > 0) {
    fail_ID_text <- c()
    for (i in 1:length(fail_ID)) {
      if(i == 1) {
        fail_ID_text <- fail_ID[i]
      } else {
        fail_ID_text <- paste0(fail_ID_text,", ",fail_ID[i])
      }
    }
    mtext(paste0("Nonmem failed in estimation for individuals with ID nr: ",fail_ID_text),side=1, line=3, adj=0,col="red")
  }
}