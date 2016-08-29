plot.ofv <- function(cdd.data.all) {
  cdd.data <- cdd.data.all[-1,]
  plot ((cdd.data$ofv+cdd.data$cdd.delta.ofv), cdd.data$ofv,
        type="p",
        xlab="OFV(cdd-i,orig)",
        ylab="OFV(cdd-i,est)",
        xlim=c(min(cdd.data$ofv+cdd.data$cdd.delta.ofv, na.rm=T),max(cdd.data$ofv+cdd.data$cdd.delta.ofv, na.rm=T)),
        ylim=c(min(cdd.data$ofv, na.rm=T),max(cdd.data$ofv, na.rm=T))
  )
  #output
  out <- list(ofv.orig=cdd.data$ofv+cdd.data$cdd.delta.ofv,
              ofv.est = cdd.data$ofv)
  return(out)
}