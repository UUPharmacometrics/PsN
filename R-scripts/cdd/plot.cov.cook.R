plot.cov.cook <- function(cdd.data.all) {
  cdd.data <- cdd.data.all[-1,]
  rownames(cdd.data) <- NULL
  plot (cdd.data$cov.ratios, cdd.data$cook.scores,
        type="p",
        xlab="Covariance ratio",
        ylab="Cook score",
        xlim=c(0,max(cdd.data$cov.ratio, na.rm=T)),
        ylim=c(0,max(cdd.data$cook.scores, na.rm=T))
  )
  #output
  return(cdd.data)
}
