plot.cov.cook.par <- function(cook.par.data,cov.par.data,parameters) {
  # plot cook score per each parameter/covariance per each parameter
  for (i in 1:length(parameters)) {
    plot (cov.par.data[,i],cook.par.data[,i],
          type="p",
          xlab=paste("cov.",parameters[i]),
          ylab=paste("cook.",parameters[i]),
          xlim=c(min(cov.par.data[,i], na.rm=T),max(cov.par.data[,i], na.rm=T)),
          ylim=c(min(cook.par.data[,i], na.rm=T),max(cook.par.data[,i], na.rm=T))
    )
    title(paste("PARAMETER ",parameters[i]))
  }
  # output
  nr.parameters <- length(parameters)
  return(nr.parameters)
}