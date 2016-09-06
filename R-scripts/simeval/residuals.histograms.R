histograms.cwres.iwres <- function(residual.files,residual.names) {
  n.residuals <- length(residual.files)
  #npde (for each data frame plot a histogram)
  inf_list <- list()
  for(j in 1:n.residuals){  
    RESIDUAL <- read.csv(residual.files[j])
    residual_npde <- RESIDUAL$NPDE
    residual_npde <- residual_npde[!is.na(residual_npde)] # Take away NA values
    len <- length(residual_npde)
    H=hist(residual_npde,plot=FALSE)
    x=seq(-3,3,length=100)
    dx <- min(diff(H$breaks))
    dy=len*dx*dnorm(x)
    dy1=len*dx*H$density
    ylimit=max(c(max(dy1),max(dy)))
    xlimit_min=min(x,min(residual_npde))
    xlimit_max=max(x,max(residual_npde))
    hist(residual_npde,main=paste0("Histogram of ",residual.names[j]," NPDE"),
         xlab=paste0(residual.names[j]," NPDE"),ylim=c(0,ylimit),xlim=c(xlimit_min,xlimit_max))
    lines(x,dy, col="red")
    # Put information to lists(need for tests)
    inf_list[[j]] <- list(RESIDUAL=RESIDUAL,
                          residual_npde=residual_npde,
                          ylimit=ylimit,
                          xlimit_min=xlimit_min,
                          xlimit_max=xlimit_max)
    }
  return(inf_list)
}
