# round values to reasonable amount of significant figures (4 as maximum)
param_round_values <- function(covdata) {
  for (i in 1:nrow(covdata)) {
    if (covdata$is.categorical[i] == "0") {
      # covdata contains percentiles and means
      covdata[i, 2:5] <- signif(covdata[i, 2:5], digits=4)
    }
  }
  return(covdata)
}
