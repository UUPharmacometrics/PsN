failed.values <- function(cdd.data) {
  # Count not successful values
  ms_nr <- length(which(cdd.data$minimization_successful == 0))
  cs_nr <- length(which(cdd.data$covariance_step_successful == 0))
  cw_nr <- length(which(cdd.data$covariance_step_warnings == 1))
  eb_nr <- length(which(cdd.data$estimate_near_boundary == 1))
  fail <- c(ms_nr,cs_nr,cw_nr,eb_nr)
  
  return(fail)
}