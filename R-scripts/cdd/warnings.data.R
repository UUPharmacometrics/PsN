warnings.data <- function(cdd.data,min.failed,cov.failed,cov.warnings,boundary) {
  
  # Default for min.failed, cov.warnings and boundary.
  if (missing(min.failed)) {
    min.failed <- FALSE
  }
  if (missing(cov.failed)) {
    cov.failed <- FALSE
  }
  if (missing(cov.warnings)) {
    cov.warnings <- FALSE
  }
  if (missing(boundary)) {
    boundary <- FALSE
  }
  
  ## create data frame for current parameter
  p1 <- cdd.data
  
  if (min.failed) 
    if (0 %in% p1$minimization_successful){
    mf  <- subset(p1, minimization_successful == 0)
    p1  <- subset(p1, minimization_successful == 1)
    }
  if (cov.failed) 
    if (0 %in% p1$covariance_step_successful){
    cs <- subset(p1, covariance_step_successful == 0)
    p1 <- subset(p1, covariance_step_successful == 1)
    }
  if (cov.warnings)
    if (1 %in% p1$covariance_step_warnings){
    cw <- subset(p1, covariance_step_warnings == 1)
    p1 <- subset(p1, covariance_step_warnings == 0)
  }
  if (boundary) 
    if (1 %in% p1$estimate_near_boundary){
    nb <- subset(p1, estimate_near_boundary == 1)
    p1 <- subset(p1, estimate_near_boundary == 0)
  }
  # check if mf, cw, nb exist. If exists, create data frame cdd.warn.
  if (exists("mf")) {
    cdd.warn <- mf
  }
  if (exists("cs")) {
    if (exists("cdd.warn")) {
      cdd.warn <- rbind(cdd.warn, cs)
    } else {
      cdd.warn <- cs
    }
  }
  if (exists("cw")) {
    if (exists("cdd.warn")) {
      cdd.warn <- rbind(cdd.warn, cw)
    } else {
      cdd.warn <- cw
    }
  }
  if (exists("nb")) {
    if (exists("cdd.warn")) {
      cdd.warn <- rbind(cdd.warn, nb)
    } else {
      cdd.warn <- nb
    }
  }
rownames(p1) <- NULL
# create list of talbe p1 and table cdd.warn if it exists
if (exists("cdd.warn")) {
  rownames(cdd.warn) <- NULL
  return(list(cdd.warn = cdd.warn, p1 = p1))  
  } else {
  return(p1)
  }
}