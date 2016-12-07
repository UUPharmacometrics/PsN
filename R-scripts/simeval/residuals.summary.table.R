summary.table <- function(residual.files,residual.names) {
  # number of data frames
  n.residuals <- length(residual.files)
  variance <- c(1:n.residuals)
  mymean <- c(1:n.residuals)
  p_mean_not_0 <- c(1:n.residuals)
  p_var_not_1 <- c(1:n.residuals)
  myskewness <- c(1:n.residuals)
  mykurtosis <- c(1:n.residuals)
  p_shap.wilks <- c()
  for(j in 1:n.residuals){  
    RESIDUAL <- read.csv(residual.files[j]) # Opec csv data frame
    residual_npde <- RESIDUAL$NPDE
    residual_npde <- residual_npde[!is.na(residual_npde)] # Take away NA values
    # Calsulation
    variance[j] <- var(residual_npde)
    mymean[j] <- mean(residual_npde)
    p_mean_not_0[j] <- wilcox.test(residual_npde)$p.value
    p_var_not_1[j]  <- ks.test(residual_npde,pnorm,mean=mymean[j],sd=1)$p.value
    myskewness[j] <- skewness(residual_npde)
    mykurtosis[j] <- kurtosis(residual_npde)
    if(length(residual_npde)>=3) { # shapiro wilks test works only fif samples are form 3-5000
      p_shap.wilks[j] <- shapiro.test(residual_npde)$p.value
    } else {
      p_shap.wilks[j] <- NA # later replacing with NA value
    }
  }
  # Create summary table
  mydataframe <- data.frame('NPDE' = residual.names, mean = round(mymean,digits=3),'p-value\n(H_0: mean==0)'=round(p_mean_not_0,digits=3),
                            variance=round(variance,digits=3),'p-value\n(H_0: var==1)'=round(p_var_not_1,digits=3),
                            skewness=round(myskewness,3),kurtosis=round(mykurtosis,3),'p-value\n(normality)'=round(p_shap.wilks,3),
                            check.names=FALSE)
  return(mydataframe)
}