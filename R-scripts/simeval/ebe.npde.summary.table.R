summary.table.ebe.npde <- function(ebenpde_obs,eta.names) {
  # Calculate variance, mean, p-values(mean==0), p-value(var==1) for each ETA
  variance <- c() 
  mymean <- c()
  myskewness <- c()
  mykurtosis <- c()
  p_mean_not_0 <- c()
  p_var_not_1 <- c()
  indiv_amount <- c()
  p_shap.wilks <- c()
  for(i in 1:length(eta.names)){ 
    vect <- ebenpde_obs[!is.na(ebenpde_obs[,i]),i]
    if(length(vect)==0) {
      indiv_amount[i] <- 0
      variance[i] <- 0
      mymean[i] <- 0
      myskewness[i] <- 0
      mykurtosis[i] <- 0
      p_mean_not_0[i] <- 0
      p_var_not_1[i]  <- 0
      p_shap.wilks[i] <- 0
    } else {
      indiv_amount[i] <- length(vect)
      variance[i] <- var(vect)
      mymean[i] <- mean(vect)
      myskewness[i] <- skewness(vect)
      mykurtosis[i] <- kurtosis(vect)
      p_mean_not_0[i] <- wilcox.test(vect)$p.value # $p.value (it means that we look only on p-value from this test)
      p_var_not_1[i]  <- ks.test(vect,"pnorm",mean=mymean[i],sd=1)$p.value #sd = standard variatio and $p.value (it means that we look only on p-value from this test)
      if(length(vect)>=3) { # shapiro wilks test works only fif samples are form 3-5000
        p_shap.wilks[i] <- shapiro.test(vect)$p.value
      } else {
        p_shap.wilks[i] <- -1 # later replacing with NA value
      }
      
    }
  }
  
  if(any(indiv_amount != indiv_amount[1])) {
    mydataframe <- data.frame('EBE NPDE' = eta.names,
                              'Number\nof ID' = indiv_amount,
                              mean = sprintf("%.3f",mymean),
                              'p-value\n(H_0: mean==0)'=sprintf("%.3f",p_mean_not_0),
                              variance=sprintf("%.3f",variance),
                              'p-value\n(H_0: var==1)'=sprintf("%.3f",p_var_not_1),
                              skewness=sprintf("%.3f",myskewness),
                              kurtosis=sprintf("%.3f",mykurtosis),
                              'p-value\n(normality)'=sprintf("%.3f",p_shap.wilks),
                              check.names=FALSE,stringsAsFactors = F)
  } else {
    mydataframe <- data.frame('EBE NPDE' = eta.names,
                              mean = sprintf("%.3f",mymean),
                              'p-value\n(H_0: mean==0)'=sprintf("%.3f",p_mean_not_0),
                              variance=sprintf("%.3f",variance),
                              'p-value\n(H_0: var==1)'=sprintf("%.3f",p_var_not_1),
                              skewness=sprintf("%.3f",myskewness),
                              kurtosis=sprintf("%.3f",mykurtosis),
                              'p-value\n(normality)'=sprintf("%.3f",p_shap.wilks),
                              check.names=FALSE,stringsAsFactors = F)
    
  }
  
  # replace 0 values with "", because if nr of ID == 0, then there are no values to calculate means....
  if(any(names(mydataframe)=="Number\nof ID")) {
    for (i in 1:nrow(mydataframe)) {
      if(mydataframe$`Number\nof ID`[i] == 0) {
        mydataframe$mean[i] <- ""
        mydataframe$`p-value\n(H_0: mean==0)`[i] <- ""
        mydataframe$variance[i] <- ""
        mydataframe$`p-value\n(H_0: var==1)`[i] <- ""
        mydataframe$skewness[i] <- ""
        mydataframe$kurtosis[i] <- ""
        mydataframe$`p-value\n(normality)`[i] <- ""
      }
    }
  }
  # shapiro wilks test works only for sample size 3-5000 (we replace -1 value with NA)
  for (i in 1:nrow(mydataframe)) {
    if(mydataframe$`p-value\n(normality)`[i] == "-1.000") {
      mydataframe$`p-value\n(normality)`[i] <- "NA"
    }
  }

  
  return(mydataframe)
}