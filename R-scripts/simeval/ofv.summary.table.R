summary.table.ofv <- function(iofv.file) {
  # Calculate variance, mean, p-values(mean==0), p-value(var==1) for ofv NPDE
  iOFV <- read.csv(iofv.file) #input data
  iOFV_npde <- iOFV$NPDE
  iOFV_npde <- iOFV_npde[!is.na(iOFV_npde)] # delete rows with NA values
    
    variance <- sprintf("%.3f",var(iOFV_npde))
    mymean <- sprintf("%.3f",mean(iOFV_npde))
    myskewness <- sprintf("%.3f",skewness(iOFV_npde))
    mykurtosis <- sprintf("%.3f",kurtosis(iOFV_npde))
    p_mean_not_0 <- wilcox.test(iOFV_npde)$p.value # $p.value (it means that we look only on p-value from this test)
    p_mean_not_0 <- sprintf("%.3f",p_mean_not_0)
    p_var_not_1  <- ks.test(iOFV_npde,"pnorm",mean=mean(iOFV_npde),sd=1)$p.value #sd = standard variatio and $p.value (it means that we look only on p-value from this test)
    p_var_not_1 <- sprintf("%.3f",p_var_not_1)
    if(length(iOFV_npde)>=3) { # shapiro wilks test works only fif samples are form 3-5000
      p_shap.wilks <- shapiro.test(iOFV_npde)$p.value
      p_shap.wilks <- sprintf("%.3f",p_shap.wilks)
    } else {
      p_shap.wilks <- "NA"
    }
      
    mydataframe <- data.frame('NPDE' = "iOFV",
                              mean = mymean,
                              'p-value\n(H_0: mean==0)'=p_mean_not_0,
                              variance=variance,
                              'p-value\n(H_0: var==1)'=p_var_not_1,
                              skewness=myskewness,
                              kurtosis=mykurtosis,
                              'p-value\n(normality)'=p_shap.wilks,
                              check.names=FALSE,stringsAsFactors = F)
    
  return(mydataframe)
}