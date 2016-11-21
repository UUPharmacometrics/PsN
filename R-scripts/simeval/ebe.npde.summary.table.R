summary.table.ebe.npde <- function(ebenpde_obs,iiv.eta.names) {
  # Calculate variance, mean, p-values(mean==0), p-value(var==1) for each ETA
  variance <- c() 
  mymean <- c()
  p_mean_not_0 <- c()
  p_var_not_1 <- c()
  indiv_amount <- c()
  for(i in 1:length(iiv.eta.names)){ 
    vect <- ebenpde_obs[!is.na(ebenpde_obs[,i]),i]
    if(length(vect)==0) {
      indiv_amount[i] <- 0
      variance[i] <- 0
      mymean[i] <- 0
      p_mean_not_0[i] <- 0
      p_var_not_1[i]  <- 0
    } else {
      indiv_amount[i] <- length(vect)
      variance[i] <- var(vect)
      mymean[i] <- mean(vect)
      p_mean_not_0[i] <- wilcox.test(vect)$p.value # $p.value (it means that we look only on p-value from this test)
      p_var_not_1[i]  <- ks.test(vect,"pnorm",mean=mymean[i],sd=1)$p.value #sd = standard variatio and $p.value (it means that we look only on p-value from this test)
    }
  }
  
  #ETAs are not sorted if any iov eta, use iiv.eta.names from preamble
  if(any(indiv_amount != indiv_amount[1])) {
    mydataframe <- data.frame('EBE NPDE' = iiv.eta.names,
                              'Number of ID' = indiv_amount,
                              mean = sprintf("%.5f",mymean),
                              'p-value (H_0: mean==0)'=sprintf("%.3f",p_mean_not_0),
                              variance=sprintf("%.5f",variance),
                              'p-value (H_0: var==1)'=sprintf("%.3f",p_var_not_1),
                              check.names=FALSE,stringsAsFactors = F)
  } else {
    mydataframe <- data.frame('EBE NPDE' = iiv.eta.names,
                              mean = sprintf("%.5f",mymean),
                              'p-value (H_0: mean==0)'=sprintf("%.3f",p_mean_not_0),
                              variance=sprintf("%.5f",variance),
                              'p-value (H_0: var==1)'=sprintf("%.3f",p_var_not_1),
                              check.names=FALSE,stringsAsFactors = F)
  }
  
  # replace 0 values with "", because if nr of ID == 0, then there are no values to calculate means....
  if(ncol(mydataframe)==6) {
    for (i in 1:nrow(mydataframe)) {
      if(mydataframe$`Number of ID`[i] == 0) {
        mydataframe$mean[i] <- ""
        mydataframe$`p-value (H_0: mean==0)`[i] <- ""
        mydataframe$variance[i] <- ""
        mydataframe$`p-value (H_0: var==1)`[i] <- ""
      }
    }
  }
  
  
  return(mydataframe)
}




# summary.table.ebe.npde <- function(ebenpde_obs,iiv.eta.names) {
#   # Calculate variance, mean, p-values(mean==0), p-value(var==1) for each ETA
#   variance <- c() 
#   mymean <- c()
#   p_mean_not_0 <- c()
#   p_var_not_1 <- c()
#   #eta_amount <- c()
#   for(i in 1:length(iiv.eta.names)){ 
#     vect <- ebenpde_obs[!is.na(ebenpde_obs[,i]),i]
#     #eta_amount[i] <- length(vect)
#     variance[i] <- var(vect)
#     mymean[i] <- mean(vect)
#     p_mean_not_0[i] <- wilcox.test(vect)$p.value # $p.value (it means that we look only on p-value from this test)
#     p_var_not_1[i]  <- ks.test(vect,"pnorm",mean=mymean[i],sd=1)$p.value #sd = standard variatio and $p.value (it means that we look only on p-value from this test)
#   }
#   
#   #ETAs are not sorted if any iov eta, use iiv.eta.names from preamble
#   mydataframe <- data.frame('EBE NPDE' = iiv.eta.names,
#                             #'Amount of ETAs' = eta_amount,
#                             mean = sprintf("%.5f",mymean),
#                             'p-value (H_0: mean==0)'=sprintf("%.3f",p_mean_not_0),
#                             variance=sprintf("%.5f",variance),
#                             'p-value (H_0: var==1)'=sprintf("%.3f",p_var_not_1),
#                             check.names=FALSE)
#   
#   return(mydataframe)
# }