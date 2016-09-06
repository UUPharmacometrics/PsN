kld_i_ofv <- function(all.iofv.file,n.subjects,samples,n) {
  # n is the number of equally spaced points at which the density is to be estimated
  # default for n
  if (missing(n)) {
    n <- 100
  }
  
  # KLD iOFV
  all.iOFV_sim <- read.csv(all.iofv.file)
  iOFV_obs <- all.iOFV_sim$ORIGINAL
  
  # Find minimum and maximum simulated values for each sample
  iOFV_min <- array(0,c(samples,1))
  iOFV_max <- array(0,c(samples,1))
  for (i in 1:samples){      
    iOFV_sim <- all.iOFV_sim[paste('sample.',i,sep='')]
    iOFV_sim <- iOFV_sim[!is.na(iOFV_sim)]
    iOFV_min[i] <- min(iOFV_sim)
    iOFV_max[i] <- max(iOFV_sim)
  }
  
  # Find y values of Kernel Density Estimation for each sample
  final_grid <- c(max(iOFV_min),min(iOFV_max)) 
  iOFV_kernel <- array(0,c(n,samples))
  for (i in 1:samples){      
    iOFV_sim <- all.iOFV_sim[paste('sample.',i,sep='')]
    iOFV_sim <- iOFV_sim[!is.na(iOFV_sim)]
    iOFV_tmp <- density(as.vector(iOFV_sim),kernel=c("gaussian"),from=final_grid[1],to=final_grid[2],n=n)
    iOFV_kernel[,i] <- iOFV_tmp$y
  }
  
  # Find y values of Kernel Density Estimation for obsorved data
  iOFV_tmp <- density(as.vector(iOFV_obs),kernel=c("gaussian"),from=final_grid[1],to=final_grid[2],n=n)
  iOFV_kernel_obs <- iOFV_tmp$y
  
  # Find mean
  iOFV_kernel_average <- array(0,c(n,1))
  for (i in 1:n){      
    iOFV_kernel_average[i] <- mean(iOFV_kernel[i,])
  }
  
  # Use function KLD
  # KLD for simulated data (sum.KLD.px.py)
  KLD_sim <- array(0,c(samples,1))
  for (i in 1:samples){     
    KLD_tmp <- KLD(iOFV_kernel_average,iOFV_kernel[,i],base=2)
    if (class(KLD_tmp) == "character") { #in situation if function KLD computes error
      return("ERROR: iOFV_kernel_average and iOFV_kernel[,i] must have the same length.")
    } else {
      KLD_sim[i] <- KLD_tmp$sum.KLD.px.py
    }
  }
  
  # KLD for obsorved data (sum.KLD.px.py)
  KLD_tmp <- KLD(iOFV_kernel_average,iOFV_kernel_obs,base=2)
  if (class(KLD_tmp) == "character") { # in situation if function KLD computes error
    return("ERROR: iOFV_kernel_average and iOFV_kernel_obs must have the same length.")
  } else {
    KLD_obs <- KLD_tmp$sum.KLD.px.py
  }
  
  # Sort KLD simulated data
  index_KLD <- sort(KLD_sim[,1],index.return=TRUE)
  KLD_sim_sort <- KLD_sim[index_KLD$ix]
  # set limit for x axis values
  newxlim <- c(KLD_sim_sort[1],KLD_sim_sort[samples])
  if(KLD_obs > KLD_sim_sort[samples]){
    newxlim <- c(KLD_sim_sort[1],KLD_obs)}
  if(KLD_obs < KLD_sim_sort[1]){
    newxlim <- c(pOFV_obs,KLD_sim_sort[samples])}
  
out <- list(all.iOFV_sim=all.iOFV_sim,
            iOFV_obs=iOFV_obs,
            iOFV_min=iOFV_min,
            iOFV_max=iOFV_max,
            final_grid=final_grid,
            iOFV_kernel=iOFV_kernel,
            iOFV_kernel_obs=iOFV_kernel_obs,
            iOFV_kernel_average=iOFV_kernel_average,
            KLD_sim=KLD_sim,
            KLD_obs=KLD_obs,
            newxlim=newxlim)
return(out) # needed for testing  
}
