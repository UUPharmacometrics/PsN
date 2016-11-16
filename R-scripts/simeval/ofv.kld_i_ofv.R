kld_i_ofv <- function(all.iofv.file,n.subjects,samples,n) {
  ###########################################################################
  # Kullback-Leibler Divergence (KLD)                                       #
  #                                                                         #
  # The purpose of the KLD function is to calculate the Kullback-Leibler    #
  # divergences between two probability distributions, p(x) and p(y).       #
  ###########################################################################
  #put KLD function in separate file. Assume this function is tested elsewhere
  KLD <- function(px, py, base=exp(1)){
    ### Initial Checks
    if(!is.vector(px)) px <- as.vector(px)
    if(!is.vector(py)) py <- as.vector(py)
    n1 <- length(px)
    n2 <- length(py)
    if(!identical(n1, n2)) {
      return("ERROR: px and py must have the same length.")
    }
    if(any(!is.finite(px)) || any(!is.finite(py))) {
      return("ERROR: px and py must have finite values.")
    }
    if(any(px <= 0)) px <- exp(px)
    if(any(py <= 0)) py <- exp(py)
    px[which(px < .Machine$double.xmin)] <- .Machine$double.xmin
    py[which(py < .Machine$double.xmin)] <- .Machine$double.xmin
    ### Normalize
    px <- px / sum(px)
    py <- py / sum(py)
    ### Kullback-Leibler Calculations
    KLD.px.py <- px * (log(px, base=base)-log(py, base=base))
    KLD.py.px <- py * (log(py, base=base)-log(px, base=base))
    sum.KLD.px.py <- sum(KLD.px.py)
    sum.KLD.py.px <- sum(KLD.py.px)
    mean.KLD <- (KLD.px.py + KLD.py.px) / 2
    mean.sum.KLD <- (sum.KLD.px.py + sum.KLD.py.px) / 2
    ### Output
    out <- list(base=base,
                n1=n1,
                n2=n2,
                px.normalized=px,
                py.normalized=py,
                KLD.px.py=KLD.px.py, #KLD[i](p(x[i]) || p(y[i]))
                KLD.py.px=KLD.py.px, #KLD[i](p(y[i]) || p(x[i]))
                mean.KLD=mean.KLD,
                sum.KLD.px.py=sum.KLD.px.py, #KLD(p(x) || p(y))
                sum.KLD.py.px=sum.KLD.py.px, #KLD(p(y) || p(x))
                mean.sum.KLD=mean.sum.KLD,
                intrinsic.discrepancy=min(sum.KLD.px.py, sum.KLD.py.px))
    return(out)
  }
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
    newxlim <- c(KLD_obs,KLD_sim_sort[samples])}
  
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
