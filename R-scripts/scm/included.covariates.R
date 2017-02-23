included_covariates <- function(scm.short.log) {
  log_input <- read.delim(scm.short.log,header=F)
  log <- log_input
  
  log$V1 <- as.character(log$V1)
  
  i <- 1
  n1 <- 0 #Number of covariate included after forward step
  while(i<=length(log$V1)){
    if((n1==0)&(log$V1[i]=="Relations included after final step:")) n1 <- i-1
    i <- i+1
  }
  if (n1==0 && nrow(log)>0) { # If text "Relations included after final step" not found, but there are some lines in the file
    n1 <- nrow(log)
  }

  log <- log[1:n1,]
  log1 <- strsplit(log, " ")
  
  i <- 1
  r <- c()
  while(i<=length(log1)){
    j <- 1
    log2 <- unlist(log1[i])
    while(j<=length(log2)){
      if((log2[j]!="")&(j==1)) r <- log2[j]
      if((log2[j]!="")&(j!=1)) r <- c(r, log2[j])
      j <- j+1
    }
    if(i==1) out <- r
    if(i!=1) out <- rbind(out,r) 
    i <- i+1
  }
  
  # if n1=1
  if(class(out)!="matrix") {
    out <- matrix(out,1)
  }
  rownames(out) <- NULL
  
  alphaf <- as.numeric(out[1,7])
  if(n1==1) {
    sign <- as.numeric(out[1,3])-alphaf
  } else {
    sign <- as.numeric(c(out[1,3], out[1:(n1-1),4]))-alphaf # why n1-1 ???
  }
  
  x <- c(0, rep(1:(n1), each=2), n1+1)
  y <- as.numeric(rep(c(out[1,3], out[,4]), each=2))
  data <- data.frame(x,y)
  name <- c("none", out[,1])
  
  out_list <- list(log_input=log_input,
                   n1=n1,
                   log=log,
                   log1=log1,
                   out=out,
                   alphaf=alphaf,
                   sign=sign,
                   data=data,
                   name=name)
  
  return(out_list)
}