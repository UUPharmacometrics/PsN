first_inclusion_step <- function(scm.log.file) {
  log_input <- read.delim(scm.log.file,header=F, skip=2)
  log <- log_input
  
  log$V1 <- as.character(log$V1)
  
  i <- 1
  n1 <- 0 #Number of covariate tested in fist step
  
  while(i==1){
    a <- unlist(strsplit(log$V1[n1+1], " "))[1]
    n1 <- n1+1
    if(a =="Parameter-covariate"){
      i <- 2
      n1 <- n1-2
    } 
  }
  
  log <- log[1:n1+1,]
  
  log1 <- strsplit(log, " ")
  
  i <- 1
  r <- c()
  while(i <= length(log1)){
    j <- 1
    log2 <- unlist(log1[i])
    while(j <= length(log2)){
      if((log2[j]!="") & (j==1)) r <- log2[j]
      if((log2[j]!="") & (j!=1)) r <- c(r, log2[j])
      j <- j+1
    }
    if(i==1) out <- r
    if(i!=1) out <- rbind(out,r) 
    i <- i+1
  }
  
  x <- c(1:n1)
  y <- as.numeric(out[,4])
  data <- data.frame(x,y)
  
  alphaf <- as.numeric(out[1,7])
  base <- as.numeric(out[1,3])
  sign <- base - alphaf
  
  name <- c(out[,1])
  
  list_out <- list(log_input=log_input,
                   n1=n1,
                   log=log,
                   log1=log1,
                   out=out,
                   data=data,
                   alphaf=alphaf,
                   base=base,
                   sign=sign,
                   name=name)
  
  return(list_out)
}