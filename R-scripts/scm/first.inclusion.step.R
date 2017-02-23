first_inclusion_step <- function(scm.log.file) {
  log_input <- read.delim(scm.log.file,header=F)
  log <- log_input
  
  log$V1 <- as.character(log$V1)
  
  i <- 0 #rows
  start <- 0
  end <- 0
  n1 <- 0 #Number of covariate tested in first step
  while(end==0) {
    i <- i+1
    text_part <- unlist(strsplit(log$V1[i], " "))[1]
    if((start==1) && ((text_part=="Parameter-covariate") || (text_part=="--------------------"))) {
      end <- 1
    }
    if(start==1 && end==0) {
      n1 <- n1+1
    }
    if((text_part=="MODEL")) {
      previous_first <- unlist(strsplit(log$V1[i-1], " "))[1] #an extra check
      previous_second <- unlist(strsplit(log$V1[i-1], " "))[2]
      if((previous_first=="Model") && (previous_second=="directory")) {
        start <- 1
      }
    }
  }
  
  log <- log[(i-n1):(i-1),]
  
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
  
  if(class(out)!="matrix") {
    out <- matrix(out,1)
  }
  rownames(out) <- NULL
  
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