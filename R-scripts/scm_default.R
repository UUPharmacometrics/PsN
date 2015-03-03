library(ggplot2)

if (rplots.level > 0){
    pdf(file=pdf.filename,width=10,height=7,title=pdf.title)
}

if (rplots.level > 0){
    
    #steps.R
    log <- read.delim(scm.short.log,header=F)
    
    log$V1 <- as.character(log$V1)
    
    i <- 1
    n1 <- 0 #Number of covariate included after forward step
    n2 <- 0 #Number of covariate included after backward step
    while(i<=length(log$V1)){
      if((n1==0)&log$V1[i]=="Relations included after final step:") n1 <- i-1
      if((n1!=0)&(log$V1[i]=="Relations included after final step:")) n2 <- i-n1-3
      i=i+1
    }
      
    log <- log[1:n1,]
    log1 <- strsplit(log, " ")
    
    i=1
    r <- c()
    while(i<=length(log1)){
      j=1
      log2<- unlist(log1[i])
      while(j<=length(log2)){
        if((log2[j]!="")&(j==1)) r <- log2[j]
        if((log2[j]!="")&(j!=1)) r <- c(r, log2[j])
        j=j+1
      }
      if(i==1) out <- r
      if(i!=1) out <- rbind(out,r) 
      i=i+1
    }
    
    x <- c(0, rep(1:(n1), each=2), n1+1)
    y <- as.numeric(rep(c(out[1,3], out[,4]), each=2))
    data <- data.frame(x,y)
    
    alphaf <- as.numeric(out[1,7])
    sign <- as.numeric(c(out[1,3], out[1:(n1-1),4]))-alphaf
    
    name <- c("none", out[,1])
    
    
    
    p<-ggplot(data, aes(x, y)) + 
      geom_line() +
      geom_hline(yintercept = sign, colour="red") +
      scale_x_continuous(limits=c(0, n1+1), breaks=(c(0:n1)+0.5), 
                       labels=c(name),
                        name="Included covariate") + 
      scale_y_continuous(name="OFV") +
      ggtitle("SCM results - included covariates")
    
    print(p)
    
    #step1.R
    log <- read.delim(scm.log.file,header=F, skip=2)
    
    
    log$V1 <- as.character(log$V1)
    
    i <- 1
    n1 <- 0 #Number of covariate tested in fist step
    
    while(i==1){
      a <- unlist(strsplit(log$V1[n1+1], " "))[1]
      n1=n1+1
      if(a =="Parameter-covariate"){
        i <- 2
        n1=n1-2
      } 
    }
      
    log <- log[1:n1+1,]
    
    log1 <- strsplit(log, " ")
    
    i=1
    r <- c()
    while(i<=length(log1)){
      j=1
      log2<- unlist(log1[i])
      while(j<=length(log2)){
        if((log2[j]!="")&(j==1)) r <- log2[j]
        if((log2[j]!="")&(j!=1)) r <- c(r, log2[j])
        j=j+1
      }
      if(i==1) out <- r
      if(i!=1) out <- rbind(out,r) 
      i=i+1
    }
    
    x <- c(1:n1)
    y <- as.numeric(out[,4])
    data <- data.frame(x,y)
      
    alphaf <- as.numeric(out[1,7])
    base <- as.numeric(out[1,3])
    sign <- base - alphaf
    
    name <- c(out[,1])
    
    
    
    p <- ggplot(data, aes(x, y)) + 
      geom_point(shape="-", size=15) +
      geom_hline(yintercept = base, colour="blue") +
      geom_hline(yintercept = sign, colour="red") +
      scale_x_continuous(breaks=(c(1:n1)), 
                       labels=c(name),
                        name="Covariate") + 
      scale_y_continuous(name="OFV") +
      ggtitle("SCM - results first inclusion step")
    
    
    print(p)
}

if (rplots.level > 0){
    dev.off()
}
