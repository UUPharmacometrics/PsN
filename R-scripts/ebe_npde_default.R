
###########################################################################
# Kullback-Leibler Divergence (KLD)                                       #
#                                                                         #
# The purpose of the KLD function is to calculate the Kullback-Leibler    #
# divergences between two probability distributions, p(x) and p(y).       #
###########################################################################

KLD <- function(px, py, base=exp(1))
     {
     ### Initial Checks
     if(!is.vector(px)) px <- as.vector(px)
     if(!is.vector(py)) py <- as.vector(py)
     n1 <- length(px)
     n2 <- length(py)
     if(!identical(n1, n2)) stop("px and py must have the same length.")
     if(any(!is.finite(px)) || any(!is.finite(py))) 
          stop("px and py must have finite values.")
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
     out <- list(KLD.px.py=KLD.px.py, #KLD[i](p(x[i]) || p(y[i]))
          KLD.py.px=KLD.py.px, #KLD[i](p(y[i]) || p(x[i]))
          mean.KLD=mean.KLD,
          sum.KLD.px.py=sum.KLD.px.py, #KLD(p(x) || p(y))
          sum.KLD.py.px=sum.KLD.py.px, #KLD(p(y) || p(x))
          mean.sum.KLD=mean.sum.KLD,
          intrinsic.discrepancy=min(sum.KLD.px.py, sum.KLD.py.px))
     return(out)
     }

#End

pdf(file=pdf.filename,width=10,height=7,title=pdf.title)

#EBE-NPDE correlation graph
library(PerformanceAnalytics)
ebenpde_tmp <- read.csv(ebe.npde.file)
ebenpde_obs <- ebenpde_tmp[,seq(3,n.eta+2)]
chart.Correlation(ebenpde_obs, histogram = TRUE, method = c("spearman"))

#outlier graph
#TODO

dev.off()

#new pdf for OFV
pdf.filename <- paste0('PsN_OFV_plots.pdf')
pdf.title <- paste0('ofv diagnostic plots run ',xpose.runno)
pdf(file=pdf.filename,width=10,height=7,title=pdf.title)

# pOFV PPC
rawres <- read.csv(raw.results.file)
rawres <- rawres[!is.na(rawres$ofv),]
len <- length(rawres$ofv)
pOFV_obs <- rawres$ofv[1]
pOFV_sim <- rawres$ofv[2:len]
sort_pOFV_sim<-sort(pOFV_sim[1:len-1])
newxlim=c(sort_pOFV_sim[1],sort_pOFV_sim[len-1])
if(pOFV_obs>sort_pOFV_sim[len-1]){
  newxlim=c(sort_pOFV_sim[1],pOFV_obs)}
if(pOFV_obs<sort_pOFV_sim[1]){
  newxlim=c(pOFV_obs,sort_pOFV_sim[len-1])}
hist(pOFV_sim,xlim=newxlim,main=paste('pOFV PPC ',model.filename),xlab="pOFV",freq=TRUE)
abline(v=pOFV_obs,col="red",lwd=2, lty=1)
abline(v=median(pOFV_sim),lwd=2, lty=4, col="green")
abline(v=quantile(pOFV_sim,c(0.025, 0.975), na.rm=T)[1],lwd=2, lty=3, col="green")
abline(v=quantile(pOFV_sim, c(0.025, 0.975), na.rm=T)[2],lwd=2, lty=3, col="green")
leg.txt <- c("pOFV","median pOFVsim","5th and 95th pOFVsim")
legend("topright", col=c('red', 'green', 'green','green'), leg.txt, lty=c(1,4,3,3),box.lwd = 0,box.col = "white",bg = "white", lwd=2, cex=1)


# iOFV NPDE
iOFV <- read.csv(iofv.file)
iOFV_npde <- iOFV$NPDE
iOFV_npde <- iOFV_npde[!is.na(iOFV_npde)]
len <- length(iOFV_npde)
H=hist(iOFV_npde,plot=FALSE)
x=seq(-3,3,length=100)
dx <- min(diff(H$breaks))
dy=len*dx*dnorm(x)
dy1=len*dx*H$density
max_npde<-max(H$density)
max_npde_index<-which(H$density == max_npde)
ylimit=max(c(max(dy1),max(dy)))
xlimit_min=min(x,min(iOFV_npde))
xlimit_max=max(x,max(iOFV_npde))
hist(iOFV_npde,xlab="iOFV NPDE",ylim=c(0,ylimit),xlim=c(xlimit_min,xlimit_max))
lines(x,dy, col="red")


# iOFV PPC
all.iOFV_sim <- read.csv(all.iofv.file)
iOFV_obs <- all.iOFV_sim$ORIGINAL
for (i in 1:n.subjects) {  
  iOFV_sim <- all.iOFV_sim[i,3:(samples+2)]
  iOFV_sim <- iOFV_sim[!is.na(iOFV_sim)]
  len <- length(iOFV_sim)
  sort_iOFV_sim<-sort(iOFV_sim)
  newxlim=c(sort_iOFV_sim[1],sort_iOFV_sim[len])
  if(iOFV_obs[i]>sort_iOFV_sim[len]){
    newxlim=c(sort_iOFV_sim[1],iOFV_obs[i])}
  if(iOFV_obs[i]<sort_iOFV_sim[1]){
    newxlim=c(iOFV_obs[i],sort_iOFV_sim[len])}  
  if  (iOFV_obs[i]>sort_iOFV_sim[len])  #outlier criteria
    {     
    hist(sort_iOFV_sim,xlim=newxlim,axes=TRUE,main=paste('iOFV PPC ',model.filename,'ID =', all.iOFV_sim$ID[i]),xlab="iOFV",freq=TRUE)
    abline(v=iOFV_obs[i], lwd= 2, lty=1, col="red") 
    abline(v=median(iOFV_sim[1:len]), lwd=2, lty=4, col="green") 
    abline(v=quantile(iOFV_sim[1:len], c(0.025, 0.975), na.rm=T)[1], lwd=2, lty=3, col="green") 
    abline(v=quantile(iOFV_sim[1:len], c(0.025, 0.975), na.rm=T)[2], lwd=2, lty=3, col="green") 
    leg.txt <- c("iOFV","median iOFVsim","5th and 95th iOFVsim")
    legend("topright", col=c('red', 'green', 'green','green'), leg.txt, lty=c(1,4,3,3),box.lwd = 0,box.col = "white",bg = "white", lwd=2, cex=1)
    }
}


# iOFV RES
all.iOFV_sim <- read.csv(all.iofv.file)
iOFV_obs <- all.iOFV_sim$ORIGINAL
iOFV_res<- array(0,c(samples,n.subjects))
iOFV_res_median <- array(0,c(n.subjects,1))
iOFV_res_ord <- array(0,c(samples,n.subjects))
for (i in 1:n.subjects) {
      iOFV_sim <- all.iOFV_sim[i,3:(samples+2)]
      iOFV_sim <- iOFV_sim[!is.na(iOFV_sim)]
      len <- length(iOFV_sim)
  for (j in 1:len) {
        iOFV_res[j,i]=(iOFV_obs[i]-iOFV_sim[j])/sd(iOFV_sim)    
  }   
  iOFV_res_median[i,1]<-median(iOFV_res[,i])
}
result<-sort(iOFV_res_median,index.return=TRUE)
flag<-0
cross<-0
for (i in 1:n.subjects) {
 if(flag==0){
   if(result$x[i]>0){
     cross<-i
     flag<-1}
    } 
}
for (i in 1:n.subjects) {
  iOFV_res_ord[,i]=iOFV_res[,result$ix[i]]
}
boxplot(iOFV_res_ord, outline=FALSE, range=0.001,names=as.character(all.iOFV_sim$ID[result$ix]),xlab="ID",ylab="iOFV RES")
vector_text<-array('',c(n.subjects,1))
index_text<-which(((iOFV_res_median[result$ix])> 3)  | ((iOFV_res_median[result$ix]) < -3))
vector_text[index_text]<-all.iOFV_sim$ID[index_text]
text(1:n.subjects,iOFV_res_median[result$ix]-1, paste("", vector_text),col="red")
abline(h=0, lwd=2, lty=3, col="black") 
abline(h=-3, lwd=2, lty=3, col="black") 
abline(h=3, lwd=2, lty=3, col="black") 
abline(v=n.subjects/2, lwd=2, lty=1, col="green")
abline(v=cross, lwd=2, lty=1, col="red")
leg.txt <- c("theoretical crossing zero line","crossing zero line")
legend("topleft", col=c('green', 'red'), leg.txt, lty=c(1,1),box.lwd = 0,bg = "white", lwd=2, cex=1)
title("iOFV RES")


# KLD iOFV
all.iOFV_sim <- read.csv(all.iofv.file)
iOFV_obs <- all.iOFV_sim$ORIGINAL
iOFV_min <- array(0,c(n.subjects,1))
iOFV_max <- array(0,c(n.subjects,1))
iOFV_kernel <- array(0,c(100,samples))
iOFV_kernel_average <-array(0,c(100,1))
KLD_sim <-array(0,c(samples,1))
for (i in 1:n.subjects){      
    iOFV_sim <- all.iOFV_sim[paste('sample.',i,sep='')]
    iOFV_min[i]<-min(iOFV_sim)
    iOFV_max[i]<-max(iOFV_sim)
  }
final_grid<-c(max(iOFV_min),min(iOFV_max))
for (i in 1:samples){      
    iOFV_sim <- all.iOFV_sim[paste('sample.',i,sep='')]
    iOFV_tmp <- density(as.vector(iOFV_sim[,1]),kernel=c("gaussian"),from=final_grid[1],to=final_grid[2],n=100)
    iOFV_kernel[,i] <- iOFV_tmp$y
    }
for (i in 1:100){      
    iOFV_kernel_average[i] <- mean(iOFV_kernel[i,])
  }
iOFV_tmp <- density(as.vector(iOFV_obs),kernel=c("gaussian"),from=final_grid[1],to=final_grid[2],n=100)
iOFV_kernel_obs <- iOFV_tmp$y
#source("KLD.R")
for (i in 1:samples){      
    KLD_tmp <-KLD(iOFV_kernel_average,iOFV_kernel[,i],base=2)
    KLD_sim[i] <- KLD_tmp$sum.KLD.px.py
  }  
KLD_tmp<-KLD(iOFV_kernel_average,iOFV_kernel_obs,base=2)
KLD_obs <- KLD_tmp$sum.KLD.px.py
index_KLD<-sort(KLD_sim[,1],index.return=TRUE)
KLD_sim_sort<-KLD_sim[index_KLD$ix]
newxlim=c(KLD_sim_sort[1],KLD_sim_sort[samples])
if(KLD_obs>KLD_sim_sort[samples]){
  newxlim=c(KLD_sim_sort[1],KLD_obs)}
if(KLD_obs<KLD_sim_sort[1]){
  newxlim=c(pOFV_obs,KLD_sim_sort[samples])}
hist(KLD_sim,xlim=newxlim,main=paste('KLD iOFV ',model.filename),xlab="KLD",freq=TRUE)
abline(v=KLD_obs,col="red",lwd=2, lty=1)
abline(v=median(KLD_sim),lwd=2, lty=4, col="green")
abline(v=quantile(KLD_sim,c(0.025, 0.975), na.rm=T)[1],lwd=2, lty=3, col="green")
abline(v=quantile(KLD_sim, c(0.025, 0.975), na.rm=T)[2],lwd=2, lty=3, col="green")
leg.txt <- c("KLDobs","median KLDsim","5th and 95th KLDsim")
legend("topright", col=c('red', 'green', 'green','green'), leg.txt, lty=c(1,4,3,3),box.lwd = 0,box.col = "white",bg = "white", lwd=2, cex=1)

dev.off()

# clear workspace and console and previous graphs
cat("\014")
rm(list=ls())
graphics.off()



