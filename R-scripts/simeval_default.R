do_outlier_plot <- 0

###########################################################################
# Kullback-Leibler Divergence (KLD)                                       #
#                                                                         #
# The purpose of the KLD function is to calculate the Kullback-Leibler    #
# divergences between two probability distributions, p(x) and p(y).       #
###########################################################################

KLD <- function(px, py, base=exp(1)){
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

#pdf for ebe_npde

pdf.title <- paste0('ebe npde diagnostic plots run ',xpose.runno)
pdf(file='PsN_ebe_npde_plots.pdf',width=10,height=7,title=pdf.title)

#EBE npde summary statistics table
library(gridExtra)
ebenpde_tmp <- read.csv(ebe.npde.file)
ebenpde_obs <- ebenpde_tmp[,seq(3,n.eta+2)]
variance <- c(1:n.eta)
mymean <- c(1:n.eta)
p_mean_not_0 <- c(1:n.eta)
p_var_not_1 <- c(1:n.eta)

for(i in 1:n.eta){ 
  variance[i] <- var(ebenpde_obs[,i])
  mymean[i] <- mean(ebenpde_obs[,i])
  p_mean_not_0[i] <- wilcox.test(ebenpde_obs[,i])$p.value
  p_var_not_1[i]  <- ks.test(ebenpde_obs[,i],pnorm,mean=mean(ebenpde_obs[,i]),sd=1)$p.value
}
#ETAs are not sorted if any iov eta, use all.eta.names from preamble
mydataframe<-data.frame('EBE NPDE' = all.eta.names, mean = format(mymean,digits=5),'p-value (H_0: mean==0)'=format(p_mean_not_0,digits=3),variance=format(variance,digits=5),'p-value (H_0: var==1)'=format(p_var_not_1,digits=3),check.names=FALSE)

if(packageVersion("gridExtra") < "2.0.0"){
  grid.table(mydataframe,show.rownames=FALSE) 
} else {
  grid.table(mydataframe,rows=NULL)  
}

#EBE-NPDE correlation graph
library(PerformanceAnalytics)
chart.Correlation(ebenpde_obs, histogram = TRUE, method = c("spearman"))

#outlier graph
#outlier table
if (require("PEIP")){
  emp_distance <- array(0,c(n.subjects,1))
  mean_ebenpde<- array(0,c(1,n.eta))
  var_ebenpde<- diag(1,n.eta,n.eta)
  identityline <- seq(1,n.subjects,by=1) #only for plot
  vector_theor_dist<- array(0,c(1,n.subjects))
  noutlier<-0
  for(i in 1:n.subjects){ 
    emp_distance[i]<- (as.matrix(ebenpde_obs[i,]-mean_ebenpde)%*%as.matrix(solve(var_ebenpde))%*%as.matrix(t(ebenpde_obs[i,]-mean_ebenpde)))
  }
  index_emp_distance<-sort(emp_distance,index.return=TRUE) #keep
  emp_distance_sort <- emp_distance[index_emp_distance$ix]
  ebe_npde_quant <- seq( (1-0.5)/n.subjects , (n.subjects-0.5)/n.subjects ,by=(1/n.subjects))
  theor_distance<-chi2inv(ebe_npde_quant,n.eta)
  out_distance<- (theor_distance[n.subjects]-emp_distance_sort[(n.subjects)])/sqrt(2)
  vector_theor_dist[n.subjects]<-theor_distance[n.subjects]
  flag<-0
  if(out_distance< outlying_criteria){  #criteria to define outlying individual
    flag<-1
    if (do_outlier_plot){
      plot(emp_distance_sort,theor_distance, xlab = "Ordered robust empirical MD^2" ,ylab= "Theoretical ChiSq MD^2", main=paste('ChiSq Q-Q plot ',model.filename))
      vector_text<-array('',c(n.subjects,1))
      index_text <- index_emp_distance$ix[ n.subjects ] # plot the ID of the outlying individual
      vector_text[ n.subjects ] <- ebenpde_tmp$ID[index_text]
      text(emp_distance_sort,theor_distance, paste("", vector_text),col="red")
      matplot(identityline,identityline,type="l",col="red",add=T)
    }
    noutlier<-noutlier+1
  } else {
    if (do_outlier_plot){
      plot(emp_distance_sort,theor_distance, xlab = "Ordered robust empirical MD^2" ,ylab= "Theoretical ChiSq MD^2", main=paste('ChiSq Q-Q plot ',model.filename))
      vector_text<-array('',c(n.subjects,1))
      matplot(identityline,identityline,type="l",col="red",add=T)
    }
  }
  
  i<-1
  while(i<n.subjects){
    flag1<-0
    ebe_npde_quant <- seq( (1-0.5)/(n.subjects-i) , ((n.subjects-i)-0.5)/(n.subjects-i) ,by=(1/(n.subjects-i)))
    theor_distance<-chi2inv(ebe_npde_quant,n.eta) #keep
    vector_theor_dist[n.subjects-i]<-theor_distance[n.subjects-i] #keep
    out_distance<- (theor_distance[(n.subjects-i)]-emp_distance_sort[(n.subjects-i)])/sqrt(2)
    if(out_distance< outlying_criteria &&flag==1){flag1 <- 1} #criteria to define outlying individual
    if(flag1==1){
      if (do_outlier_plot){
        identityline <- seq(1,(n.subjects-i),by=1)
        emp_distance_sort1 <- emp_distance_sort[1:(n.subjects-i)]
        theor_distance1 <- theor_distance[1:(n.subjects-i)]
        plot(emp_distance_sort1,theor_distance1, xlab = "Ordered robust empirical MD^2" ,ylab= "Theoretical ChiSq MD^2", main=paste('ChiSq Q-Q plot ',model.filename))
        vector_text<-array('',c(n.subjects-i,1))
        index_text <- index_emp_distance$ix[ n.subjects-i ] # plot the ID of the last 10 subjects
        vector_text[n.subjects-i ] <- ebenpde_tmp$ID[index_text]
        text(emp_distance_sort,theor_distance, paste("", vector_text),col="red")
        matplot(identityline,identityline,type="l",col="red",add=T)
      }
      noutlier<-noutlier+1
      flag<-1
    }
    i<-i+1
  }
  
  #CREATE FINAL TABLE
  if(noutlier>0){
    ncolunmns<-(3+n.eta)
    fortable <- array(0,c(noutlier,ncolunmns))
    for(i in 1:noutlier){ 
      index_text <- index_emp_distance$ix[ n.subjects -i +1 ] # plot the ID of the last subjects
      fortable[i,1]<- ebenpde_tmp$ID[index_text]
      fortable[i,2]<- format((vector_theor_dist[n.subjects-i +1]-emp_distance_sort[(n.subjects-i +1)])/sqrt(2),digits=5)
      fortable[i,3]<- format(emp_distance_sort[(n.subjects-i +1)],digits=5)
      for(j in 1:n.eta){  
        fortable[i,3+j]<- ebenpde_obs[index_text,j]
      }   
    }                      
    
    
    fortable1<-as.data.frame(fortable)
    colnames(fortable1) <- c("ID", "outlying criteria","MD distance",all.eta.names)
    
  }else{
    #no outliers
    fortable1 <- data.frame('1' = c(' '))
    colnames(fortable1) <- c("No outliers detected")
    
    
  }
  frame()
  if(packageVersion("gridExtra") < "2.0.0"){
    grid.table(fortable1,show.rownames=FALSE) 
  } else {
    grid.table(fortable1,rows=NULL)  
  }
  
  
} else{
  print("library PEIP not installed, cannot create outlier results for ebe npde")
}

dev.off()

#new pdf for vpc:s DV vs PRED, CWRES vs idv
library(xpose4)

n.vpc <- length(vpctab.filenames)
pdf(file='PsN_simeval_vpc_plots.pdf',width=10,height=7,title='simeval VPC plots')
for(j in 1:n.vpc){  
	  plots<-xpose.VPC(vpc.info=vpc.result.files[j],vpctab=vpctab.filenames[j])
	  print(plots) 
}
dev.off()


#new pdf for CWRES,IWRES
pdf.filename <- paste0('PsN_residual_plots.pdf')
pdf.title <- paste0('residual diagnostic plots run ',xpose.runno)
pdf(file=pdf.filename,width=10,height=7,title=pdf.title)

n.residuals <- length(residual.files)
variance <- c(1:n.residuals)
mymean <- c(1:n.residuals)
p_mean_not_0 <- c(1:n.residuals)
p_var_not_1 <- c(1:n.residuals)

#npde
for(j in 1:n.residuals){  
	  RESIDUAL <- read.csv(residual.files[j])
	  residual_npde <- RESIDUAL$NPDE
	  residual_npde <- residual_npde[!is.na(residual_npde)]

  	  variance[j] <- var(residual_npde)
  	  mymean[j] <- mean(residual_npde)
  	  p_mean_not_0[j] <- wilcox.test(residual_npde)$p.value
  	  p_var_not_1[j]  <- ks.test(residual_npde,pnorm,mean=mymean[j],sd=1)$p.value

	  len <- length(residual_npde)
	  H=hist(residual_npde,plot=FALSE)
	  x=seq(-3,3,length=100)
	  dx <- min(diff(H$breaks))
	  dy=len*dx*dnorm(x)
	  dy1=len*dx*H$density
	  max_npde<-max(H$density)
	  max_npde_index<-which(H$density == max_npde)
	  ylimit=max(c(max(dy1),max(dy)))
	  xlimit_min=min(x,min(residual_npde))
	  xlimit_max=max(x,max(residual_npde))
	  hist(residual_npde,main=paste0("Histogram of ",residual.names[j]," NPDE"),xlab=paste0(residual.names[j]," NPDE"),ylim=c(0,ylimit),xlim=c(xlimit_min,xlimit_max))
	  lines(x,dy, col="red")
}

#table

plot.new()

mydataframe<-data.frame('NPDE' = residual.names, mean = format(mymean,digits=5),'p-value (H_0: mean==0)'=format(p_mean_not_0,digits=3),variance=format(variance,digits=5),'p-value (H_0: var==1)'=format(p_var_not_1,digits=3),check.names=FALSE)

if(packageVersion("gridExtra") < "2.0.0"){
  grid.table(mydataframe,show.rownames=FALSE) 
} else {
  grid.table(mydataframe,rows=NULL)  
}

plot.new()
outlierframe <- read.csv("residual_outliers.csv")
if(length(outlierframe$ID)<1){
    outlierframe <- data.frame('1' = c(' '))
    colnames(outlierframe) <- c("No residual outliers detected")
}

  if(packageVersion("gridExtra") < "2.0.0"){
    grid.table(outlierframe,show.rownames=FALSE) 
  } else {
    grid.table(outlierframe,rows=NULL)  
  }


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
id_sorted <- array(0,c(n.subjects,1))
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
  id_sorted[i]=all.iOFV_sim$ID[result$ix[i]]  
}
boxplot(iOFV_res_ord, outline=FALSE, range=0.001,names=as.character(all.iOFV_sim$ID[result$ix]),xlab="ID",ylab="iOFV RES")
vector_text<-array('',c(n.subjects,1))
#this are indices in plotted sorted arr which are outside lim. always the last few, if any
index_text<-which(((iOFV_res_median[result$ix])> 3)  | ((iOFV_res_median[result$ix]) < -3))

vector_text[index_text]<-id_sorted[index_text]
text(1:n.subjects,iOFV_res_median[result$ix]-1, paste("", vector_text),col="red")
abline(h=0, lwd=2, lty=3, col="black") 
abline(h=-3, lwd=2, lty=3, col="black") 
abline(h=3, lwd=2, lty=3, col="black") 
abline(v=n.subjects/2, lwd=2, lty=1, col="green")
abline(v=cross, lwd=2, lty=1, col="red")
leg.txt <- c("theoretical crossing zero line","crossing zero line")
legend("topleft", col=c('green', 'red'), leg.txt, lty=c(1,1),box.lwd = 0,bg = "white", lwd=2, cex=1)
title("iOFV RES")


if(rplots.level>1) {

# KLD iOFV
all.iOFV_sim <- read.csv(all.iofv.file)
iOFV_obs <- all.iOFV_sim$ORIGINAL
iOFV_min <- array(0,c(n.subjects,1))
iOFV_max <- array(0,c(n.subjects,1))
iOFV_kernel <- array(0,c(100,samples))
iOFV_kernel_average <-array(0,c(100,1))
KLD_sim <-array(0,c(samples,1))
for (i in 1:samples){      
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
} #end if rplots.level > 1

dev.off()




# clear workspace and console and previous graphs
#cat("\014")
#rm(list=ls())
#graphics.off()






