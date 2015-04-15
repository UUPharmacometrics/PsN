#############################################################################################################
### Diagnostics for SIR
### Author: AG Dosne
### Date: March 2015
#############################################################################################################

### COMMENTS #############################################################################################################
### OUTPUT : This code will give you:
### - a PDF file with 
### - a plot of 3 comparative dOFV distributions: SIR, reference chi-square and original covariance matrix 
### - a plot of number of resampled values in each bin of the parameter space, parameter by parameter
### - a plot of CI95% obtained with SIR and compared to the original covariance matrix, parameter by parameter

require(ggplot2)                       # check that you have all the packages installed
require(dplyr) 						   #important to load plyr after dplyr even if get warning
require(plyr)
require(caTools)
require(reshape)
require(tidyr)
theme_set(theme_bw())

### The rest of the code should be left as is (but can be changed if you want to improve the graphical output of course)
#############################################################################################################

#############################################################################################################
### Read in and format data
#############################################################################################################

### Read in SIR results

rawres             <- read.csv(paste(working.directory,raw.results.file,sep=""))
mrawres            <- melt(rawres,measure.vars=names(rawres)[COL.ESTIMATED.PARAMS],variable_name="Parameter")

### Parameter CI

paramCI_cov95      <- ddply(mrawres, .(Parameter),summarise, "P2.5"=quantile(value,probs=0.025,na.rm=TRUE), "PMED"=quantile(value,probs=0.5,na.rm=TRUE), "P97.5"=quantile(value,probs=0.975,na.rm=TRUE))
paramCI_cov        <- ddply(mrawres, .(Parameter),summarise, "PLOW"=quantile(value,probs=(1-CI/100)/2,na.rm=TRUE), "PMED"=quantile(value,probs=0.5,na.rm=TRUE), "PHIGH"=quantile(value,probs=1-(1-CI/100)/2,na.rm=TRUE))
paramCI_sir        <- ddply(filter(mrawres,resamples==1), .(Parameter),summarise, "PLOW"=quantile(value,probs=(1-CI/100)/2,na.rm=TRUE), "PMED"=quantile(value,probs=0.5,na.rm=TRUE), "PHIGH"=quantile(value,probs=1-(1-CI/100)/2,na.rm=TRUE))

paramCI_all        <- rbind(cbind(paramCI_cov,"TYPE"="COV"),cbind(paramCI_sir,"TYPE"="SIR"))

### dOFV distributions

QUANT              <- seq(0,(RESAMPLES-1)/RESAMPLES,length.out=RESAMPLES-1)                                                                         # quantiles (at each point except last to avoid +Inf for ref chisquare) 
dOFV_ref           <- data.frame(".id"=NA,"dOFV"=qchisq(QUANT,df=N.ESTIMATED.PARAMS),"QUANT"=QUANT,"TYPE"="REF")                                    # dOFV distribution of reference chisquare distribution
dOFV_cov           <- ddply(rawres,.(), summarise, "dOFV"=quantile(deltaofv,probs=QUANT,na.rm=TRUE), "QUANT"=QUANT,"TYPE"=rep("COV",length(QUANT)))  # dOFV distribution of original covariance matrix
dOFV_sir           <- ddply(filter(rawres,resamples==1),.(), summarise, "dOFV"=quantile(deltaofv,probs=QUANT,na.rm=TRUE), "QUANT"=QUANT,"TYPE"=rep("SIR",length(QUANT)))  # dOFV distribution of SIR with SAMPLES

dOFV_all           <- rbind(dOFV_ref,dOFV_cov,dOFV_sir)

### Number of resamples per bin

N                 <- 10                                     # number of bins 
paramCI_covbin    <- ddply(mrawres[,c("Parameter","value")], .(Parameter),summarise, "interval"=list(unname(quantile(value,probs=seq(1/N,1-1/N,by=1/N),na.rm=TRUE))))
mrawres_bin       <- left_join(mrawres,paramCI_covbin)
bin               <- ddply (mrawres_bin,.(Parameter), summarize, "BIN"=findInterval(value,unique(unlist(interval))))
mrawres_bin$BIN   <- bin$BIN
mrawres_bin       <- filter(mrawres_bin,!is.na(importance_ratio))
mrawres_bin_ratio <- ddply(mrawres_bin,.(Parameter,BIN),summarize, "m"=sum(resamples), "M"=length(resamples),"m/M"=sum(resamples)/length(resamples))

  
#############################################################################################################
### Do plots
#############################################################################################################

pdf(file=pdf.filename,title=pdf.title,width=20)

### dOFV distributions

qdOFV <- ggplot(dOFV_all,aes(x=QUANT,y=dOFV,color=TYPE)) + 
  geom_line() +
  labs(x="Distribution quantiles") +
  scale_color_manual(values=c("black","blue","red")) +
  coord_cartesian(ylim=c(0,5*N.ESTIMATED.PARAMS))
qdOFV


### Parameter bin plots

mM_ratio <- ggplot(mrawres_bin_ratio,aes(x=BIN+1,y=m)) +
  geom_point() +
  geom_line() +
  geom_text(aes(y=m+8,label=m),size=4) +
  geom_hline(yintercept=c(0,RESAMPLES/N),linetype=c(1,2),color=c("white","black")) + 
  facet_wrap(~Parameter,scales="free") +
  scale_x_continuous(breaks=seq(1,N),limits=c(1,N)) +
  labs(x="Bin",y="Number of resamples")
mM_ratio


### Comparative CI

CI <- ggplot(paramCI_all,aes(x=TYPE,y=PMED)) +  
  geom_point() +
  geom_errorbar(aes(ymin=PLOW,ymax=PHIGH)) +
  facet_wrap(~Parameter,scales="free") +
  labs(y=paste("CI",CI,"%",sep=""),x="")
CI

dev.off()

### END
#############################################################################################################

