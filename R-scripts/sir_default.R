#############################################################################################################
### Diagnostics for SIR
### Author: AG Dosne
### Date: March 2015
#############################################################################################################

### COMMENTS #############################################################################################################
### OUTPUT : This code will give you:
### - a PDF file with a plot of 3 comparative dOFV distributions (SIR, reference chi-square and original covariance matrix) 
### - a csv file with the value of dOFVint (integration of the dOFV distribution from their 2.5th-97.5th quantiles) for each alternative
### - a PDF file with a plot of IR values versus model parameters
### - a PDF file with a plot of CI95% for each parameter for SIR and the original covariance matrix
### - a PDF file with plots of dOFV and dOFVint if SIR would have been done with less samples (to check convergence)


require(ggplot2)                                           # check that you have all the packages installed
require(dplyr) 											   #important to load plyr after dplyr even if get warning
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

### Variables needed for dOFVint diagnostic

QUANT              <- seq(0,(RESAMPLES-1)/RESAMPLES,length.out=RESAMPLES-1)       # quantiles (at each point except last to avoid +Inf for ref chisquare) 
INTCI              <- 0.95                                                        # CI to integrate dOFVs over 
INTRANGE           <- seq((1-INTCI)/2,(1-(1-INTCI)/2),length.out=RESAMPLES*INTCI) # range of integration, use all available points
MULT               <- floor(SAMPLES/RESAMPLES)                                     # 

### Read in and format SIR results

rawres             <- read.csv(paste(working.directory,raw.results.file,sep=""))
mrawres            <- melt(rawres,measure.vars=names(rawres)[COL.ESTIMATED.PARAMS],variable_name="Parameter")
paramCI_cov        <- ddply(mrawres, .(Parameter),summarise, "PLOW"=quantile(value,probs=(1-CI/100)/2,na.rm=TRUE), "PMED"=quantile(value,probs=0.5,na.rm=TRUE), "PHIGH"=quantile(value,probs=1-(1-CI/100)/2,na.rm=TRUE))
paramCI_cov95      <- ddply(mrawres, .(Parameter),summarise, "P2.5"=quantile(value,probs=0.025,na.rm=TRUE), "PMED"=quantile(value,probs=0.5,na.rm=TRUE), "P97.5"=quantile(value,probs=0.975,na.rm=TRUE))
paramCI_sir        <- ddply(filter(mrawres,resamples==1), .(Parameter),summarise, "PLOW"=quantile(value,probs=(1-CI/100)/2,na.rm=TRUE), "PMED"=quantile(value,probs=0.5,na.rm=TRUE), "PHIGH"=quantile(value,probs=1-(1-CI/100)/2,na.rm=TRUE))

### Perform SIR with lower multiples of SAMPLES to if SAMPLES was enough

dat <- rawres[-1,]

for (i in seq(2,MULT,1)) {  # perform SIR with all multiple of RESAMPLES contained in SAMPLES starting from 2
  
  rawres.sir     <- rawres[-1,]                   # need to remove first line of final estimates (no IR)
  dat.cur        <- rawres.sir[seq(i*RESAMPLES),] # take first i*RESAMPLES vectors
  resamp         <- sample(rep(dat.cur$sample.id,MAX.RESAMPLE), RESAMPLES, replace=FALSE, prob=rep(dat.cur$importance_ratio,MAX.RESAMPLE)) # resampled models 
  resamples      <- as.numeric(dat.cur$sample.id %in% resamp)
  nresamp        <- data.frame(table(resamp))
  names(nresamp) <- c("model","nresamples")
  nresamples     <- merge(data.frame("model"=dat.cur$sample.id),nresamp,all=TRUE)
  nresamples$nresamples[is.na(nresamples$nresamples)] <- 0 # replace NAs by 0 
  dat.cur        <- cbind(dat.cur,resamples,nresamples$nresamples)
  names(dat.cur)[(ncol(dat.cur)-1):ncol(dat.cur)] <- c(paste("resamples",i*RESAMPLES,paste("rep",MAX.RESAMPLE,sep=""),sep="_"),paste("nresamples",i*RESAMPLES,paste("rep",MAX.RESAMPLE,sep=""),sep="_"))
  dat            <- left_join(dat,dat.cur)
  
} # end of loop

names(dat)[which(names(dat)=="resamples")]  <- "resamplesPsN"
dat.long       <- melt(dat,measure.vars=names(dat)[grep("resamples_",names(dat))])
dat.long$ID    <- do.call(rbind,lapply(strsplit(as.character(dat.long$variable),"_"),"[[",1))
dat.long$SAMPLES <- as.numeric(do.call(rbind,lapply(strsplit(as.character(dat.long$variable),"_"),"[[",2)))
dat.long$NREP  <- as.numeric(gsub(do.call(rbind,lapply(strsplit(as.character(dat.long$variable),"_"),"[[",3)),pattern="rep",replacement=""))

dat.wide        <- spread(select(dat.long,-variable),ID,value)
dat.wide.resamp <- filter(dat.wide,resamples==1)
dat.wide.resamp <- dat.wide.resamp[rep(seq(nrow(dat.wide.resamp)),dat.wide.resamp$nresamples),]  # need to duplicated rows which are resampled multiple times

### Reference dOFV distribution

dOFV_ref           <- data.frame(".id"=NA,"dOFV"=qchisq(QUANT,df=N.ESTIMATED.PARAMS),"QUANT"=QUANT,"TYPE"="REF")   # dOFV values of reference chisquare distribution
dOFVint_ref        <- ddply(dOFV_ref,.(TYPE),summarise, "dOFVint"=trapz(INTRANGE,dOFV-dOFV))

### dOFV distribution from initial samples (before SIR)

dOFV_cov          <- ddply(rawres,.(), summarise, "dOFV"=quantile(deltaofv,probs=QUANT,na.rm=TRUE), "QUANT"=QUANT,"TYPE"=rep("COV",length(QUANT)))  # dOFV distribution of original covariance matrix
dOFVint_cov       <- ddply(dOFV_cov,.(TYPE),summarise, "dOFVint"=trapz(INTRANGE,abs(dOFV-dOFV_ref$dOFV)))

### dOFV distribution from resamples (after SIR)

dOFV_sir          <- ddply(filter(rawres,resamples==1),.(), summarise, "dOFV"=quantile(deltaofv,probs=QUANT,na.rm=TRUE), "QUANT"=QUANT,"TYPE"=rep("SIR",length(QUANT)))  # dOFV distribution of SIR with SAMPLES
dOFVint_sir       <- ddply(dOFV_sir,.(TYPE),summarise, "dOFVint"=trapz(INTRANGE,abs(dOFV-dOFV_ref$dOFV)))

dOFV_sir_conv     <- ddply(dat.wide.resamp,.(SAMPLES), summarise, "dOFV"=quantile(deltaofv,probs=QUANT,na.rm=TRUE), "QUANT"=QUANT,"TYPE"=rep("SIR",length(QUANT)))  # dOFV distribution of SIR with decreasing SAMPLES
dOFVint_sir_conv  <- ddply(dOFV_sir_conv,.(SAMPLES,TYPE),summarise, "dOFVint"=trapz(INTRANGE,abs(dOFV-dOFV_ref$dOFV)))

### Combine files

dOFV_all       <- rbind(dOFV_ref,dOFV_cov,dOFV_sir)
dOFVint_all    <- rbind(dOFVint_ref,dOFVint_cov,dOFVint_sir)
paramCI_all    <- rbind(cbind(paramCI_cov,"TYPE"="COV"),cbind(paramCI_sir,"TYPE"="SIR"))
  
#############################################################################################################
### Do plots
#############################################################################################################

### Plot and output dOFV distributions

qdOFV <- ggplot(dOFV_all,aes(x=QUANT,y=dOFV,color=TYPE)) + 
  geom_line() +
  labs(x="Distribution quantiles") +
  scale_color_manual(values=c("black","blue","red")) +
  coord_cartesian(ylim=c(0,5*N.ESTIMATED.PARAMS))
qdOFV

pdf(paste(working.directory,"dOFV_plot.pdf",sep=""))
qdOFV
dev.off()

### Output dOFVint values

write.csv(dOFVint_all, paste(working.directory,"dOFVint.csv",sep=""),quote=FALSE,row.names=FALSE)


### Plot and output IR versus parameter plots

IRvsP <- ggplot(mrawres,aes(x=value,y=importance_ratio)) + 
  geom_point(shape=1,alpha=0.1) +
  geom_smooth(aes(group=Parameter),se=TRUE) +
  facet_wrap(~Parameter,scales="free_x") +
  theme(axis.text.x=element_text(angle=90)) +
  labs(x="Parameter value") +
  coord_cartesian(ylim=c(0,5*mean(rawres$importance_ratio,na.rm=TRUE))) +
  geom_vline(data=paramCI_cov95,aes(xintercept=c(P2.5,P97.5)),linetype=2) 
IRvsP

pdf(paste(working.directory,"IRversusParam.pdf",sep=""),width=20)
IRvsP
dev.off()

### Plot and output comparative CI

CI <- ggplot(paramCI_all,aes(x=TYPE,y=PMED)) +  
  geom_point() +
  geom_errorbar(aes(ymin=PLOW,ymax=PHIGH)) +
  facet_wrap(~Parameter,scales="free") +
  labs(y=paste("CI",CI,"%",sep=""),x="")
CI

pdf(paste(working.directory,"CI.pdf",sep=""),width=20)
CI
dev.off()

### Plot convergence 

qdOFV_conv <- ggplot(dOFV_sir_conv,aes(x=QUANT,y=dOFV,color=as.factor(SAMPLES))) + 
  geom_line() +
  geom_line(data=dOFV_ref,color="black") +
  geom_line(data=dOFV_cov,color="blue") +
  labs(x="Distribution quantiles") +
  coord_cartesian(ylim=c(0,5*N.ESTIMATED.PARAMS)) +
  scale_color_discrete(name="SIR SAMPLES") +
  theme(legend.position="bottom")
qdOFV_conv

dOFVint_conv <- ggplot(dOFVint_sir_conv, aes(x=SAMPLES,y=dOFVint)) +
  geom_point(color="red") +
  geom_line(color="red") +
  geom_hline(data=dOFVint_ref,aes(yintercept=dOFVint),color="black")
dOFVint_conv  

pdf(paste(working.directory,"convergence_samples.pdf",sep=""),width=12)
qdOFV_conv
dOFVint_conv
dev.off()

### END
#############################################################################################################
