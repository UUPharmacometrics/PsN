
#############################################################################################################
### Diagnostics for SIR
### Author: AG Dosne
### Date: August 2015
#############################################################################################################

### COMMENTS ################################################################################################
### OUTPUT : This code will give you:
### - a PDF file with base plots: 
###     - a plot of comparative dOFV distributions: SIR, reference chi-square and proposal for each iteration
###     - a plot of CI95% obtained with SIR and compared to the proposal, parameter by parameter and for each iteration
###     - a plot of CI95% obtained with SIR only, parameter by parameter and for each iteration
### - an additional PDF file if rplots.levels=2 with extended plots: 
###     - a plot of the proportion of resampled values in each bin of the parameter space, parameter by parameter and iteration by iteration
###     - a plot of the number of resampled values in the bin with the highest proportion as the number of samples increases, parameter by parameter and iteration by iteration 
###     - a plot of the number of resampled values in the bin with the highest proportion as the number of samples increases, iteration by iteration (all parameters together)
### COMMENTS ################################################################################################

require(ggplot2)             # check that you have all the packages installed
require(dplyr) 						   # important to load plyr after dplyr even if get warning
require(plyr)
require(caTools)
require(reshape)
require(tidyr)
require(gridExtra)
require(gplots)
require(RColorBrewer)
theme_set(theme_bw(base_size=20))

### The rest of the code should be left as is (but can be changed if you want to improve the graphical output of course)
#############################################################################################################

#############################################################################################################
### Read in and format data
#############################################################################################################

### Read in SIR raw results

raw.results.file <- c()
for (i in ALL.RAWRESFILES) {
  rawres.cur           <- read.csv(paste(i,sep=""))
  rawres.cur$ITERATION <- which(ALL.RAWRESFILES==i) 
  raw.results.file     <- rbind(raw.results.file,rawres.cur)
}

rawres              <- raw.results.file
final_est           <- melt(rawres[1,COL.ESTIMATED.PARAMS],variable_name="Parameter")    # final ML estimates
rawres              <- rawres[!is.na(rawres$resamples),] # remove center of distribution 
rawres              <- rawres[!is.na(rawres$ofv),]       # removes runs without ofv 
rawres$DISTRIBUTION <- rawres$ITERATION 
rawres[rawres$resamples==1,]$DISTRIBUTION <- rawres[rawres$resamples==1,]$ITERATION+1
rawres$DISTRIBUTION <- factor(rawres$DISTRIBUTION ,levels=c("REF",unique(rawres$DISTRIBUTION))) 
parnames            <- names(rawres)[COL.ESTIMATED.PARAMS]

### Create SIR specification file

sir_spec           <- ddply(rawres,.(ITERATION),summarize,"NSAMP"=length(resamples),"NRESAMP"=sum(resamples)) # take only successful samples, can differ from ALL.SAMPLES
msir_spec          <- melt(sir_spec,measure.vars=c("NSAMP","NRESAMP"))
msir_spec$variable <- factor(msir_spec$variable,labels=c("m","M"),levels=c("NRESAMP","NSAMP"))  
msir_spec$ITERATION<- factor(msir_spec$ITERATION,levels=seq(0,length(ALL.RESAMPLES)),labels=c("REF",seq(length(ALL.RESAMPLES)))) 

rawres     <- left_join(rawres,sir_spec)
mrawres    <- melt(rawres,measure.vars=parnames,variable_name="Parameter")


### Parameter CI

paramCI_cov95             <- ddply(mrawres, .(ITERATION,Parameter),summarise, "P2.5"=quantile(value,probs=0.025,na.rm=TRUE), "PMED"=quantile(value,probs=0.5,na.rm=TRUE), "P97.5"=quantile(value,probs=0.975,na.rm=TRUE))
paramCI_cov               <- ddply(mrawres, .(ITERATION,Parameter),summarise, "PLOW"=quantile(value,probs=(1-CI/100)/2,na.rm=TRUE), "PMED"=quantile(value,probs=0.5,na.rm=TRUE), "PHIGH"=quantile(value,probs=1-(1-CI/100)/2,na.rm=TRUE))
paramCI_cov$DISTRIBUTION  <- paramCI_cov$ITERATION  
paramCI_sir               <- ddply(filter(mrawres,resamples==1), .(ITERATION,Parameter),summarise, "PLOW"=quantile(value,probs=(1-CI/100)/2,na.rm=TRUE), "PMED"=quantile(value,probs=0.5,na.rm=TRUE), "PHIGH"=quantile(value,probs=1-(1-CI/100)/2,na.rm=TRUE))
paramCI_sir$DISTRIBUTION  <- paramCI_sir$ITERATION + 1  

paramCI_all               <- rbind(cbind(paramCI_cov,"TYPE"="PROPOSAL"),cbind(paramCI_sir,"TYPE"="SIR"))
paramCI_all$DISTRIBUTION  <- factor(paramCI_all$DISTRIBUTION,levels=levels(rawres$DISTRIBUTION)) # for plotting colors
paramCI_all$ITERATION     <- factor(paramCI_all$ITERATION,levels=seq(0,length(ALL.RESAMPLES)),labels=c("REF",seq(length(ALL.RESAMPLES)))) 

### dOFV distributions

RESAMPLES             <- ALL.RESAMPLES[length(ALL.RESAMPLES)]
QUANT                 <- seq(0,(RESAMPLES-1)/RESAMPLES,length.out=RESAMPLES-1)                                                                                               # quantiles (at each point except last to avoid +Inf for ref chisquare) 
dOFV_ref              <- data.frame("ITERATION"=0,"DISTRIBUTION"="REF","dOFV"=qchisq(QUANT,df=N.ESTIMATED.PARAMS),"QUANT"=QUANT,"TYPE"="SIR")                                    # dOFV distribution of reference chisquare distribution
dOFV_cov              <- ddply(rawres,.(ITERATION), summarise, "dOFV"=quantile(deltaofv,probs=QUANT,na.rm=TRUE), "QUANT"=QUANT,"TYPE"=rep("PROPOSAL",length(QUANT)))              # dOFV distribution of original covariance matrix
dOFV_cov$DISTRIBUTION <- factor(dOFV_cov$ITERATION,levels=levels(rawres$DISTRIBUTION)) # cannot do it in one step because a single cov is attributed to 2 distributinos (pre and post SIR)
dOFV_cov              <- dOFV_cov[,c(names(dOFV_ref))]
dOFV_sir              <- ddply(filter(rawres,resamples==1),.(ITERATION,DISTRIBUTION), summarise, "dOFV"=quantile(deltaofv,probs=QUANT,na.rm=TRUE), "QUANT"=QUANT,"TYPE"=rep("SIR",length(QUANT)))  # dOFV distribution of SIR with SAMPLES

dOFV_all              <- rbind(dOFV_ref,dOFV_cov,dOFV_sir)
dOFV_all$TYPE         <- factor(dOFV_all$TYPE,levels=c("PROPOSAL","SIR")) 
dOFV_all$ITERATION    <- factor(dOFV_all$ITERATION,levels=seq(0,length(ALL.RESAMPLES)),labels=c("REF",seq(length(ALL.RESAMPLES)))) 

# Calculate df for all distributions
df_est                <- ddply(dOFV_all,.(ITERATION,DISTRIBUTION,TYPE), summarise, "df"=round(mean(dOFV),1))  # get df for eac hdistribution
df_est$perc           <- seq(0.1,0.9,length.out=nrow(df_est))  
df_est$order          <- rank(df_est$df)                                             # set x and y for plotting
df_est2               <- ddply(dOFV_all,.(ITERATION,DISTRIBUTION), summarise, "perc_value"=quantile(dOFV,probs=df_est$perc,na.rm=TRUE),"perc"=df_est$perc)
df_est                <- left_join(df_est,df_est2)  
df_est$TYPE2          <- ifelse(df_est$DISTRIBUTION=="REF","",paste(df_est$TYPE))
df_est[df_est$TYPE2=="",]$df <- N.ESTIMATED.PARAMS

# Create reference chi-square for all estimated df
ref_full      <- expand.grid("df"=df_est[,"df"],"QUANT"=QUANT) # take only 1 colum because expand.grid only takes vectors as input, not dataframes
ref_full      <- merge(df_est,ref_full)
ref_full$dOFV <- qchisq(ref_full$QUANT,df=ref_full$df)

# Calculate resampling noise around SIR dOFV curves
N    <- 2000        # number of times resampling will be done
res  <- matrix(NA,ncol=4,nrow=N*sum(ALL.RESAMPLES),dimnames=list(NULL,c("ITERATION","NSAMP","NRESAMP","sample.id")))
for (i in seq(N)) { # need to resample in a loop to sample without replacement
    res.cur   <- ddply(rawres,.(ITERATION,NSAMP,NRESAMP),summarize,"sample.id"=sample(sample.id,unique(NRESAMP),prob=importance_ratio))
    res[seq((i-1)*sum(ALL.RESAMPLES)+1,i*sum(ALL.RESAMPLES)),]       <- as.matrix(res.cur,ncol=5)
}

res              <- as.data.frame(res)
res$NOISE        <- rep(seq(N),each=sum(ALL.RESAMPLES))
res              <- left_join(res,rawres[,c("sample.id","deltaofv","ITERATION")])
dOFV_sir_noise   <- ddply(res,.(ITERATION,NOISE), summarize, "dOFV"=quantile(deltaofv,probs=QUANT,na.rm=TRUE), "QUANT"=QUANT)
qdOFV_sir_noise  <- ddply(dOFV_sir_noise,.(ITERATION,QUANT), summarize, "PLOW"=quantile(dOFV,probs=0.025,na.rm=TRUE), "PHIGH"=quantile(dOFV,probs=0.975,na.rm=TRUE))
qdOFV_sir_noise$ITERATION    <- factor(qdOFV_sir_noise$ITERATION,levels=seq(0,length(ALL.RESAMPLES)),labels=c("REF",seq(length(ALL.RESAMPLES)))) 

### Number of resamples per bin

set.seed(123)
LASTIT              <- length(ALL.RESAMPLES)                  # only plot this diagnostic for the last iteration
N1                  <- 10                                     # number of bins for simulated parameters
N2                  <- 5                                      # number of bins for resampled parameters
resamp              <- ddply(rawres, .(ITERATION), summarize, "sample.id"=sample(sample.id,NRESAMP,prob=importance_ratio), "ORDER"=rep(seq(1,N2,1),each=unique(NRESAMP)/N2)) # need to resample because need to track order of sampling
resamp$resamples2   <- 1 
paramCI_covbin      <- ddply(mrawres[,c("ITERATION","Parameter","value")], .(ITERATION,Parameter),summarise, "interval"=list(unname(quantile(value,probs=seq(1/N1,1-1/N1,by=1/N1),na.rm=TRUE)))) # create bins for all parameter values
mrawres_bin         <- left_join(mrawres,paramCI_covbin)
bin                 <- ddply (mrawres_bin,.(ITERATION,Parameter), summarize, "sample.id"=sample.id, "BIN"=(findInterval(value,unique(unlist(interval)))+1))
mrawres_bin         <- left_join(mrawres_bin,bin)
mrawres_bin         <- filter(mrawres_bin,!is.na(importance_ratio))
mrawres_bin         <- left_join(mrawres_bin,resamp)  # add column to identify resampled models and their order
mrawres_bin$resamples2[is.na(mrawres_bin$resamples2)] <- 0
mrawres_bin_ratio   <- ddply(mrawres_bin,.(ITERATION,Parameter,BIN),summarize, "m"=sum(resamples2), "M"=length(resamples2),"mM"=sum(resamples2)/length(resamples2))
mrawres_bin_ratio   <- left_join(mrawres_bin_ratio,sir_spec)
# mrawres_bin_ratio$mM2 <- mrawres_bin_ratio$m/mrawres_bin_ratio$NRESAMP # alternative proportion, but not used  

# Test bin exhaustion

resamp_prop         <- ddply(filter(mrawres_bin,!is.na(ORDER)),.(ITERATION,Parameter,BIN,ORDER),summarize, "m2"=length(ORDER)) # calculate the number of resampled parameters in each sample bin and for each resample bin 
resamp_prop_dummy   <- expand.grid("ITERATION"=unique(resamp_prop$ITERATION),"BIN"=unique(resamp_prop$BIN),"ORDER"=unique(resamp_prop$ORDER),"Parameter"=unique(resamp_prop$Parameter))   # because ORDERS that have no match in BINS do not appear in previous dataset 
resamp_prop         <- left_join(resamp_prop_dummy,resamp_prop)
resamp_prop$m2[is.na(resamp_prop$m2)]  <- 0 
resamp_prop         <- left_join(resamp_prop,mrawres_bin_ratio[,c("ITERATION","Parameter","BIN","m","M","NSAMP","NRESAMP")])  
resamp_prop$prop    <- resamp_prop$m2/(resamp_prop$M/N2) 
# resamp_prop$prop2   <- resamp_prop$m2/(resamp_prop$m)  # alternative proportion, but not used
  
max_bin                <- ddply(mrawres_bin_ratio,.(ITERATION,Parameter), summarize, "BIN"=(BIN[which(m==max(m))][1])) # identify bin with max resamples
names(max_bin)[which(names(max_bin)=="BIN")] <- "MAXBIN"
max_bin2               <- ddply(resamp_prop,.(ITERATION,BIN,Parameter),summarize, "p"=mean(prop), "NRESAMP"=unique(NRESAMP), "NSAMP"=unique(NSAMP)) # more accurate since use resmaples "p"=mean(prop)
max_bin2$se            <- sqrt(max_bin2$p*(1-max_bin2$p)/(max_bin2$NSAMP/(N1*N2)))
# max_bin2$se2         <- sqrt(max_bin2$p2*(1-max_bin2$p2)/(max_bin2$NRESAMP/N))
max_bin2               <- left_join(max_bin2,max_bin[,c("ITERATION","Parameter","MAXBIN")])
resamp_prop_max        <- left_join(resamp_prop,max_bin2)
resamp_prop_max$IDBIN  <- ifelse(resamp_prop_max$BIN==resamp_prop_max$MAXBIN,1,0) 
resamp_prop_max$MAXBIN <- paste("INIT BIN",resamp_prop_max$MAXBIN)
  
#############################################################################################################
### Do plots
#############################################################################################################


#############################################################################################################
### dOFV distributions
#############################################################################################################

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

colDIST <- gg_color_hue(length(levels(dOFV_all$DISTRIBUTION)))  # default ggplot colors
colDIST <- c("darkgrey",colDIST[-1])                            # replace the first by black for reference dOFV distribution
colITER <- gg_color_hue(length(levels(dOFV_all$ITERATION)))     # default ggplot colors
colITER <- c("darkgrey",colITER[-1])                            # replace the first by black for reference dOFV distribution

qdOFV_all <- ggplot(dOFV_all,aes(x=QUANT,color=ITERATION)) + 
  geom_ribbon(data=qdOFV_sir_noise,aes(group=ITERATION,ymin=PLOW,ymax=PHIGH),show_guide=FALSE,fill="lightgrey",color="lightgrey") +
  geom_line(aes(y=dOFV,group=interaction(TYPE,ITERATION),linetype=TYPE),size=1) +
  geom_line(data=filter(ref_full,TYPE=="SIR"),aes(y=dOFV,group=ITERATION),color="grey") +
  labs(x="Distribution quantiles") +
  scale_linetype_manual(name="Iteration step",drop=FALSE,values=c(2,1)) +
  scale_color_manual(name="Iteration number",drop=FALSE,values=colITER) +
#   geom_text(data=df_est, aes(x = perc,y=1.1*perc_value,label=df),show_guide=FALSE) +
  geom_text(data=df_est, aes(x = 0.7,y=order*qchisq(0.95,df=N.ESTIMATED.PARAMS)/(2*nrow(df_est)),label=paste(df," (",TYPE2,ITERATION,")",sep="")),show_guide=FALSE,hjust=0,size=5) +
  annotate("text",x = 0.7,y=(nrow(df_est)+1)*qchisq(0.95,df=N.ESTIMATED.PARAMS)/(2*nrow(df_est)),label="Estimated df",hjust=0,fontface="italic",size=5) +
  coord_cartesian(ylim=c(0,2*qchisq(0.95,df=N.ESTIMATED.PARAMS))) +
  theme(legend.position="bottom",legend.box="horizontal") + labs(title="All dOFV distributions") +
  guides(colour = guide_legend(order = 1,title.position="top",nrow=1), linetype = guide_legend(order = 2,title.position="top",nrow=1))
# qdOFV_all

#############################################################################################################
### Comparative CI
#############################################################################################################

ci <- ggplot(paramCI_all,aes(x=interaction(TYPE,ITERATION),y=PMED,color=ITERATION)) +  
  geom_point() +
  geom_errorbar(aes(ymin=PLOW,ymax=PHIGH,linetype=TYPE),width=0.5) +
  geom_hline(data=final_est,aes(yintercept=value),linetype=2) +
  facet_wrap(~Parameter,scales="free_y") +
  scale_color_manual(name="Iteration number",values=colITER[-1]) +
  scale_linetype_manual(name="Iteration step",drop=FALSE,values=c(2,1)) +
  labs(y=paste("CI",CI,"%",sep=""),x="",title="CI over all iterations") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),legend.position="bottom",legend.box="horizontal") +
  guides(colour = guide_legend(order = 1,title.position="top",nrow=1), linetype = guide_legend(order = 2,title.position="top",nrow=1))
# ci

ci_sir <- ci %+% filter(paramCI_all, TYPE=="SIR" | (TYPE=="PROPOSAL" & ITERATION=="1"))  +  labs(title="CI of SIR densities over iterations") + theme(legend.position="none") 
# ci_sir

#############################################################################################################
### Parameter bin plots by iteration
#############################################################################################################

mM_ratio   <-  vector('list', LASTIT)
m2_maxbin  <-  vector('list', LASTIT)
all_maxbin <-  vector('list', LASTIT)

for (i in seq(LASTIT)) {
  
mM_ratio.cur <- ggplot(filter(mrawres_bin_ratio,ITERATION==i),aes(x=BIN,y=mM)) +
  geom_ribbon(aes(ymin=(NRESAMP/NSAMP-qnorm(0.975)*sqrt(NRESAMP/NSAMP*(1-NRESAMP/NSAMP)/(NSAMP/N1))),ymax=(NRESAMP/NSAMP+qnorm(0.975)*sqrt(NRESAMP/NSAMP*(1-NRESAMP/NSAMP)/(NSAMP/N1)))),alpha=0.2) +  # stochastic noise
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept=NRESAMP/NSAMP),linetype=2,color="black") +
  geom_hline(aes(yintercept=0),linetype=1,color="white") + 
  facet_wrap(ITERATION~Parameter) +
  scale_x_continuous(breaks=seq(1,N1),limits=c(1,N1)) +
  labs(x="Percentile bin of initial samples",y="Proportion resampled",title=paste("Adequacy of proposal density of iteration",i))
mM_ratio[[i]] <- mM_ratio.cur

m2_maxbin.cur  <- ggplot(filter(resamp_prop_max,IDBIN==1 & ITERATION==i),aes(x=ORDER,y=m2)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept=p*M/N2),linetype=2) +
  geom_ribbon(aes(ymin=(p-qnorm(0.975)*se)*(M/N2),ymax=(p+qnorm(0.975)*se)*(M/N2)),alpha=0.2) +
  facet_wrap(ITERATION~Parameter+BIN,scales="free_y",drop=TRUE) +
  scale_x_continuous(breaks=seq(1,N2,by=1),limits=c(1,N2)) +
  scale_y_continuous(limits=c(0,unique(filter(resamp_prop_max,IDBIN==1 & ITERATION==LASTIT)$NSAMP)/(N1*N2))) +
  labs(x="Percentile bin of resamples",y="Number of parameters resampled",title=paste("Exhaustion of samples of iteration",i,"by parameter")) 
m2_maxbin[[i]] <- m2_maxbin.cur

all_maxbin.cur  <- ggplot(filter(resamp_prop_max,IDBIN==1 & ITERATION==i),aes(x=ORDER,y=m2,group=Parameter)) +
  geom_point() +
  geom_line(alpha=0.2) +
  geom_smooth(aes(group=1),color="blue",size=2,se=FALSE) +
  scale_x_continuous(breaks=seq(1,N2,by=1),limits=c(1,N2)) +
  scale_y_continuous(limits=c(0,2*unique(filter(resamp_prop_max,IDBIN==1 & ITERATION==i)$NRESAMP)/(N1*N2))) +
  labs(x="Percentile bin of resamples",y="Number of parameters resampled",title=paste("Exhaustion of samples of iteration",i, "over all parameters")) 
all_maxbin[[i]] <- all_maxbin.cur

}


#############################################################################################################
### Output plots
#############################################################################################################

if(rplots.level>=1) {
  pdf(file=paste(working.directory,"PsN_plots_base.pdf",sep=""),title=pdf.title,width=10,height=10)  
  print(qdOFV_all)
  print(ci_sir)
  dev.off()
}

if(rplots.level>1) {
  pdf(file=paste(working.directory,"PsN_plots_extended.pdf",sep=""),title=pdf.title,width=15,height=15)
  print(do.call("grid.arrange", c(mM_ratio, ncol=ceiling(LASTIT/2))))
  print(do.call("grid.arrange", c(m2_maxbin, ncol=ceiling(LASTIT/2))))
  print(do.call("grid.arrange", c(all_maxbin, ncol=ceiling(LASTIT/2))))
  dev.off()
}

### END
#############################################################################################################