# get libPaths
source(file.path(rscripts.directory,"common/R_info.R"))
R_info(directory=working.directory,only_libPaths=T)
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

pkg1 <- c("ggplot2","dplyr","plyr","reshape","tidyr","gridExtra","gplots","RColorBrewer","MCMCpack","stats4")  # check that you have all the packages installed
pkg2 <- paste0("package:",pkg1) ### just paste package with all the pacakges ##### 
lapply(pkg1, require, character.only = TRUE)                                                # important to load plyr after dplyr even if get warning
theme_set(theme_bw(base_size=20))  ### theme is set to black and white #####

#add R_info to the meta file
R_info(directory=working.directory)    ##### set up work directory ####
# lapply(pkg2, detach, character.only = TRUE, unload = TRUE)

### The rest of the code should be left as is (but can be changed if you want to improve the graphical output of course)
#############################################################################################################

#############################################################################################################
### Read in and format data
#############################################################################################################

### Read in SIR raw results

raw.results.file <- c()   ###  initialize empty vector #####
for (i in ALL.RAWRESFILES) {                    ###### ALL.RAWRESFILES is defined above, it has all the names of the files ###### 
  rawres.cur           <- read.csv(paste(i,sep=""))    
  rawres.cur$ITERATION <- which(ALL.RAWRESFILES==i) #### adding ITERATIONS #####
  raw.results.file     <- rbind(raw.results.file,rawres.cur)  ####### binding 2 data frames by rows ####
}

rawres              <- raw.results.file  #### no need of this line ###### 
final_est           <- reshape::melt(rawres[1,COL.ESTIMATED.PARAMS],variable_name="Parameter")    # final ML estimates ## melt dataframe ##
rawres              <- rawres[!is.na(rawres$resamples),] # remove center of distribution ## remove NA from resmaples ###
rawres              <- rawres[!is.na(rawres$ofv),]       # removes runs without ofv      #### remove NA from OFV if any ######
rawres$DISTRIBUTION <- rawres$ITERATION        #### add one column with the name  distribution ######
rawres[rawres$resamples==1,]$DISTRIBUTION <- rawres[rawres$resamples==1,]$ITERATION+1  #### add resample and iterations and write that value in distribution ####
rawres$DISTRIBUTION <- factor(rawres$DISTRIBUTION ,levels=c("REF",unique(rawres$DISTRIBUTION))) ### no idea ####
parnames            <- names(rawres)[COL.ESTIMATED.PARAMS]    ##### now parnames contains the names of the cloumns ####

### Create SIR specification file

LASTIT             <- length(ALL.RESAMPLES)                  ###  line not needed ### # last iteration
summary_it         <- read.csv("summary_iterations.csv")     ## read in the file ####
sir_spec           <- data.frame("ITERATION"=summary_it$iteration,"NSAMP"=summary_it$successful.samples,"NRESAMP"=summary_it$actual.resamples) ## what is the need to do that ####
sir_spec           <- dplyr::filter(sir_spec, ITERATION != 0) # ITERATION=0 only used with rawres input to estimate mutlivariate normal
msir_spec          <- reshape::melt(sir_spec,measure.vars=c("NSAMP","NRESAMP"))
msir_spec$variable <- factor(msir_spec$variable,labels=c("m","M"),levels=c("NRESAMP","NSAMP"))  
msir_spec$ITERATION<- factor(msir_spec$ITERATION,levels=seq(0,length(ALL.RESAMPLES)),labels=c("REF",seq(length(ALL.RESAMPLES)))) 

rawres     <- dplyr::left_join(rawres,sir_spec) ##### rawres file now have NRESAMP and NSAMP columns also   ########
mrawres    <- reshape::melt(rawres,measure.vars=parnames,variable_name="Parameter")


### Parameter CI

paramCI_cov95             <- plyr::ddply(mrawres, .(ITERATION,Parameter),summarise, "P2.5"=quantile(value,probs=0.025,na.rm=TRUE), "PMED"=quantile(value,probs=0.5,na.rm=TRUE), "P97.5"=quantile(value,probs=0.975,na.rm=TRUE))
paramCI_cov               <- plyr::ddply(mrawres, .(ITERATION,Parameter),summarise, "PLOW"=quantile(value,probs=(1-CI/100)/2,na.rm=TRUE), "PMED"=quantile(value,probs=0.5,na.rm=TRUE), "PHIGH"=quantile(value,probs=1-(1-CI/100)/2,na.rm=TRUE))
paramCI_cov$DISTRIBUTION  <- paramCI_cov$ITERATION  
paramCI_sir               <- plyr::ddply(dplyr::filter(mrawres,resamples==1), .(ITERATION,Parameter),summarise, "PLOW"=quantile(value,probs=(1-CI/100)/2,na.rm=TRUE), "PMED"=quantile(value,probs=0.5,na.rm=TRUE), "PHIGH"=quantile(value,probs=1-(1-CI/100)/2,na.rm=TRUE))
paramCI_sir$DISTRIBUTION  <- paramCI_sir$ITERATION + 1  

paramCI_all               <- rbind(cbind(paramCI_cov,"TYPE"="PROPOSAL"),cbind(paramCI_sir,"TYPE"="SIR"))
paramCI_all$DISTRIBUTION  <- factor(paramCI_all$DISTRIBUTION,levels=levels(rawres$DISTRIBUTION)) # for plotting colors
paramCI_all$ITERATION     <- factor(paramCI_all$ITERATION,levels=seq(0,length(ALL.RESAMPLES)),labels=c("REF",seq(length(ALL.RESAMPLES)))) 

### dOFV distributions

set.seed(123)
RESAMPLES             <- ALL.RESAMPLES[length(ALL.RESAMPLES)]
QUANT                 <- seq(0,(RESAMPLES-1)/RESAMPLES,length.out=RESAMPLES-1)                                                                                               # quantiles (at each point except last to avoid +Inf for ref chisquare) 
dOFV_ref              <- data.frame("ITERATION"=0,"DISTRIBUTION"="REF","dOFV"=stats::qchisq(QUANT,df=N.ESTIMATED.PARAMS),"QUANT"=QUANT,"TYPE"="SIR")                                    # dOFV distribution of reference chisquare distribution
dOFV_cov              <- plyr::ddply(rawres,.(ITERATION), summarise, "dOFV"=quantile(deltaofv,probs=QUANT,na.rm=TRUE), "QUANT"=QUANT,"TYPE"=rep("PROPOSAL",length(QUANT)))              # dOFV distribution of original covariance matrix
dOFV_cov$DISTRIBUTION <- factor(dOFV_cov$ITERATION,levels=c("REF",paste(seq(LASTIT+1)))) # cannot do it in one step because a single cov is attributed to 2 distributinos (pre and post SIR)
dOFV_cov              <- dOFV_cov[,c(names(dOFV_ref))]
dOFV_sir              <- plyr::ddply(dplyr::filter(rawres,resamples==1),.(ITERATION,DISTRIBUTION), summarise, "dOFV"=quantile(deltaofv,probs=QUANT,na.rm=TRUE), "QUANT"=QUANT,"TYPE"=rep("SIR",length(QUANT)))  # dOFV distribution of SIR with SAMPLES

dOFV_all              <- rbind(dOFV_ref,dOFV_cov,dOFV_sir)
dOFV_all$TYPE         <- factor(dOFV_all$TYPE,levels=c("PROPOSAL","SIR")) 
dOFV_all$ITERATION    <- factor(dOFV_all$ITERATION,levels=seq(0,length(ALL.RESAMPLES)),labels=c("REF",seq(length(ALL.RESAMPLES)))) 

# Calculate df for all distributions
df_est                <- plyr::ddply(dOFV_all,.(ITERATION,DISTRIBUTION,TYPE), summarise, "df"=round(mean(dOFV),1))  # get df for each distribution
df_est$perc           <- seq(0.1,0.9,length.out=nrow(df_est))  
df_est$order          <- rank(df_est$df,ties.method="first")                                             # set x and y for plotting
df_est2               <- plyr::ddply(dOFV_all,.(ITERATION,DISTRIBUTION), summarise, "perc_value"=quantile(dOFV,probs=df_est$perc,na.rm=TRUE),"perc"=df_est$perc)
df_est                <- dplyr::left_join(df_est,df_est2)  
df_est$TYPE2          <- ifelse(df_est$DISTRIBUTION=="REF","",paste(df_est$TYPE))
df_est[df_est$TYPE2=="",]$df <- N.ESTIMATED.PARAMS

# Create reference chi-square for all estimated df
ref_full      <- expand.grid("df"=df_est[,"df"],"QUANT"=QUANT) # take only 1 colum because expand.grid only takes vectors as input, not dataframes
ref_full      <- merge(df_est,ref_full)
ref_full$dOFV <- stats::qchisq(ref_full$QUANT,df=ref_full$df)

# Calculate resampling noise around last SIR dOFV curve
N    <- 2000        # number of times resampling will be done
res  <- matrix(NA,ncol=4,nrow=N*sir_spec$NRESAMP[nrow(sir_spec)],dimnames=list(NULL,c("ITERATION","NSAMP","NRESAMP","sample.id")))
for (i in seq(N)) { # need to resample in a loop to sample without replacement
  res.cur   <- plyr::ddply(dplyr::filter(rawres,ITERATION==nrow(sir_spec)),.(ITERATION,NSAMP,NRESAMP),summarize,"sample.id"=sample(sample.id,unique(NRESAMP),prob=importance_ratio))
  res[seq((i-1)*sir_spec$NRESAMP[nrow(sir_spec)]+1,i*sir_spec$NRESAMP[nrow(sir_spec)]),]       <- as.matrix(res.cur,ncol=5)
}

res              <- as.data.frame(res)
res$NOISE        <- rep(seq(N),each=sir_spec$NRESAMP[nrow(sir_spec)])
res              <- dplyr::left_join(res,rawres[,c("sample.id","deltaofv","ITERATION")])
dOFV_sir_noise   <- plyr::ddply(res,.(ITERATION,NOISE), summarize, "dOFV"=quantile(deltaofv,probs=QUANT,na.rm=TRUE), "QUANT"=QUANT)
qdOFV_sir_noise  <- plyr::ddply(dOFV_sir_noise,.(ITERATION,QUANT), summarize, "PLOW"=quantile(dOFV,probs=0.025,na.rm=TRUE), "PHIGH"=quantile(dOFV,probs=0.975,na.rm=TRUE))
qdOFV_sir_noise$ITERATION    <- factor(qdOFV_sir_noise$ITERATION,levels=seq(0,length(ALL.RESAMPLES)),labels=c("REF",seq(length(ALL.RESAMPLES)))) 

### Covmat visualization (Only for proposal and final) 

cov.proposal <- stats::var(dplyr::filter(rawres,ITERATION==1)[,COL.ESTIMATED.PARAMS ])
cov.final    <- stats::var(dplyr::filter(rawres,ITERATION==LASTIT & resamples==1)[,COL.ESTIMATED.PARAMS ])

paramCI_all$asym <- (paramCI_all$PHIGH-paramCI_all$PMED)/(paramCI_all$PMED-paramCI_all$PLOW) # ratio between distances median-CI bound
asym.proposal    <- dplyr::filter(paramCI_all,ITERATION==1 & TYPE=="PROPOSAL")
asym.final       <- dplyr::filter(paramCI_all,ITERATION==LASTIT & TYPE=="SIR")

plot.cor <- function (cov,asym,final_est,title) { 
  
  mat       <- as.matrix(cov)
  var       <- diag(mat)
  cor       <- stats::cov2cor(mat)                  # returns 1 on the diagonal, needs to be replaced by SD
  diag(cor) <- sqrt(var)
  cor[lower.tri(cor)] <- NA                  # plot only lower triangular part of  symmetric matrix
  rse       <- as.matrix(100*abs(diag(cor)/as.matrix(final_est[,2]))) # compute RSE=100*SE/FINAL_EST
  diag(cor)     <- rse                   # replace SE by RSE
  cor           <- as.data.frame(cor) 
  colnames(cor) <- rownames(cor)
  cor$Parameter <- rownames(cor)
  
  mcor           <- reshape::melt(cor,id.var="Parameter")
  mcor$Parameter <- ordered(mcor$Parameter, levels=rownames(cor)) 
  mcor$variable  <- ordered(mcor$variable, levels=rev(rownames(cor)))
  mcor           <- mcor[!is.na(mcor$value),]
  mcor$Fvalue    <- factor(findInterval(abs(mcor$value),seq(0,0.9,0.1)),levels=seq(10),labels=paste(seq(0,0.9,0.1),"-",seq(0.1,1,0.1),sep=""))
  mcor[mcor$variable==mcor$Parameter,]$Fvalue    <- NA # identify diagonal element
  mcor           <- dplyr::left_join(mcor,asym[,c("Parameter","asym")]) # add asymmetry measurement  
  mcor[!is.na(mcor$Fvalue),]$asym      <- NA 
  mcor$Fvalue2   <- factor(findInterval(mcor$asym,c(0.5,1,1.5,2)),levels=seq(0,4),labels=c("<0.5",paste(c(0.5,1,1.25),"-",c(1,1.25,2),sep=""),">2"))
  
  col <- rev(brewer.pal(n=10,name="RdYlGn"))
  
  corplot       <-  ggplot2::ggplot(mcor,aes(x=Parameter,y=variable)) +
    geom_tile(aes(fill=Fvalue)) +
    scale_fill_manual(values=col,name="Correlation level",drop=FALSE) +
    scale_color_manual(values=col[c(9,7,2,7,9)],name="Asymmetry",drop=FALSE) +
    geom_text(data=mcor[is.na(mcor$Fvalue2),],aes(label=round(value,2)),size=7) +
    geom_text(data=mcor[!is.na(mcor$Fvalue2),],aes(label=round(value,2),color=Fvalue2),size=7) +
    labs(x="",y="",title=paste(title,"\nRSE (%) - correlation (-) plot")) +
    theme(axis.text.x=element_text(angle=90),axis.ticks=element_blank(),panel.grid = element_blank())
  corplot
  
  return(corplot)  # return plot
}

### Number of resamples per bin

if(rplots.level>1) {
  N1                  <- 10                                     # number of bins for simulated parameters
  N2                  <- 5                                      # number of bins for resampled parameters
  resamp              <- plyr::ddply(rawres[rawres$resamples!=0,], .(ITERATION), summarize, "sample.id"=sample.id, "ORDER"=findInterval(sample_order,seq(0,unique(NRESAMP),length.out=(N2+1)),all.inside=T)) # need to resample because need to track order of sampling
  paramCI_covbin      <- plyr::ddply(mrawres[,c("ITERATION","Parameter","value")], .(ITERATION,Parameter),summarise, "interval"=list(unname(quantile(value,probs=seq(1/N1,1-1/N1,by=1/N1),na.rm=TRUE)))) # create bins for all parameter values
  mrawres_bin         <- dplyr::left_join(mrawres,paramCI_covbin)
  bin                 <- plyr::ddply (mrawres_bin,.(ITERATION,Parameter), summarize, "sample.id"=sample.id, "BIN"=(findInterval(value,unique(unlist(interval)))+1))
  mrawres_bin         <- dplyr::left_join(mrawres_bin,bin)
  mrawres_bin         <- dplyr::filter(mrawres_bin,!is.na(importance_ratio))
  mrawres_bin         <- dplyr::left_join(mrawres_bin,resamp)  # add column to identify resampled models and their order
  mrawres_bin_ratio   <- plyr::ddply(mrawres_bin,.(ITERATION,Parameter,BIN),summarize, "m"=sum(resamples), "M"=length(resamples),"mM"=sum(resamples)/length(resamples))
  mrawres_bin_ratio   <- dplyr::left_join(mrawres_bin_ratio,sir_spec)
  # mrawres_bin_ratio$mM2 <- mrawres_bin_ratio$m/mrawres_bin_ratio$NRESAMP # alternative proportion, but not used  
  
  # Test bin exhaustion
  
  resamp_prop         <- plyr::ddply(dplyr::filter(mrawres_bin,sample_order!=0),.(ITERATION,Parameter,BIN,ORDER),summarize, "m2"=length(sample_order)) # calculate the number of resampled parameters in each sample bin and for each resample bin 
  resamp_prop_dummy   <- expand.grid("ITERATION"=unique(resamp_prop$ITERATION),"BIN"=unique(resamp_prop$BIN),"ORDER"=unique(resamp_prop$ORDER),"Parameter"=unique(resamp_prop$Parameter))   # because sample_order that have no match in BINS do not appear in previous dataset 
  resamp_prop         <- dplyr::left_join(resamp_prop_dummy,resamp_prop)
  resamp_prop$m2[is.na(resamp_prop$m2)]  <- 0 
  resamp_prop         <- dplyr::left_join(resamp_prop,mrawres_bin_ratio[,c("ITERATION","Parameter","BIN","m","M","NSAMP","NRESAMP")])  
  resamp_prop$prop    <- resamp_prop$m2/(resamp_prop$M/N2) 
  # resamp_prop$prop2   <- resamp_prop$m2/(resamp_prop$m)  # alternative proportion, but not used
  
  max_bin                <- plyr::ddply(mrawres_bin_ratio,.(ITERATION,Parameter), summarize, "BIN"=(BIN[which(m==max(m))][1])) # identify bin with max resamples
  names(max_bin)[which(names(max_bin)=="BIN")] <- "MAXBIN"
  max_bin2               <- plyr::ddply(resamp_prop,.(ITERATION,BIN,Parameter),summarize, "p"=mean(prop), "NRESAMP"=unique(NRESAMP), "NSAMP"=unique(NSAMP)) # more accurate since use resmaples "p"=mean(prop)
  max_bin2$se            <- sqrt(max_bin2$p*(1-max_bin2$p)/(max_bin2$NSAMP/(N1*N2)))
  # max_bin2$se2         <- sqrt(max_bin2$p2*(1-max_bin2$p2)/(max_bin2$NRESAMP/N))
  max_bin2               <- dplyr::left_join(max_bin2,max_bin[,c("ITERATION","Parameter","MAXBIN")])
  resamp_prop_max        <- dplyr::left_join(resamp_prop,max_bin2)
  resamp_prop_max$IDBIN  <- ifelse(resamp_prop_max$BIN==resamp_prop_max$MAXBIN,1,0) 
  resamp_prop_max$MAXBIN <- paste("INIT BIN",resamp_prop_max$MAXBIN)
  
  ### Calculate N for OMEGA distributions
  if (N.ESTIMATED.OMEGAS >= 1) { 
    dat      <- dplyr::filter(rawres, ITERATION==LASTIT & resamples==1)
    omega.col<- COL.ESTIMATED.PARAMS[seq((N.ESTIMATED.THETAS+1),(N.ESTIMATED.THETAS+N.ESTIMATED.OMEGAS))]                                                                            # column index of the variances we want to estimate the df of 
    parnames <- names(dat)[omega.col]                                                                         # names of the variances  
    var      <- dplyr::filter(final_est,Parameter %in% parnames)                                                     # MLE estimates
    mdat     <- reshape::melt(dat[,omega.col,drop=FALSE]) 
    se       <- plyr::ddply(mdat,.(variable),summarize,"se"=sd(value))
    se$MLE   <- var$value
    se$df_norm  <- 2*(se$MLE/se$se)**2+1
    mdat     <- plyr::join(mdat,se)
    
    full.df.est <- function (x) {  # function for the fitting
      x   <- as.data.frame(x)
      nLL <- function(df,s) { # easier if defined inside full.df.est so same environment (else does not find "x")
        ylist <- lapply(x[,2],as.matrix)
        if(df>=1)
          nLL <- -sum(log(unlist(lapply(ylist,diwish, S=as.matrix(s), v=df))))
        if(df<1) nLL <- NA
        return(nLL)
      }
      fit <- try(stats4::mle(nLL,start=list(df=x$df_norm[1],s=x$df_norm[1]*mean(x[,2]))), silent=TRUE) # test if the fit returns an error
      if ('try-error' %in% class(fit)) db <- rep(NA,7)
      else {
        fit  <- stats4::mle(nLL,start=list(df=x$df_norm[1],s=x$df_norm[1]*mean(x[,2])))
        db   <- data.frame(nrow(x),fit@details$par[1],fit@details$par[2],
                           sqrt(diag(fit@vcov))[1],sqrt(diag(fit@vcov))[2],
                           fit@min,fit@details$convergence)}
      # names(db) <- c("nobs","df","s","se_df","se_s","nLL","convergence")  
      return(db)
    } # end of full.df.est function
    
    fit     <- matrix(0,ncol=7,nrow=length(levels(mdat$variable)),dimnames=list(levels(mdat$variable),c("nobs","df","s","se_df","se_s","nLL","convergence") ))
    for (i in seq(length(levels(mdat$variable)))) {
      fit[i,] <- as.matrix(full.df.est(dplyr::filter(mdat,variable==levels(mdat$variable)[i])),nrow=1)
    }
    
    fit_db          <- as.data.frame(fit)
    fit_db$variable <- levels(mdat$variable)
    mdat            <- plyr::join(mdat,fit_db)
    
    simdat          <- matrix(NA,nrow=nrow(mdat)) # Simulate data from inverse Wishart from estimated df and s
    for (i in seq(nrow(mdat))) {
      if(is.na(mdat$df[i])==FALSE) simdat[i,] <- riwish(v=mdat$df[i],S=as.matrix(mdat$s[i]))
    }
    mdat$simdat      <- as.numeric(simdat)
    mdat$LEGEND_NORM <- paste(mdat$variable,round(mdat$df_norm,0),sep=":")    # for plotting
    mdat$LEGEND_NORM <- factor(mdat$LEGEND_NORM,levels=unique(mdat$LEGEND_NORM))
    mdat$LEGEND_WISH <- paste(mdat$variable,round(mdat$df,0),sep=":")    
    mdat$LEGEND_WISH <- factor(mdat$LEGEND_WISH,levels=unique(mdat$LEGEND_WISH))
  } # end of estimated.omega >= 1
} # end of rplots.level >1

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

warn <- ""  # print warning if proposal < chi-square > 25% of the time
msg  <- "The proposal is not entirely above the reference chi-square.\n It is advised to restart SIR with an inflated proposal (e.g. -theta_inflation=1.5 -omega_inflation=1.5 -sigma_inflation=1.5)"
test <- sum(dplyr::filter(dOFV_all,ITERATION==1 & TYPE=="PROPOSAL")$dOFV<dplyr::filter(dOFV_all,ITERATION=="REF")$dOFV)/length(dplyr::filter(dOFV_all,ITERATION=="REF")$dOFV)>0.25
warn <- ifelse(test==TRUE,msg,warn)

qdOFV_all <- ggplot2::ggplot(dOFV_all,aes(x=QUANT,color=ITERATION)) + 
  geom_ribbon(data=qdOFV_sir_noise[qdOFV_sir_noise$ITERATION %in% c(LASTIT-1,LASTIT),],aes(group=ITERATION,ymin=PLOW,ymax=PHIGH,fill=ITERATION),show.legend=FALSE,alpha=0.3,color=NA) +
  geom_line(aes(y=dOFV,group=interaction(TYPE,ITERATION),linetype=TYPE),size=1) +
  labs(x="Distribution quantiles") +
  scale_linetype_manual(name="Iteration step",drop=FALSE,values=c(2,1)) +
  scale_color_manual(name="Iteration number",drop=FALSE,values=colITER) +
  scale_fill_manual(name="Iteration number",drop=FALSE,values=colITER) +
  #   geom_text(data=df_est, aes(x = perc,y=1.1*perc_value,label=df),show.legend=FALSE) +
  geom_text(data=df_est, aes(x = 0.7,y=order*stats::qchisq(0.95,df=N.ESTIMATED.PARAMS)/(2*nrow(df_est)),label=paste(df," (",TYPE2,ITERATION,")",sep="")),show.legend=FALSE,hjust=0,size=5) +
  annotate("text",x = 0.7,y=(nrow(df_est)+1)*stats::qchisq(0.95,df=N.ESTIMATED.PARAMS)/(2*nrow(df_est)),label="Estimated df",hjust=0,fontface="italic",size=5) +
  annotate("text",x = 0,y=2*stats::qchisq(0.95,df=N.ESTIMATED.PARAMS),label=warn,hjust=0,fontface="italic",size=4,color="red") +
  coord_cartesian(ylim=c(0,2*stats::qchisq(0.95,df=N.ESTIMATED.PARAMS))) +
  theme(legend.position="bottom",legend.box="horizontal") + labs(title="All dOFV distributions") +
  guides(colour = guide_legend(order = 1,title.position="top",nrow=1), linetype = guide_legend(order = 2,title.position="top",nrow=1))
# qdOFV_all


#############################################################################################################
### Comparative CI
#############################################################################################################

ci <- ggplot2::ggplot(paramCI_all,aes(x=interaction(TYPE,ITERATION),y=PMED,color=ITERATION)) +  
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

ci_sir <- ci %+% dplyr::filter(paramCI_all, TYPE=="SIR" | (TYPE=="PROPOSAL" & ITERATION=="1"))  +  labs(title="CI of SIR densities over iterations") + theme(legend.position="none") 
# ci_sir

#############################################################################################################
### Covmat visualization
#############################################################################################################

cor.prop  <- plot.cor(cov.proposal,asym.proposal,final_est,title="Proposal")
cor.final <- plot.cor(cov.final,asym.final,final_est,title="SIR")

#############################################################################################################
### Parameter bin plots by iteration
#############################################################################################################

if(rplots.level>1) {
  
  mM_ratio   <-  vector('list', LASTIT)
  m2_maxbin  <-  vector('list', LASTIT)
  all_maxbin <-  vector('list', LASTIT)
  
  for (i in seq(LASTIT)) {
    
    mM_ratio.cur <- ggplot2::ggplot(dplyr::filter(mrawres_bin_ratio,ITERATION==i),aes(x=BIN,y=mM)) +
      geom_ribbon(aes(ymin=(NRESAMP/NSAMP-qnorm(0.975)*sqrt(NRESAMP/NSAMP*(1-NRESAMP/NSAMP)/(NSAMP/N1))),ymax=(NRESAMP/NSAMP+qnorm(0.975)*sqrt(NRESAMP/NSAMP*(1-NRESAMP/NSAMP)/(NSAMP/N1)))),alpha=0.2) +  # stochastic noise
      geom_point() +
      geom_line() +
      geom_hline(aes(yintercept=NRESAMP/NSAMP),linetype=2,color="black") +
      geom_hline(aes(yintercept=0),linetype=1,color="white") + 
      facet_wrap(ITERATION~Parameter) +
      scale_x_continuous(breaks=seq(1,N1)) +
      coord_cartesian(xlim=c(1,N1)) +
      labs(x="Percentile bin of initial samples",y="Proportion resampled",title=paste("Adequacy of proposal density \n Iteration",i))
    mM_ratio[[i]] <- mM_ratio.cur
    
    m2_maxbin.cur  <- ggplot2::ggplot(dplyr::filter(resamp_prop_max,IDBIN==1 & ITERATION==i),aes(x=ORDER,y=m2)) +
      geom_point() +
      geom_line() +
      geom_hline(aes(yintercept=p*M/N2),linetype=2) +
      geom_ribbon(aes(ymin=(p-qnorm(0.975)*se)*(M/N2),ymax=(p+qnorm(0.975)*se)*(M/N2)),alpha=0.2) +
      facet_wrap(ITERATION~Parameter+BIN,scales="free_y",drop=TRUE) +
      scale_x_continuous(breaks=seq(1,N2,by=1)) +
      coord_cartesian(xlim=c(1,N2),ylim=c(0,c(0,unique(dplyr::filter(resamp_prop_max,IDBIN==1 & ITERATION==LASTIT)$NSAMP)/(N1*N2)))) +
      labs(x="Percentile bin of resamples",y="Number of parameters resampled",title=paste("Exhaustion of samples \n Iteration",i)) 
    m2_maxbin[[i]] <- m2_maxbin.cur
    
    all_maxbin.cur  <- ggplot2::ggplot(dplyr::filter(resamp_prop_max,IDBIN==1 & ITERATION==i),aes(x=ORDER,y=m2,group=Parameter)) +
      geom_point() +
      geom_line(alpha=0.2) +
      geom_smooth(aes(group=1),color="blue",size=2,se=FALSE) +
      scale_x_continuous(breaks=seq(1,N2,by=1)) +
      coord_cartesian(xlim=c(1,N2),ylim=c(0,2*unique(dplyr::filter(resamp_prop_max,IDBIN==1 & ITERATION==i)$NRESAMP)/(N1*N2))) +
      labs(x="Percentile bin of resamples",y="Number of parameters resampled",title=paste("Exhaustion of samples \n Iteration",i, ",all parameters")) 
    all_maxbin[[i]] <- all_maxbin.cur
    
  }
  
  #############################################################################################################
  ### Inverse Wishart fit for OMEGAs
  #############################################################################################################
  
  if (N.ESTIMATED.OMEGAS >= 1) { 
    invWish <- ggplot2::ggplot(mdat,aes(x=value)) +
      geom_histogram(aes(y=..density..),fill="lightgrey",color="black") +
      geom_hline(aes(yintercept=0,linetype=LEGEND_NORM)) + # dummy to output df_norm
      geom_density(aes(x=simdat,color=LEGEND_WISH,sep=":"),size=1) +
      facet_wrap(~variable, scales="free") +
      scale_linetype_manual(values=rep(0,N.ESTIMATED.OMEGAS)) +
      scale_color_discrete(drop=F) +
      theme(legend.position="bottom") +
      guides(color=guide_legend(title="Estimated N based on inverse Wishart distribution"),linetype=guide_legend(title="Calculated N based on normal distribution (not plotted)"))
  } # end of estimated.omegas >=1
} # end of r.plots.level>1

#############################################################################################################
### Output plots
#############################################################################################################

if(rplots.level>=1) {
  pdf(file=paste(working.directory,"PsN_plots_base.pdf",sep=""),title=pdf.title,width=20,height=10)  
  print(qdOFV_all)
  print(ci_sir)
  print(cor.prop)
  print(cor.final)
  dev.off()
}

if(rplots.level>1) {
  pdf(file=paste(working.directory,"PsN_plots_extended.pdf",sep=""),title=pdf.title,width=15,height=15)
  print(mM_ratio)
  print(m2_maxbin)
  print(all_maxbin)
  if (N.ESTIMATED.OMEGAS >= 1) { print(invWish) }
  dev.off()
}

### END
#############################################################################################################

