library(PsNR)
library(magrittr)
library(methods)
library(ggplot2)
library(dplyr)

#add R_info to the meta file
R_info(directory=working.directory)

meta <- PsNR::metadata(working.directory)
model_path <- PsNR::model_path(meta)

pdf(file=pdf.filename, title="Log-likelihood profiling")

    RUN <- paste0(PsNR::model_prefix(meta), xpose.runno)
    theme_set(theme_bw(base_size = 18))
    
    CL <- round(pchisq(3.84,df=1),2)                                # confidence level corresponding to chosen ofv_increase
    
    ## Read llp result files
    
    llp.data           <- read.csv("unstacked_llplog.csv")                               # all iterations                               
    llp.data$Parameter <- gsub(" ","", llp.data$Parameter , fixed=TRUE)                  # remove space
    llp.data$Parameter <- factor(llp.data$Parameter, levels=unique(llp.data$Parameter))  # order: theta-omega-sigma (default: alphabetical)
    llp.data$Side      <- gsub(" ","", llp.data$Side , fixed=TRUE)                 
    
    llp.res            <- read.csv(tool.results.file,skip=1,header=TRUE)                 # summary results
    llp.res            <- llp.res[,-ncol(llp.res)]
    names(llp.res)     <- c("Parameter","lower","ML.estimate","upper","interval.ratio","near.bound","max.iterations")
    
    llp.res$Parameter <- gsub(" ", "", llp.res$Parameter, fixed=TRUE)
    llp.res$Parameter <- factor(llp.res$Parameter, levels=levels(llp.data$Parameter))
    llp.all            <- dplyr::full_join(llp.data,llp.res[,c("Parameter","lower","upper","ML.estimate","interval.ratio")]) # Combine both results files
    llp.all$strip.text <- factor(paste(llp.all$Parameter," (IR=",round(llp.all$interval.ratio,1),")",sep=""),levels=paste(levels(llp.all$Parameter)," (IR=",round(unique(llp.all$interval.ratio),1),")",sep="")) 
    
    ## Identify first guess of CI
    
    nparam         <- length(levels(llp.all$Parameter))
    llp.first      <- llp.data[seq((nparam+1),3*nparam),]                                   # first iterations use MLE +/- NORMQ*SE 
    llp.first.wide <- reshape(llp.first[,-grep("OFV",names(llp.first))],v.names="Value",timevar="Side",idvar="Parameter",direction="wide")
    
    llp.all        <- dplyr::full_join(llp.all, llp.first.wide)
    
    ## Calculate smooth to link llp points together 
    
    poly3  <- function (dat) lm(OFV.diff~Value+I(Value^2)+I(Value^3),data=dat)             # 3rd order polynomial
    poly4  <- function (dat) lm(OFV.diff~Value+I(Value^2)+I(Value^3)+I(Value^4),data=dat)  # 4th order polynomial
    
    llp.pred3 <- c()                                                                      # will contain predictions for polynomials
    llp.pred4 <- c()
      
    for (i in levels(llp.all$Parameter)) {
      
      dat       <- llp.all[llp.all$Parameter==i,]                                         # dataset for one parameter
      range     <- seq(min(dat$Value),max(dat$Value),length=1000)                         # select 1000 equally sized points between min and max value of parameter
      
      fit_poly3 <- poly3(dat)                                                             # 3rd order polynomial fit
      pred_poly3<- predict( fit_poly3,newdata=data.frame("Value"=range))                  # discrete predictions over investigated parameter range 
      llp.pred3 <- rbind(llp.pred3,data.frame("Parameter"=i,"Value"=range,"OFV.diff"=pred_poly3))
      
      fit_poly4 <- poly4(dat)                                                             # same with 4rth order polynomial fit
      pred_poly4<- predict( fit_poly4,newdata=data.frame("Value"=range))                  
      llp.pred4 <- rbind(llp.pred4,data.frame("Parameter"=i,"Value"=range,"OFV.diff"=pred_poly4))
    }
      
    ## Plot 
    
    p <- ggplot(llp.all,aes(x=Value,y=OFV.diff)) +
      geom_point(size=3) +                                                                # all points tried by LLP
      geom_line() +                                                                       # connect points with line
      geom_point(data=llp.all[llp.all$Side=="orig",],color="red",size=3) +                # ML estimate in red  for LLP CI 
      geom_point(data=llp.all[llp.all$Side=="orig",],aes(y=-0.5),color="blue",size=3) +   # ML estimate in blue for initial CI  
      geom_vline(aes(xintercept=c(lower)),linetype=2,color="darkgrey") +            # gridlines for CI bounds        
      geom_vline(aes(xintercept=c(upper)),linetype=2,color="darkgrey") +
      geom_hline(yintercept=refofv,linetype=2,color="darkgrey") +                         # gridline for refofv
      geom_text(aes(x=lower,y=refofv,label=signif(lower,3)),hjust=-0.5,vjust=0) +          # paste value of lower bound
      geom_text(aes(x=upper,y=refofv,label=signif(upper,3)),hjust=1.25,vjust=0) +          # paste value of upper bound
      geom_text(aes(x=ML.estimate,y=1,label=signif(ML.estimate,3))) +                      # paste value of ML estimate
      geom_errorbarh(aes(xmin=lower,xmax=upper,y=0),color="red") +                        # parameter CI projected on x-axis
      geom_errorbarh(aes(xmin=Value.lower,xmax=Value.upper,y=-0.5),color="blue") +        # initial guess at parameter CI projected on x-axis
      #   geom_line(data=llp.pred3) +                                                       # lm predictions with 3rd order polynomial
      #   geom_line(data=llp.pred4) +                                                       # lm predictions with 4rth order polynomial
      #   geom_smooth(se=FALSE) +                                                           # smooth
      scale_y_continuous(limits=c(-1,round(refofv*2,0))) +
      facet_wrap(~strip.text,scales="free",ncol=2) +
      labs(x="Parameter value",y="dOFV",title=paste("Log-Likelihood Profiling\n(",RUN,")",sep=""))
    print(p)
    
    
    
dev.off()

