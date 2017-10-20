# get libPaths
source(file.path(rscripts.directory,"common/R_info.R"))
R_info(directory=working.directory,only_libPaths=T)
library(ggplot2)
#add R_info to the meta file
R_info(directory=working.directory)

    pdf(file=pdf.filename,width=10,height=7,title=pdf.title)

    
    rawres <- read.csv(tool.results.file, as.is=T)
    
    
    # Appearance for default plot + extended plot1
    PLoT1 = ggplot()+
      geom_hline(yintercept=80, linetype=2, colour='red', size=0.7)+
      scale_y_continuous(limits=c(25,100),breaks=c(25,50,75,100))+
      xlab('Sample size (N)')+
      ylab('Power (%)')+
      ggtitle('MCMP-based power curve')+
      theme(legend.position = "none",
            title=element_text(size=20, colour='black', vjust=1.2),
            axis.text.x=element_text(size=17,colour="black"),
            axis.text.y=element_text(size=15,colour="black"),
            axis.title.y=element_text(size=20, vjust=0.4),
            axis.title.x=element_text(size=20, vjust=0.3),
            axis.line=element_line(colour="black"),
            panel.background=element_rect(color="black",fill="gray100"),
            panel.grid.minor=element_line(color="gray95"),
            panel.grid.major=element_line(color="gray95"))
    
    # Appearance for the extended plot2
    PLoT2 = ggplot()+
      xlab(expression(Delta * 'iOFV'))+
      ylab('Number of individuals')+
      ggtitle(expression('Distribution of ' * Delta * 'iOFV'))+
      theme(legend.position = "none",
            title=element_text(size=20, colour='black', vjust=1.2),
            axis.text.x=element_text(size=17,colour="black"),
            axis.text.y=element_text(size=15,colour="black"),
            axis.title.y=element_text(size=20, vjust=0.4),
            axis.title.x=element_text(size=20, vjust=0.3),
            axis.line=element_line(colour="black"),
            panel.background=element_rect(color="black",fill="gray100"),
            panel.grid.minor=element_line(color="gray95"),
            panel.grid.major=element_line(color="gray95"))
    
         

    if(sig.level %in% c(0.1,1,5,10,15,20)){
      column_name <- sprintf("power.at.%i.", sig.level)
      vline <-  data.frame(x=rawres$total_X[which(rawres[,column_name]>=80)[1]], 
                         xend=rawres$total_X[which(rawres[,column_name]>=80)[1]],
                         y=80, yend=25)
      default_plot <- PLoT1+
        geom_point(data=rawres, 
                   aes_string("total_X", column_name), 
                   size=2.2)+
        geom_segment(data=vline, aes(x=x, xend=xend, y=y, yend=yend), 
                     linetype=2, colour='red', size=0.7)
      print(default_plot)
    }
    
    if (rplots.level > 1){
      
      # for rplots>1 a PPE based curve will be included in the output
      
      #parametric power estimation function
      alpha <- sig.level/100
      ppe <- function(dofvs, df=1){
        dofvs <- dofvs[dofvs>0]
        opt.fun<-function(ncp)-sum(dchisq(dofvs,df,ncp,log=T))
        init <- mean(dofvs)-df
        fit <- optim(par=init, fn=opt.fun, lower=0, method="L-BFGS-B")
        ncp <- fit$par
        power <- pchisq(qchisq(1-alpha,df=df,ncp=0),df=df,ncp=ncp,lower.tail=F)
        return(list(power=power, ncp=ncp))
      }
      
      ppe_subjects <- function(dofvs, ncp=NULL, df=1, n.subjects, pred.n.subjects){
        if(is.null(ncp)){
          ppe.fit <- ppe(dofvs, df)
          ncp <- ppe.fit$ncp
        }
        ncps <- ncp/n.subjects*pred.n.subjects  
        power <- pchisq(qchisq(1-alpha,df=df,ncp=0),df=df,ncp=ncps,lower.tail=F)
        return(power)
      }
      
      
      if(!is.null(degrees.of.freedom) ||!is.null(n.individuals)){
        ncp <- ofv.reduced-ofv.full
        ppe_power <- ppe_subjects(ncp = ncp, df=1, n.subjects = n.individuals, pred.n.subjects = rawres$total_X)
        ppe_power_curves <- data.frame(subjects=rawres$total_X, power=ppe_power*100, method="PPE")
        p <- PLoT1+
          geom_point(data=transform(rawres, method="MCMP"), 
                     aes_string("total_X", column_name, color="method"), 
                     size=2.2) + 
          geom_line(data=ppe_power_curves, 
                    aes(subjects, power, color=method), 
                    size=1.2)+
          scale_color_manual("Method", values = c(MCMP="black", PPE="darkblue"))+
          ggtitle("MCMP- versus PPE-based power curve")+
          theme(legend.position=c(1,0), legend.justification=c(1,0),
                legend.text=element_text(size=12))
        print(p) 
      }
      
      
      # Rearrange the rawres file (without having to use the package reshape2)
      Power       = c(rawres$power.at.20.,rawres$power.at.15.,rawres$power.at.10.,rawres$power.at.5., rawres$power.at.1., rawres$power.at.0.1.)
      Sample_size = rep(rawres$total_X,6) #3
      Type        = c(rep('20 %',length(rawres[,1])),rep('15 %',length(rawres[,1])),rep('10 %',length(rawres[,1])),rep('5 %',length(rawres[,1])),rep('1 %',length(rawres[,1])),rep('0.1 %',length(rawres[,1])))
      New.rawres  = data.frame(Sample_size, Power, Type)
      
      # Define the smallest sample size to reach >= 80 % power with for each significance level (and put the info in a dataframe, for plotting reasons)
      vline2 = data.frame(x=c(rawres$total_X[which(rawres$power.at.20.>=80)[1]],rawres$total_X[which(rawres$power.at.15.>=80)[1]],rawres$total_X[which(rawres$power.at.10.>=80)[1]],rawres$total_X[which(rawres$power.at.5.>=80)[1]], rawres$total_X[which(rawres$power.at.1.>=80)[1]], rawres$total_X[which(rawres$power.at.0.1.>=80)[1]]), 
                          xend=c(rawres$total_X[which(rawres$power.at.20.>=80)[1]],rawres$total_X[which(rawres$power.at.15.>=80)[1]],rawres$total_X[which(rawres$power.at.10.>=80)[1]],rawres$total_X[which(rawres$power.at.5.>=80)[1]], rawres$total_X[which(rawres$power.at.1.>=80)[1]], rawres$total_X[which(rawres$power.at.0.1.>=80)[1]]),
                          y=80, yend=25, Type=c('20 %','15 %','10 %','5 %', '1 %', '0.1%'))
      
      # Extended plot1
      print(PLoT1+geom_point(data=New.rawres, aes(x=Sample_size, y=Power, group=Type, shape=Type), size=2.2)+
              scale_shape_manual(name='Significance \n level', values=c(4,6,7,10,15,19))+
              geom_segment(data=vline2, aes(x=x, xend=xend, y=y, yend=yend), linetype=2, colour='red', size=0.7)+
              theme(legend.position=c(0.9,0.20),
                    legend.background=element_rect(fill="white", colour="black"), 
                    legend.key=element_rect(fill='white'),
                    legend.text=element_text(size=12)))
      
      # Read in the individual OFV info from full and reduced run
      Full.ofv = read.table('m1/full.phi', skip=1, header=T)
      Red.ofv  = read.table('m1/reduced.phi', skip=1, header=T)
      
      # Create a dataframe with delta iofv
      d.iofv = Full.ofv$OBJ-Red.ofv$OBJ
      Data.diofv = data.frame(ID=Full.ofv$ID, DIOFV=d.iofv)
      
      # Extended plot2
      print(PLoT2+geom_bar(data=Data.diofv, aes(x=DIOFV), colour='grey40'))
      
    }
    

    dev.off()


