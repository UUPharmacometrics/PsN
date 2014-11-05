if (rplots.level > 0){
    pdf(file=pdf.filename,width=10,height=7,title=pdf.title)
}

if (rplots.level > 0){
    
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
    
         
    # If significance level 5 was used
    if(sig.level==5){
        # Define the smallest sample size to reach >= 80 % power with (and put the info in a dataframe, for plotting reasons)
        vline = data.frame(x=rawres$total_X[which(rawres$power.at.5.>=80)[1]], 
                           xend=rawres$total_X[which(rawres$power.at.5.>=80)[1]],
                           y=80, yend=25)
        
        # Default plot
        print(PLoT1+geom_point(data=rawres, aes(x=total_X, y=power.at.5.), size=2.2)+
            geom_segment(data=vline, aes(x=x, xend=xend, y=y, yend=yend), linetype=2, colour='red', size=0.7))
    }
    # If significance level 1 was used
    if(sig.level==1){
        # Define the smallest sample size to reach >= 80 % power with (and put the info in a dataframe, for plotting reasons)
        vline = data.frame(x=rawres$total_X[which(rawres$power.at.1.>=80)[1]], 
                           xend=rawres$total_X[which(rawres$power.at.1.>=80)[1]],
                           y=80, yend=25)
        
        # Default plot
        print(PLoT1+geom_point(data=rawres, aes(x=total_X, y=power.at.1.))+
          geom_segment(data=vline, aes(x=x, xend=xend, y=y, yend=yend), linetype=2, colour='red', size=0.7) )
    }
    # If significance level 0.1 was used
    if(sig.level==0.1){
        # Define the smallest sample size to reach >= 80 % power with (and put the info in a dataframe, for plotting reasons)
        vline = data.frame(x=rawres$total_X[which(rawres$power.at.0.1.>=80)[1]], 
                           xend=rawres$total_X[which(rawres$power.at.0.1.>=80)[1]],
                           y=80, yend=25)
        
        # Default plot
        print(PLoT1+geom_point(data=rawres, aes(x=total_X, y=power.at.0.1.)) +
          geom_segment(data=vline, aes(x=x, xend=xend, y=y, yend=yend), linetype=2, colour='red', size=0.7))
    }
    
    
    if (rplots.level > 1){
      
      
      # Rearrange the rawres file (without having to use the package reshape2)
      Power       = c(rawres$power.at.5., rawres$power.at.1., rawres$power.at.0.1.)
      Sample_size = rep(rawres$total_X,3)
      Type        = c(rep('5 %',length(rawres[,1])),rep('1 %',length(rawres[,1])),rep('0.1 %',length(rawres[,1])))
      New.rawres  = data.frame(Sample_size, Power, Type)
      
      # Define the smallest sample size to reach >= 80 % power with for each significance level (and put the info in a dataframe, for plotting reasons)
      vline2 = data.frame(x=c(rawres$total_X[which(rawres$power.at.5.>=80)[1]], rawres$total_X[which(rawres$power.at.1.>=80)[1]], rawres$total_X[which(rawres$power.at.0.1.>=80)[1]]), 
                          xend=c(rawres$total_X[which(rawres$power.at.5.>=80)[1]], rawres$total_X[which(rawres$power.at.1.>=80)[1]], rawres$total_X[which(rawres$power.at.0.1.>=80)[1]]),
                          y=80, yend=25, Type=c('5 %', '1 %', '0.1%'))
      
      # Extended plot1
      print(PLoT1+geom_point(data=New.rawres, aes(x=Sample_size, y=Power, group=Type, shape=Type), size=2.2)+
              scale_shape_manual(name='Significance \n level', values=c(4,6,19))+
              geom_segment(data=vline2, aes(x=x, xend=xend, y=y, yend=yend), linetype=2, colour='red', size=0.7)+
              theme(legend.position=c(0.9,0.15),
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
    
}
