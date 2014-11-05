if (rplots.level > 0){
    pdf(file=pdf.filename,width=10,height=7,title=pdf.title)
}


if (rplots.level > 0){
    
    if (have.base.model){
    dfRange <- seq(1,2.5,0.001)
    
      objdf <- read.csv(raw.results.file)
      objdf <- subset(objdf, select=c(model,ofv,deltaofv))
      names(objdf) <- c("model","OFV","dOFV")
      bofv   <- objdf$OFV[1]    # OFV of base model
      iofv   <- objdf$OFV[2]    # OFV of full model
      delofv <- iofv-bofv       # True DOFV
      objdf  <- objdf[-c(1:2),]     # Delete first two rows
      ndata  <- length(objdf$dOFV)
      dof    <- dfRange
      nchi   <- 10000
      
      # Create CDF and perform K-S test to determine the correct DoF
      for (i in 1:length(dof)){
        if (i == 1){
          cmdf   <- summarize(objdf, value=unique(sort(-1*dOFV)), "|dOFV|"=ecdf(-1*dOFV)(value), chisq=ecdf(rchisq(nchi,df=dof[i]))(value))
          ksDist <- ks.test(cmdf[,2], cmdf$chisq)[[1]]
          pval   <- ks.test(cmdf[,2], cmdf$chisq)[[2]]
          df     <- dof[i]
        }else{
          tcmdf    <- summarize(objdf, value=unique(sort(-1*dOFV)), "|dOFV|"=ecdf(-1*dOFV)(value), chisq=ecdf(rchisq(nchi,df=dof[i]))(value))
          tksDist <- ks.test(tcmdf[,2], tcmdf$chisq)[[1]]
          tpval   <- ks.test(tcmdf[,2], tcmdf$chisq)[[2]]
          if (tksDist < ksDist){
            cmdf <- tcmdf; ksDist <- tksDist; pval <- tpval; df <- dof[i]
          }
        }
      }
      rm(tcmdf)
      
      df    <- signif(df,3); pval <- signif(pval,2); ksDist <- signif(ksDist,2); ksCrit <- signif(1.358/sqrt(ndata), 2)
      ofvqn <- unname(quantile(-1*objdf$dOFV, 0.95))
      chsqn <- unname(quantile(rchisq(nchi, df), 0.95))
      vdf   <- data.frame(chsqn,ofvqn,delofv=-1*delofv); vdf <- melt(vdf)
      
      cmdf  <- melt(cmdf,id=c("value"),c("|dOFV|","chisq"),value.name="CDF")
    # CDF
      ggplt <- ggplot(cmdf, aes(value,CDF,color=variable)) +
        labs(title=bquote("DOF"[chi^2]~"="~.(df)~", D"[KS]~"="~.(ksDist)~", D"[critical]~"="~.(ksCrit)~", p-value="~.(pval))) +
        xlab("\nValue") + ylab("CDF\n") +
        theme(plot.title = element_text(size=20, face="bold"),
              axis.title.x = element_text(size=18,face="bold"),
              axis.title.y = element_text(size=18,face="bold"),
              axis.text.x  = element_text(size=16,face="bold",color="black",angle=90),
              axis.text.y  = element_text(size=16,face="bold",color="black",hjust=0),
              legend.text  = element_text(size=18,face="bold"),
              legend.title = element_text(size=18,face="bold")) +
        geom_vline(data=vdf,aes(xintercept=value, color=variable, lty=variable), size=2) +
        geom_point(size=3)
      print(ggplt)
      
      # Compare PDF based on previous K-S test
      bin   <- hist(-1*objdf$dOFV, plot=F)$breaks
      dofv  <- hist(-1*objdf$dOFV, plot=F)$density/sum(hist(-1*objdf$dOFV, plot=F)$density)
      imdf  <- data.frame(bin=numeric(0),dOFV=numeric(0))
      for (i in 1:length(dofv)){imdf <- rbind(imdf, data.frame(bin=mean(bin[i:(i+1)]),dOFV=dofv[i]))}
      pmdf  <- cbind(imdf, chisq=dchisq(imdf$bin,df))
      colnames(pmdf)[2] <- "|dOFV|"
      pmdf  <- melt(pmdf,id=c("bin"),c("|dOFV|","chisq"),value.name="PDF")
      
      # Plot fitted PDF and CDF data
    #PDF
      ggplt <-ggplot(pmdf, aes(bin,PDF,color=variable)) +
        geom_line(size=2) +
        labs(title=bquote("DOF"[chi^2]~"="~.(df)~", K-S score (D="~.(ksDist)~"), p-value="~.(pval)~"\n")) +
        xlab("\nValue") + ylab("PDF\n") +
        theme(plot.title = element_text(size=20, face="bold"),
              axis.title.x = element_text(size=18,face="bold"),
              axis.title.y = element_text(size=18,face="bold"),
              axis.text.x  = element_text(size=16,face="bold",color="black",angle=90),
              axis.text.y  = element_text(size=16,face="bold",color="black",hjust=0),
              legend.text  = element_text(size=18,face="bold"),
              legend.title = element_blank()) +
        geom_vline(xintercept=chsqn, color="skyblue", size=2, lty=2) +
        geom_vline(xintercept=ofvqn, color="red", size=2, lty=2) +
        geom_vline(xintercept=-1*delofv, color="red", size=2, lty=1)
      print(ggplt)
      
      # Check true randomization frequency
    
      df <- read.table(data.diff.table,header = F, sep=" ")
    #randFrq
      ggplt <- ggplot(df, aes(V1)) + geom_histogram(fill="white",color="black") +
        xlab("\nNo. of individuals with randomized dose") + ylab("Count\n") +
        theme(plot.title = element_text(size=20, face="bold"),
              axis.title.x = element_text(size=18,face="bold"),
              axis.title.y = element_text(size=18,face="bold"),
              axis.text.x  = element_text(size=16,face="bold",color="black",angle=90),
              axis.text.y  = element_text(size=16,face="bold",color="black",hjust=0))
      print(ggplt)
    
    
    
    }
}
 
