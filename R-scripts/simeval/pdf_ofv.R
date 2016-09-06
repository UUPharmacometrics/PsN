pdf_ofv <- function(raw.results.file,iofv.file,all.iofv.file,n.subjects,
                    samples,ofv_outlier_criteria,ofv.filename,
                    rplots.level,model.filename,n) {
  
  #open PDF
  pdf(file=ofv.filename,width=10,height=7)
  
  #1. draw a histogram of the simulated ofv values (pOFV PPC)
  p_ofv_ppc_data <- p_ofv_ppc(raw.results.file) # calcultation
  
  #histogram
  histogram_p_ofv_ppc <- function(p_ofv_ppc_data,model.filename) {
    #unlist
    pOFV_sim <- p_ofv_ppc_data$pOFV_sim
    pOFV_obs <- p_ofv_ppc_data$pOFV_obs
    newxlim <- p_ofv_ppc_data$newxlim
    #draw a histogram
    hist(pOFV_sim,xlim=newxlim,main=paste('pOFV PPC ',model.filename),xlab="pOFV",freq=TRUE)
    # draw vertical lines
    abline(v=pOFV_obs,col="red",lwd=2, lty=1)
    abline(v=median(pOFV_sim),lwd=2, lty=4, col="green")
    abline(v=quantile(pOFV_sim,c(0.025, 0.975), na.rm=T)[1],lwd=2, lty=3, col="green")
    abline(v=quantile(pOFV_sim, c(0.025, 0.975), na.rm=T)[2],lwd=2, lty=3, col="green")
    leg.txt <- c("pOFV","median pOFVsim","5th and 95th pOFVsim")
    legend("topright", col=c('red', 'green', 'green','green'), leg.txt, lty=c(1,4,3,3),box.lwd = 0,box.col = "white",bg = "white", lwd=2, cex=1)
  }
  histogram_p_ofv_ppc(p_ofv_ppc_data,model.filename)
  
  #2. draw a histogram of (iOFV NPDE)
  i_ofv_npde_data <- i_ofv_npde(iofv.file)# calculation
  
  histogram_i_ofv_npde <- function(i_ofv_npde_data) {
    #unlist
    iOFV_npde <- i_ofv_npde_data$iOFV_npde
    ylimit <- i_ofv_npde_data$ylimit
    xlimit_min <- i_ofv_npde_data$xlimit_min
    xlimit_max <- i_ofv_npde_data$xlimit_max
    x <- i_ofv_npde_data$x
    dy <- i_ofv_npde_data$dy
    #draw a histogram
    hist(iOFV_npde,xlab="iOFV NPDE",ylim=c(0,ylimit),xlim=c(xlimit_min,xlimit_max))
    lines(x,dy, col="red")
  }
  histogram_i_ofv_npde(i_ofv_npde_data)
  
  #3. iOFV RES
  list_i_ofv_res <- i_ofv_res(all.iofv.file,n.subjects,samples,ofv_outlier_criteria)# calculation
  
  # Make a boxplot
  boxplot_i_ofv_res <- function(list_i_ofv_res,n.subjects,ofv_outlier_criteria) {
    # unlist
    iOFV_res_ord <- list_i_ofv_res$iOFV_res_ord
    id_sorted <- list_i_ofv_res$id_sorted
    result <- list_i_ofv_res$result
    vector_text <- list_i_ofv_res$vector_text
    
    # From which position sorted res medians starts crossing zero line 
    #(need for drawing vertical line in the plot)
    flag <- 0
    cross <- 0
    for (i in 1:n.subjects) {
      if(flag == 0){
        if(result$x[i] > 0){
          cross <- i
          flag <- 1
        }
      } 
    }
    
    #make a plot
    boxplot(iOFV_res_ord, outline=FALSE, range=0.001,names=as.character(id_sorted),xlab="ID",ylab="iOFV RES")
    #this are indices in plotted sorted arr which are outside lim. always the last few, if any
    if (any(vector_text != "")) {
      text(1:n.subjects,result$x-1, paste("", vector_text),col="red") # mark outliers
    } 
    abline(h=0, lwd=2, lty=3, col="black")
    abline(h=-ofv_outlier_criteria, lwd=2, lty=3, col="black") # mark outlier line
    abline(h=ofv_outlier_criteria, lwd=2, lty=3, col="black") # mark outlier line
    abline(v=n.subjects/2, lwd=2, lty=1, col="green") #theoretical crossing zero line
    abline(v=cross, lwd=2, lty=1, col="red") # real crossing zero line
    leg.txt <- c("theoretical crossing zero line","crossing zero line")
    legend("topleft", col=c('green','red'), leg.txt, lty=c(1,1),box.lwd = 0,bg = "white", lwd=2, cex=1)
    title("iOFV RES")
  }
  boxplot_i_ofv_res(list_i_ofv_res,n.subjects,ofv_outlier_criteria)
  
  #4. iOFV PPC
  outlier_ID <- list_i_ofv_res$outlier_ID
  list_i_ofv_ppc <- i_ofv_ppc(all.iofv.file,samples,outlier_ID)# calculation
  
  histograms_i_ofv_ppc <- function(list_i_ofv_ppc,model.filename) {
    amout <- length(list_i_ofv_ppc)
    if(amout > 0) {
      for (i in 1:(amout-2)) {
        #unlist
        iOFV_sim <- list_i_ofv_ppc[[i]]$iOFV_sim
        len <- length(iOFV_sim)
        iOFV_obs <- list_i_ofv_ppc[[i]]$iOFV_obs
        sort_iOFV_sim <- list_i_ofv_ppc[[i]]$sort_iOFV_sim
        newxlim <- list_i_ofv_ppc[[i]]$newxlim
        outlier_data <- list_i_ofv_ppc$outlier_data
        # make a plot
        hist(sort_iOFV_sim,xlim=newxlim,axes=TRUE,main=paste('iOFV PPC ',model.filename,'ID =', outlier_data$ID[i]),xlab="iOFV",freq=TRUE)
        abline(v=iOFV_obs, lwd= 2, lty=1, col="red") 
        abline(v=median(iOFV_sim[1:len]), lwd=2, lty=4, col="green") 
        abline(v=quantile(iOFV_sim[1:len], c(0.025, 0.975), na.rm=T)[1], lwd=2, lty=3, col="green") 
        abline(v=quantile(iOFV_sim[1:len], c(0.025, 0.975), na.rm=T)[2], lwd=2, lty=3, col="green") 
        leg.txt <- c("iOFVobs","median iOFVsim","5th and 95th iOFVsim")
        legend("topright", col=c('red', 'green', 'green','green'), leg.txt, lty=c(1,4,3,3),box.lwd = 0,box.col = "white",bg = "white", lwd=2, cex=1)
      }
    }
    
  }
  histograms_i_ofv_ppc(list_i_ofv_ppc,model.filename)
  
  #5. KLD iOFV
  if (rplots.level > 1) {
    list_kld_i_ofv <- kld_i_ofv(all.iofv.file,n.subjects,samples,n)# calculation
  
    histogram_kld_i_ofv <- function(list_kld_i_ofv,model.filename) {
      # unlist needed data
      KLD_sim <- list_kld_i_ofv$KLD_sim
      newxlim <- list_kld_i_ofv$newxlim
      KLD_obs <- list_kld_i_ofv$KLD_obs
      # Draw a histogram
      hist(KLD_sim,xlim=newxlim,main=paste('KLD iOFV ',model.filename),xlab="KLD",freq=TRUE)
      abline(v=KLD_obs,col="red",lwd=2, lty=1)
      abline(v=median(KLD_sim),lwd=2, lty=4, col="green")
      abline(v=quantile(KLD_sim,c(0.025, 0.975), na.rm=T)[1],lwd=2, lty=3, col="green")
      abline(v=quantile(KLD_sim, c(0.025, 0.975), na.rm=T)[2],lwd=2, lty=3, col="green")
      leg.txt <- c("KLDobs","median KLDsim","5th and 95th KLDsim")
      legend("topright", col=c('red', 'green', 'green','green'), leg.txt, lty=c(1,4,3,3),
             box.lwd = 0,box.col = "white",bg = "white", lwd=2, cex=1)
    }
    histogram_kld_i_ofv(list_kld_i_ofv,model.filename)# histogram
  }
  # close PDF
  dev.off()
}