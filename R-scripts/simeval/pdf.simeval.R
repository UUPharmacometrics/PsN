pdf.simeval <- function(ebe.npde.file,iiv.eta.names,iov.eta.names,outlying_criteria,
                        residual.files,residual.outliers.file,residual.names,
                        raw.results.file,iofv.file,all.iofv.file,n.subjects,samples,
                        model.filename,rplots.level,pdf_filename,n) {
  
  # open pdf file
  pdf(file=pdf_filename,width=10,height=7)
  
  ###########################################   OFV PLOTS   ###############################################
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
    leg.txt <- c("pOFV","median pOFVsim","2.5th and 97.5th pOFVsim")
    legend("topright", col=c('red', 'green', 'green','green'), leg.txt, lty=c(1,4,3,3),box.lwd = 0,box.col = "white",bg = "white", lwd=2, cex=1)
  }
  histogram_p_ofv_ppc(p_ofv_ppc_data,model.filename)
  
  #2. draw a histogram of (iOFV NPDE)
  if (rplots.level > 1) {
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
  }
  
  #3. iOFV NPDE summary table
  iofv_summary_table <- summary.table.ofv(iofv.file)
  
  # plot summary table
  plot.table(iofv_summary_table)
  
  #3. iOFV RES
  list_i_ofv_res <- i_ofv_res(all.iofv.file,n.subjects,samples)# calculation
  # for all outlier table
  ofv_outliers <- list_i_ofv_res$ofv_outliertable
  n.subjects <- list_i_ofv_res$n.subjects
  ID_deleted_ofv <- list_i_ofv_res$ID_deleted_ofv
  
  # Make a boxplot
  boxplot_i_ofv_res <- function(list_i_ofv_res,n.subjects) {
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
    abline(h=-3, lwd=2, lty=3, col="black") # mark outlier line
    abline(h=3, lwd=2, lty=3, col="black") # mark outlier line
    title("iOFV RES")
  }
  boxplot_i_ofv_res(list_i_ofv_res,n.subjects)
  
  if (rplots.level > 1) {
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
          abline(v=quantile(iOFV_sim[1:len], c(0.001, 0.999), na.rm=T)[1], lwd=2, lty=3, col="green") 
          abline(v=quantile(iOFV_sim[1:len], c(0.001, 0.999), na.rm=T)[2], lwd=2, lty=3, col="green") 
          leg.txt <- c("iOFVobs","median iOFVsim","0.1th and 99.9th iOFVsim")
          legend("topright", col=c('red', 'green', 'green','green'), leg.txt, lty=c(1,4,3,3),box.lwd = 0,box.col = "white",bg = "white", lwd=2, cex=1)
        }
      }
      
    }
    histograms_i_ofv_ppc(list_i_ofv_ppc,model.filename)
    
    #5. KLD iOFV
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
      leg.txt <- c("KLDobs","median KLDsim","2.5th and 97.5th KLDsim")
      legend("topright", col=c('red', 'green', 'green','green'), leg.txt, lty=c(1,4,3,3),
             box.lwd = 0,box.col = "white",bg = "white", lwd=2, cex=1)
    }
    histogram_kld_i_ofv(list_kld_i_ofv,model.filename)# histogram
  }
  
  ###########################################   EBE.NPDE PLOTS  ################################################
  ebe_list <- ebe.npde.all(ebe.npde.file,iiv.eta.names,iov.eta.names,outlying_criteria,rplots.level,model.filename,make_plot=TRUE)
  
  ebe.npde_outliers <- ebe_list$ebe.npde_outliers
  ID_deleted_ebe <- ebe_list$ID_deleted_ebe
  
  #######################################     RESIDUALS PLOTS    #############################################
  if (file.exists(residual.files[1]) && file.exists(residual.files[2]) && file.exists(residual.outliers.file)) {
    #------------------------------------------(1)histograms-----------------------------------------
    # npde (for each data frame plot a histogram)
    histograms.cwres.iwres(residual.files,residual.names)
    
    #------------------------------------------(2)summary_table plot----------------------------------
    # Summary table
    mydataframe <- summary.table(residual.files,residual.names)
    
    # plot summary table "mydataframe" on the new page
    plot.table(mydataframe)
    
    #------------------------------------------(3)outliertable plot----------------------------------
    # Use outliertable function to plot outlier tabel
    list <- outlier.table(residual.outliers.file)
    outlierframe <- list$outlierframe
    cwres.iwres_outliers <- list$outliers_count
    
    # Plot created outlier table on the next page
    plot.table(outlierframe)
    
    ########################################   ALL OUTLIERS REPORT TABLE   ##################################################
    all_outlier_table <- all.outlier.report.table(ofv_outliers,ebe.npde_outliers=ebe.npde_outliers,cwres.iwres_outliers,
                                                  ID_deleted_ebe,ID_deleted_ofv)
    
    # draw the table 
    plot.all.outlier.table(all_outlier_table)
  } else {
    ########################################   ALL OUTLIERS REPORT TABLE   ##################################################
    all_outlier_table <- all.outlier.report.table(ofv_outliers,ebe.npde_outliers=ebe.npde_outliers,
                                                  ID_deleted_ebe=ID_deleted_ebe,ID_deleted_ofv=ID_deleted_ofv)
    
    # draw the table 
    plot.table(all_outlier_table)
  }
  
  dev.off()
}