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
    leg.txt <- c("pOFV","median pOFVsim","5th and 95th pOFVsim")
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
  
  #3. iOFV RES
  list_i_ofv_res <- i_ofv_res(all.iofv.file,n.subjects,samples)# calculation
  # for all outlier table
  ofv_outliers <- list_i_ofv_res$ofv_outliertable 
  
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
          abline(v=quantile(iOFV_sim[1:len], c(0.025, 0.975), na.rm=T)[1], lwd=2, lty=3, col="green") 
          abline(v=quantile(iOFV_sim[1:len], c(0.025, 0.975), na.rm=T)[2], lwd=2, lty=3, col="green") 
          leg.txt <- c("iOFVobs","median iOFVsim","5th and 95th iOFVsim")
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
      leg.txt <- c("KLDobs","median KLDsim","5th and 95th KLDsim")
      legend("topright", col=c('red', 'green', 'green','green'), leg.txt, lty=c(1,4,3,3),
             box.lwd = 0,box.col = "white",bg = "white", lwd=2, cex=1)
    }
    histogram_kld_i_ofv(list_kld_i_ofv,model.filename)# histogram
  }
  
  ###########################################   EBE.NPDE PLOTS  ################################################
  # default for the do_outlier_plot
  if(rplots.level > 1) {
    do_outlier_plot <- TRUE
  } else {
    do_outlier_plot <- FALSE
  }
  
  #iiv and iov etas
  list_eta_names <- list(c(iiv.eta.names))
  if(length(iov.eta.names) > 0) {
    explanation_text <- c("IIV",rep("IOV",length(iov.eta.names)))
    for(i in 1:length(iov.eta.names)) {
      list_eta_names[[i+1]] <- iov.eta.names[[i]]
    }
  }

  # plot explanation text (if they are iiv etas or iov etas and which etas)
  # loop over iiv and iov etas, create same plots
  for (i in 1:length(list_eta_names)) {
    eta_names <- list_eta_names[[i]]
    if(exists("explanation_text")) {
      eta_names <- list_eta_names[[i]]
      eta_names_text <- c()
      for (n in 1:length(eta_names)) {
        if(n == 1) {
          eta_names_text <- eta_names[n]
        } else {
          eta_names_text <- paste0( eta_names_text,", ",eta_names[n])
        }
      }
      plot_explanation <- data.frame(C = paste0(explanation_text[i],":  (",eta_names_text,")"))
      names(plot_explanation) <- NULL
      # draw a table
      plot.table(plot_explanation)
    }
    
    
    #.........................................(1)input_ebe_npde...........................  
    input_data <- input.data(ebe.npde.file,iiv.eta.names=eta_names)
    
    ebenpde_tmp <- input_data$ebenpde_tmp
    n.subjects <- input_data$n.subjects
    ebenpde_obs <- input_data$ebenpde_obs
    eta_names <- input_data$iiv.eta.names
    ID_deleted <- input_data$ID_deleted
    
    #.........................................(2)ebe_npde_summary......................... 
    # create EBE npde summary statistics table
    mydataframe <- summary.table.ebe.npde(ebenpde_obs,iiv.eta.names=eta_names)
    # draw a table
    plot.table(mydataframe)
    
    # ..........................................EBE-NPDE correlation graph.............................................  
    # create EBE-NPDE correlation graph
    if(nrow(mydataframe) > 1) {
      chart.Correlation(ebenpde_obs, histogram = TRUE, method = c("spearman"))
    }
    
    # create outlier grafs and tables
    if (require("PEIP") == TRUE){
      #...........................................(3)emp_distance....................................................    
      # Calsulate empirical distance
      list_emp_distance <- empirical.distance(ebenpde_obs,n.subjects)
      emp_distance <- list_emp_distance$emp_distance
      ID_deleted <- list_emp_distance$ID_deleted
      #...........................................(4)out_tables......................................................   
      # Sort emp_distance values and remember rows, where they were in the beginning, 
      # create a vector of probability,
      # compute the inverse Chi^2 distribution,
      # create out_distance table
      out_tables <- data.for.plots(emp_distance,n.subjects,iiv.eta.names=eta_names)
      index_emp_distance <- out_tables$index_emp_distance
      emp_distance_sort <- out_tables$emp_distance_sort
      theor_distance <- out_tables$theor_distance
      out_distance <- out_tables$out_distance
      
      #...........................................(5)plot_1.....................................................    
      # ChiSq Q-Q plot (save flag and noutlier values from function)
      list_plot_1 <- plot_1(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
                            out_distance,n.subjects,outlying_criteria,do_outlier_plot,
                            model.filename)
      flag <- list_plot_1$flag
      noutlier <- list_plot_1$noutlier
      outlier_id_row <-list_plot_1$outlier_id_row
      
      #............................................(6)plot_2....................................................
      # MORE PAGES OF PDF FILE WITH ChiSq Q-Q plot grafs for each n.subjects (only if out_distance < outlying_criteria && flag==1)
      # vector with values of theor_distance to print on the plot
      list_plot_2 <- plot_2(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,
                            noutlier,flag,n.subjects,iiv.eta.names=eta_names,outlying_criteria,outlier_id_row,
                            do_outlier_plot,model.filename)
      vector_theor_dist <- list_plot_2$vector_theor_dist
      noutlier <- list_plot_2$noutlier
      outlier_id_row <- list_plot_2$outlier_id_row
      
      #............................................(7)outlier_table......................................................    
      #CREATE FINAL TABLE (use function outlier_table to create a table fortable1)
      fortable1 <- outlier.table.ebe.npde(noutlier,outlier_id_row,ebenpde_tmp,ebenpde_obs,
                                          index_emp_distance,emp_distance_sort,vector_theor_dist,
                                          n.subjects,iiv.eta.names=eta_names)
      #for ebe.npde.all.outliers
      if (ncol(fortable1) > 1) {
        ebe.npde_outliers <- fortable1[,1:2]
      } else {
        ebe.npde_outliers <- fortable1
      }
      
      if(i <- 1) {
        ebe.npde_outliers_iiv <- ebe.npde_outliers
      }
      
      #............................................(8)plot.table......................................................    
      #draw the table
      plot.table(fortable1)
      
    } else {
      print("library PEIP not installed, cannot create outlier results for ebe npde")
    } 
    
    
  }
  
  
  
    
#   #.........................................(1)input_ebe_npde...........................    
#   list_input <- list()
#   list_input[[1]] <- input.data(ebe.npde.file,iiv.eta.names)
#   #unlist
#   ebenpde_tmp <- list_input[[1]]$ebenpde_tmp
#   
#   #find rows with NA values and save ID numbers of those rows in the vector
#   deleted_ID <- c()
#   for (i in 1:nrow(ebenpde_tmp)) {
#     if(any(is.na(ebenpde_tmp[i,]))) {
#       deleted_ID <- c(deleted_ID,ebenpde_tmp$ID[i])
#     }
#   }
#   
#   if(any(is.na(ebenpde_tmp))) {
#     list_input <- list()
#     list_input <- two.data.cases(ebenpde_tmp)
#   }
#   
#   list_ebe_outlier_table <- list()
#   cases <- c()
#   for (i in 1:length(list_input)) {
#     ebenpde_tmp <- list_input[[i]]$ebenpde_tmp
#     nr_subjects <- list_input[[i]]$n.subjects
#     ebenpde_obs <- list_input[[i]]$ebenpde_obs
#     iiv.eta.names <- list_input[[i]]$iiv.eta.names
#     case <- list_input[[i]]$case
#     
#     # Add case name in beginning of analysis  
#     if(case != '') {
#       case_table <- data.frame(C = case)
#       names(case_table) <- NULL
#       plot.table(case_table)
#     }
#     
#     #.........................................(2)ebe_npde_summary......................... 
#     # create EBE npde summary statistics table
#     mydataframe <- summary.table.ebe.npde(ebenpde_obs,iiv.eta.names)
#     # draw a table
#     plot.table(mydataframe)
#     
#     #..........................................EBE-NPDE correlation graph.............................................  
#     # create EBE-NPDE correlation graph
#     if(nrow(mydataframe) > 1) {
#       chart.Correlation(ebenpde_obs, histogram = TRUE, method = c("spearman"))
#     }
#     
#     # create outlier grafs and tables
#     if (require("PEIP") == TRUE){
#       #...........................................(3)emp_distance....................................................    
#       # Calsulate empirical distance
#       emp_distance <- empirical.distance(ebenpde_obs,n.subjects=nr_subjects,iiv.eta.names)
#       #...........................................(4)out_tables......................................................   
#       # Sort emp_distance values and remember rows, where they were in the beginning, 
#       # create a vector of probability,
#       # compute the inverse Chi^2 distribution,
#       # create out_distance table
#       out_tables <- data.for.plots(emp_distance,n.subjects=nr_subjects,iiv.eta.names)
#       index_emp_distance <- out_tables$index_emp_distance
#       emp_distance_sort <- out_tables$emp_distance_sort
#       theor_distance <- out_tables$theor_distance
#       out_distance <- out_tables$out_distance
#       
#       #...........................................(5)plot_1.....................................................    
#       # ChiSq Q-Q plot (save flag and noutlier values from function)
#       list_plot_1 <- plot_1(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
#                        out_distance,n.subjects=nr_subjects,outlying_criteria,do_outlier_plot,
#                        model.filename)
#       flag <- list_plot_1$flag
#       noutlier <- list_plot_1$noutlier
#       outlier_id_row <-list_plot_1$outlier_id_row
#       
#       #............................................(6)plot_2....................................................
#       # MORE PAGES OF PDF FILE WITH ChiSq Q-Q plot grafs for each n.subjects (only if out_distance < outlying_criteria && flag==1)
#       # vector with values of theor_distance to print on the plot
#       list_plot_2 <- plot_2(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,
#                        noutlier,flag,n.subjects=nr_subjects,iiv.eta.names,outlying_criteria,outlier_id_row,
#                        do_outlier_plot,model.filename)
#       vector_theor_dist <- list_plot_2$vector_theor_dist
#       noutlier <- list_plot_2$noutlier
#       outlier_id_row <- list_plot_2$outlier_id_row
#       
#       #............................................(7)outlier_table......................................................    
#       #CREATE FINAL TABLE (use function outlier_table to create a table fortable1)
#       fortable1 <- outlier.table.ebe.npde(noutlier,outlier_id_row,ebenpde_tmp,ebenpde_obs,
#                                           index_emp_distance,emp_distance_sort,vector_theor_dist,
#                                           n.subjects=nr_subjects,iiv.eta.names)
#       #for ebe.npde.all.outliers
#       if (ncol(fortable1) > 1) {
#         ebe.npde_outliers <- fortable1[,1:2]
#       } else {
#         ebe.npde_outliers <- fortable1
#       }
#       list_ebe_outlier_table[[i]] <- ebe.npde_outliers
#       cases[i] <- case
#       
#       #............................................(8)plot.table......................................................    
#       #draw the table
#       plot.table(fortable1)
#       
#       #.................................................................................................    
#     } else {
#       print("library PEIP not installed, cannot create outlier results for ebe npde")
#     }
#     
#   }
  
  #######################################     RESIDUALS PLOTS    #############################################
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
  all_outlier_table <- all.outlier.report.table(ofv_outliers,ebe.npde_outliers=ebe.npde_outliers_iiv,cwres.iwres_outliers,ID_deleted)
  
  # draw the table 
  # if((nrow(all_outlier_table) == 1) && (ncol(all_outlier_table)==1)) {
    plot.table(all_outlier_table)
#   } else {
#     tab <- tableGrob(all_outlier_table, rows=NULL)
#     header <- tableGrob(all_outlier_table[1, 1:3], rows=NULL, cols=c("","Individual level", "Observation level")) 
#       
#     jn <- combine(header[1,], tab, along=2)
#     # jn$widths <- rep(max(jn$widths), length(jn$widths)) # make column widths equal
#       
#     # change the relevant rows of gtable
#     jn$layout[1:6 , c("l","r")] <- list(c(1,2,4),c(1,3,5))
#       
#     grid.newpage()
#     grid.draw(jn)
#   }
  
  
#   all_outlier_table <- all.outlier.report.table(ofv_outliers,cwres.iwres_outliers,list_ebe_outlier_table,cases,deleted_ID)
#   
#   # draw the table 
#   # if((nrow(all_outlier_table) == 1) && (ncol(all_outlier_table)==1)) {
#     plot.table(all_outlier_table)
# #   } else {
# #     tab <- tableGrob(all_outlier_table, rows=NULL)
# #     header <- tableGrob(all_outlier_table[1, 1:2], rows=NULL, cols=c("Individual level", "Observation level")) 
# #     
# #     jn <- combine(header[1,], tab, along=2)
# #     jn$widths <- rep(max(jn$widths), length(jn$widths)) # make column widths equal
# #     
# #     # change the relevant rows of gtable
# #     jn$layout[1:4 , c("l","r")] <- list(c(2,4),c(3,5))
# #     
# #     # grid.newpage()
# #     grid.draw(jn)
# #   }
  
  dev.off()
}


