ebe.npde.all <- function(ebe.npde.file,iiv.eta.names,iov.eta.names,outlying_criteria,rplots.level,model.filename,make_plot=TRUE) {
  
  if(make_plot) { # make or not to make plot
    # default for the do_outlier_plot
    if(rplots.level > 1) {
      do_outlier_plot <- TRUE
    } else {
      do_outlier_plot <- FALSE
    }
  } else {
    do_outlier_plot <- FALSE
  }
  
  #..........................................(0) all.eta.names.............................
  etas_list <- eta_iiv_iov(iiv.eta.names,iov.eta.names)
  
  eta.names <- etas_list$eta.names
  iiv.eta.names <- etas_list$iiv.eta.names
  iov.eta.names <- etas_list$iov.eta.names
  eta.names_text <- etas_list$eta.names_text
  
  #.........................................(1)input_ebe_npde...........................  
  input_data <- input.data(ebe.npde.file,eta.names)
  
  ebenpde_tmp <- input_data$ebenpde_tmp
  n.subjects <- input_data$n.subjects
  ebenpde_obs <- input_data$ebenpde_obs
  ID_deleted_ebe <- input_data$ID_deleted
  
  #.........................................(2)ebe_npde_summary......................... 
  if(make_plot) { # make or not to make plot
    # create EBE npde summary statistics table
    mydataframe <- summary.table.ebe.npde(ebenpde_obs,eta.names_text)
    # draw a table
    plot.table(mydataframe)
  }
  
  # ..........................................EBE-NPDE correlation graph iiv.............................................
  ebenpde_obs_iiv <- ebenpde_obs[,iiv.eta.names]
  
  # explanation (correlation graph iiv)
  if(length(iov.eta.names) > 0) {
    iiv.eta.names_text <- etas_list$iiv.eta.names_text
    if(make_plot) { # make or not to make plot
      textplot(iiv.eta.names_text,halign="center",valign="center")
    }
  }
  # create EBE-NPDE correlation graph
  if(make_plot) { # make or not to make plot
    if(ncol(ebenpde_obs_iiv) > 1) {
      chart.Correlation(ebenpde_obs_iiv, histogram = TRUE, method = c("spearman"))
    }
  }
  
  # ..........................................EBE-NPDE correlation graph iov.............................................  
  if(length(iov.eta.names)>0) {
    iiv_eta_place <- which(eta.names==iiv.eta.names)
    iov.eta.names_all <- eta.names[-iiv_eta_place]
    ebenpde_obs_iov <- ebenpde_obs[,iov.eta.names_all]
    
    # create ebenpde_obs_iov table by sections
    ebenpde_obs_list <- list()
    for (i in 1:length(iov.eta.names)) {
      input_data_list <- input.data(ebe.npde.file,iov.eta.names[[i]],show.warning=FALSE)
      ebenpde_obs_list[[i]] <- input_data_list$ebenpde_obs
      if(i==1){
        ebenpde_obs_iov_sect <- ebenpde_obs_list[[i]]
      } else {
        col_names <- names(ebenpde_obs_iov_sect)
        new_names <- iov.eta.names[[i]]
        for (j in 1:length(iov.eta.names[[i]])) {
          col_names[j] <- paste0(col_names[j],",",new_names[j])
        }
        colnames(ebenpde_obs_list[[i]]) <- col_names
        colnames(ebenpde_obs_iov_sect) <- col_names
        ebenpde_obs_iov_sect <- rbind(ebenpde_obs_iov_sect,ebenpde_obs_list[[i]])
      }
      
    }
    # explanation (correlation graph iov)
    if((length(iov.eta.names[[1]]) > 1) && !(any(is.na(ebenpde_obs_iov_sect)))) {
      iov.eta.names_text <- etas_list$iov.eta.names_text
      if(make_plot) { # make or not to make plot
        textplot(iov.eta.names_text,halign="center",valign="center")
      }
    }
    
    # create EBE-NPDE IOV correlation graph
    if(make_plot) { # make or not to make plot
      if(!(any(is.na(ebenpde_obs_iov_sect)))) {
        if(ncol(ebenpde_obs_iov_sect) > 1) {
          chart.Correlation(ebenpde_obs_iov_sect, histogram = TRUE, method = c("spearman"))
        }
      }
    }
  }
  
  # create outlier grafs and tables
  if (require("PEIP") == TRUE){
    #...........................................(3)emp_distance....................................................    
    # Calsulate empirical distance
    list_emp_distance <- empirical.distance(ebenpde_obs,n.subjects)
    emp_distance <- list_emp_distance$emp_distance
    #...........................................(4)out_tables......................................................   
    # Sort emp_distance values and remember rows, where they were in the beginning, 
    # create a vector of probability,
    # compute the inverse Chi^2 distribution,
    # create out_distance table
    out_tables <- data.for.plots(emp_distance,n.subjects,eta.names)
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
                          noutlier,flag,n.subjects,eta.names,outlying_criteria,outlier_id_row,
                          do_outlier_plot,model.filename)
    vector_theor_dist <- list_plot_2$vector_theor_dist
    noutlier <- list_plot_2$noutlier
    outlier_id_row <- list_plot_2$outlier_id_row
    
    #............................................(7)outlier_table......................................................    
    #CREATE FINAL TABLE (use function outlier_table to create a table fortable1)
    fortable1 <- outlier.table.ebe.npde(ebenpde_tmp,eta.names,outlier_id_row)
    
    #............................................(8)plot.table......................................................  
    #draw the table
    if(make_plot) { # make or not to make plot
      plot.table(fortable1)
    }
    
    #for ebe.npde.outliers
    if (ncol(fortable1) > 1) {
      ebe.npde_outliers <- fortable1[,1:2]
    } else {
      ebe.npde_outliers <- fortable1
    }
    
  } else {
    print("library PEIP not installed, cannot create outlier results for ebe npde")
  }
  
  out <- list(ebe.npde_outliers=ebe.npde_outliers,
              ID_deleted_ebe=ID_deleted_ebe)
  
  return(out)
}