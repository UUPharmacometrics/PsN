ebe.npde.outliers <- function(ebe.npde.file,iiv.eta.names,outlying_criteria,model.filename) {
  #.........................................(1)input_ebe_npde.............................     
  list_input <- input.data(ebe.npde.file,iiv.eta.names)
  #unlist
  ebenpde_tmp <- list_input$ebenpde_tmp
  n.subjects <- list_input$n.subjects
  iiv.eta.names <- list_input$iiv.eta.names
  ebenpde_obs <- list_input$ebenpde_obs
  
  # create outlier table
  if (require("PEIP") == TRUE){
    #...........................................(2)emp_distance....................................................    
    # Calsulate empirical distance
    emp_distance <- empirical.distance(ebenpde_obs,n.subjects,iiv.eta.names)
    #...........................................(3)out_tables......................................................   
    # Sort emp_distance values and remember rows, where they were in the beginning, 
    # create a vector of probability,
    # compute the inverse Chi^2 distribution,
    # create out_distance table
    out_tables <- data.for.plots(emp_distance,n.subjects,iiv.eta.names)
    index_emp_distance <- out_tables$index_emp_distance
    emp_distance_sort <- out_tables$emp_distance_sort
    theor_distance <- out_tables$theor_distance
    out_distance <- out_tables$out_distance
    #...........................................(4)plot_1.....................................................    
    # THIRD PAGE OF PDF FILE WITH ChiSq Q-Q plot (save flag and noutlier values from function)
    plot_1 <- plot_1(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
                     out_distance,n.subjects,outlying_criteria,do_outlier_plot=FALSE,
                     model.filename)
    flag <- plot_1$flag
    noutlier <- plot_1$noutlier
    outlier_id_row <-plot_1$outlier_id_row
    #............................................(5)plot_2....................................................
    # MORE PAGES OF PDF FILE WITH ChiSq Q-Q plot grafs for each n.subjects (only if out_distance < outlying_criteria && flag==1)
    # vector with values of theor_distance to print on the plot
    plot_2 <- plot_2(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,
                     noutlier,flag,n.subjects,iiv.eta.names,outlying_criteria,outlier_id_row,
                     do_outlier_plot=FALSE,model.filename)
    vector_theor_dist <- plot_2$vector_theor_dist
    noutlier <- plot_2$noutlier
    outlier_id_row <- plot_2$outlier_id_row
    #............................................(6)outlier_table......................................................    
    #CREATE FINAL TABLE (use function outlier_table to create a table fortable1)
    fortable1 <- outlier.table.ebe.npde(noutlier,outlier_id_row,ebenpde_tmp,ebenpde_obs,
                                        index_emp_distance,emp_distance_sort,vector_theor_dist,
                                        n.subjects,iiv.eta.names)
    
    #.................................................................................................    
  } else{
    print("library PEIP not installed, cannot create outlier results for ebe npde")
  }
  
  if (ncol(fortable1) > 1) {
    ebe.npde_outliers <- fortable1[,1:2]
  } else {
    ebe.npde_outliers <- data.frame(C = c("No outliers detected"))
    colnames(ebe.npde_outliers) <- NULL
  }
  return(ebe.npde_outliers)
}

