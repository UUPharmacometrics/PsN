pdf.ebe.npde <- function(ebe.npde.file,n.eta,all.eta.names,outlying_criteria,ebe.filename,
                         do_outlier_plot,model.filename) {
  # default for the do_outlier_plot
  if(missing(do_outlier_plot)) {
    do_outlier_plot <- TRUE
  }
  
  #.........................................(1)input_ebe_npde...........................     
  list_input <- input.data(ebe.npde.file,n.eta)
  #unlist
  ebenpde_tmp <- list_input$ebenpde_tmp
  n.subjects <- list_input$n.subjects
  ebenpde_obs <- list_input$ebenpde_obs

  # open pdf file
  pdf(file=ebe.filename,width=10,height=7)
  #.........................................(2)ebe_npde_summary.........................    
  # create EBE npde summary statistics table
  mydataframe <- summary.table.ebe.npde(ebenpde_obs,all.eta.names,n.eta)
  
  # draw a table
  plot.table(mydataframe)
  
#..........................................EBE-NPDE correlation graph.............................................  
  # create EBE-NPDE correlation graph
  if (nrow(mydataframe) > 1) {
    chart.Correlation(ebenpde_obs, histogram = TRUE, method = c("spearman"))
  }
  
# create outlier grafs and tables
  if (require("PEIP") == TRUE){
    #...........................................(3)emp_distance....................................................    
    # Calsulate empirical distance
    emp_distance <- empirical.distance(ebenpde_obs,n.subjects,n.eta)
    #...........................................(4)out_tables......................................................   
    # Sort emp_distance values and remember rows, where they were in the beginning, 
    # create a vector of probability,
    # compute the inverse Chi^2 distribution,
    # create out_distance table
    out_tables <- data.for.plots(emp_distance,n.subjects,n.eta)
    index_emp_distance <- out_tables$index_emp_distance
    emp_distance_sort <- out_tables$emp_distance_sort
    theor_distance <- out_tables$theor_distance
    out_distance <- out_tables$out_distance
    #...........................................(5)plot_1.....................................................    
    # THIRD PAGE OF PDF FILE WITH ChiSq Q-Q plot (save flag and noutlier values from function)
    plot_1 <- plot_1(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
                     out_distance,n.subjects,outlying_criteria,do_outlier_plot,
                     model.filename)
    flag <- plot_1$flag
    noutlier <- plot_1$noutlier
    outlier_id_row <-plot_1$outlier_id_row
    #............................................(6)plot_2....................................................
    # MORE PAGES OF PDF FILE WITH ChiSq Q-Q plot grafs for each n.subjects (only if out_distance < outlying_criteria && flag==1)
    # vector with values of theor_distance to print on the plot
    plot_2 <- plot_2(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,
                     noutlier,flag,n.subjects,n.eta,outlying_criteria,outlier_id_row,
                     do_outlier_plot,model.filename)
    vector_theor_dist <- plot_2$vector_theor_dist
    noutlier <- plot_2$noutlier
    outlier_id_row <- plot_2$outlier_id_row
    #............................................(7)outlier_table......................................................    
    #CREATE FINAL TABLE (use function outlier_table to create a table fortable1)
    fortable1 <- outlier.table.ebe.npde(noutlier,outlier_id_row,ebenpde_tmp,ebenpde_obs,
                                        index_emp_distance,emp_distance_sort,vector_theor_dist,
                                        n.subjects,all.eta.names,n.eta)
    
    #............................................(8)plot.table......................................................    
    #draw the table
    plot.table(fortable1)
    
    #.................................................................................................    
  } else{
    print("library PEIP not installed, cannot create outlier results for ebe npde")
  }
  
  dev.off()
}