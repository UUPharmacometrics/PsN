pdf.cdd <- function(raw.results.file,skipped.id.file,pdf.filename,markeropt,
                    min.failed,cov.failed,cov.warnings,boundary,legend,cutoff_delta.ofv,
                    cutoff_cook,outlier_ID) {
  #default for legend is FALSE
  if (missing(legend)) {
    legend <- FALSE
  }  
  #default for legend is FALSE
  if (missing(cutoff_cook)) {
    cutoff_cook <- 0.8
  }
  # 1. Create table cdd.data
  list_input_data <- create.data.full(raw.results.file,skipped.id.file)
  cdd.data.all <- list_input_data$cdd.data.all
  make_pdf <- list_input_data$make_pdf
  
  if(make_pdf) {
    ## skip first record (no cases deleted)
    cdd.data <- cdd.data.all[-1,]
    
    # 2. Create tables cdd.warn and p1
    p1 <- warnings.data(cdd.data,min.failed,cov.failed,cov.warnings,boundary) 
    if (class(p1) == "list") {
      cdd.warn <- p1$cdd.warn
      p1 <- p1$p1
    }
    # 3. Create tables cdd.pt and cdd.txt
    out <- mark.options.data(p1,markeropt)
    cdd.pt <- out$cdd.pt
    cdd.txt <- out$cdd.txt
    
    # 4. Count not successful values
    fail <- failed.values(cdd.data)
    
    # create pdf file
    pdf(file=pdf.filename,width=11.69, height=8.27)
    
    # 6. calculate cutoff cov ratios
    list_out <- cutoff.cov.cook(raw.results.file,skipped.id.file,cutoff_cook)
    cutoff_cov <- list_out$cutoff_cov
    
    # 5. Create a plot
    if (exists("cdd.warn")) {
      plot.cdd(cdd.data,cutoff_cook,cutoff_cov,cdd.pt,cdd.txt,legend,fail,cdd.warn)
    } else {
      plot.cdd(cdd.data,cutoff_cook,cutoff_cov,cdd.pt,cdd.txt,legend,fail)
    }
    
    # create cutoff tables
    library(gridExtra)
    # rotate column names
    rotate = ttheme_default(colhead=list(fg_params=list(rot=90)))
    #unlist
    
    cook_outliers_data <- list_out$cook_outliers_data
    infl_cook_data <- list_out$infl_cook_data
    # draw a table
    plot.table(cook_outliers_data,rotate=rotate)
    
    
    cov_outliers_data <- list_out$cov_outliers_data
    infl_cov_data <- list_out$infl_cov_data
    # draw a table
    plot.table(cov_outliers_data,rotate=rotate)
    
    # 8. influential indivifuals
    if (missing(outlier_ID)) {
      # create needed data form plotting
      list.delta.ofv <- delta.ofv.data(cdd.data.all,cutoff_delta.ofv=cutoff_delta.ofv)
      data_plot <- list.delta.ofv$data_plot
      row_infl <- list.delta.ofv$row_infl
      fail_ID <- list.delta.ofv$fail_ID
      infl_ofv <- list.delta.ofv$infl_ofv
      # create a plot
      plot.delta.ofv(data_plot,row_infl,fail_ID,cutoff_delta.ofv=cutoff_delta.ofv)
    } else {
      # create needed data for plotting
      list.delta.ofv <- delta.ofv.data(cdd.data.all,cutoff_delta.ofv=cutoff_delta.ofv,outlier_ID)
      data_plot <- list.delta.ofv$data_plot
      row_outl_infl <- list.delta.ofv$row_outl_infl
      row_outl <- list.delta.ofv$row_outl
      row_infl <- list.delta.ofv$row_infl
      fail_ID <- list.delta.ofv$fail_ID
      infl_ofv <- list.delta.ofv$infl_ofv
      # create a plot
      plot.delta.ofv(data_plot,row_infl,fail_ID,row_outl,row_outl_infl,cutoff_delta.ofv=cutoff_delta.ofv)
    }
    
    # influential summary table
    all_infl_indiv_table <- all.infl.indiv.table(infl_ofv,infl_cook_data,infl_cov_data,fail_ID)
    plot.table(all_infl_indiv_table)
    
    #close pdf
    dev.off()
  } else {
    message(paste0("Can't create a pdf file, because no cook score or cov ratio values are found in the raw results csv file!"))
  }
}
