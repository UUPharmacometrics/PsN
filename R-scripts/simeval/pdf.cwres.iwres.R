pdf.cwres.iwres <- function(residual.files,residual.outliers.file,
                            residual.names,pdf.filename) {
  # Open PDF file
  pdf(file=pdf.filename,width=10,height=7)
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
  
  # Plot created outlier table on the next page
  plot.table(outlierframe)
  
  dev.off()
}

