pdf_outl.infl.indiv <- function(all.iofv.file,raw.results.file,skipped.id.file) {
  # Compare outliers and influential individuals
  outdata <- influential_outliers_data(all.iofv.file,raw.results.file,skipped.id.file) # use function
  # unlist
  infl_data <- outdata$infl_data
  outl_data <- outdata$outl_data
  ID <- outdata$ID
  row_simeval <- outdata$row_simeval
  row_cdd <- outdata$row_cdd
  infl_outl <- outdata$infl_outl
  infl_not_outl <- outdata$infl_not_outl
  outl_not_infl <- outdata$outl_not_infl
  not_outl_not_infl <- outdata$not_outl_not_infl
  
  # create pdf file
  pdf(file=pdf.filename,width=10, height=7)
  # plot points, mark outliers and influential individuals ir red
  plot_infl_outl_data(outl_data,infl_data,ID,row_simeval,row_cdd)
  
  # plot summary about how many are only influential, only outliers, both and none.
  plot_summary(infl_outl,infl_not_outl,outl_not_infl,not_outl_not_infl)
  
  dev.off()
}