pdf_outl.infl.indiv <- function(all.iofv.file,n.subjects,samples,
                                raw.results.file,skipped.id.file,pdf.filename) {
  # Compare outliers and influential individuals
  outdata <- influential_outliers_data(all.iofv.file,n.subjects,samples,
                                       raw.results.file,skipped.id.file) # use function
  # unlist
  table_for_plot <- outdata$table_for_plot
  ID <- outdata$ID
  row <- outdata$row
  infl_outl <- outdata$infl_outl
  infl_not_outl <- outdata$infl_not_outl
  outl_not_infl <- outdata$outl_not_infl
  not_outl_not_infl <- outdata$not_outl_not_infl
  
  # create pdf file
  pdf(file=pdf.filename,width=10, height=7)
  # plot points, mark outliers and influential individuals ir red
  plot_infl_outl_data(table_for_plot,ID,row)
  
  # plot summary about how many are only influential, only outliers, both and none.
  plot_summary(infl_outl,infl_not_outl,outl_not_infl,not_outl_not_infl)
  
  dev.off()
}