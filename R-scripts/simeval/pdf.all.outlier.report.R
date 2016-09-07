pdf.all.outlier.report <- function(report.file.name,
                                   all.iofv.file,n.subjects,samples,ofv_outlier_criteria,
                                   ebe.npde.file,n.eta,outlying_criteria,model.filename,
                                   residual.outliers.file) {
  # Open pdf file
  pdf(file=report.file.name,width=10,height=7)
  
  # create all outliers table
  all_outlier_table <- all.outlier.report.table(all.iofv.file,n.subjects,samples,ofv_outlier_criteria,
                                                ebe.npde.file,n.eta,outlying_criteria,
                                                residual.outliers.file)
  
  # draw the table 
  plot.table(all_outlier_table)
  
  #close pdf file
  dev.off()
}

