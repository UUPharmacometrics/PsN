pdf.all.outlier.report <- function(report.file.name,
                                   all.iofv.file,n.subjects,samples,
                                   ebe.npde.file,iiv.eta.names,outlying_criteria,model.filename,
                                   residual.outliers.file) {
  # Open pdf file
  pdf(file=report.file.name,width=10,height=7)
  
  # create all outliers table
  all_outlier_table <- all.outlier.report.table(all.iofv.file,n.subjects,samples,
                                                ebe.npde.file,iiv.eta.names,outlying_criteria,
                                                residual.outliers.file)
  
  # draw the table 
  if((nrow(all_outlier_table) == 1) && (ncol(all_outlier_table)==1)) {
    plot.table(all_outlier_table)
  } else {
    tab <- tableGrob(all_outlier_table, rows=NULL)
    header <- tableGrob(all_outlier_table[1, 1:2], rows=NULL, cols=c("Individual level", "Observation level")) 
    
    jn <- combine(header[1,], tab, along=2)
    jn$widths <- rep(max(jn$widths), length(jn$widths)) # make column widths equal

    # change the relevant rows of gtable
    jn$layout[1:4 , c("l","r")] <- list(c(2,4),c(3,5))
    
    # grid.newpage()
    grid.draw(jn)
  }
  
  #close pdf file
  dev.off()
}

