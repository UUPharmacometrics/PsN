pdf.delta.ofv <- function(raw.results.file,skipped.id.file,pdf,outlier_ID) {
  # create pdf file
  pdf(file=pdf,width=11.69, height=8.27)
  
  # read in input data
  out_cdd.data.all <- create.data.full(raw.results.file,skipped.id.file)
  cdd.data.all <- out_cdd.data.all$cdd.data.all

  if (missing(outlier_ID)) {
    # create needed data form plotting
    list.delta.ofv <- delta.ofv.data(cdd.data.all)
    model <- list.delta.ofv$model
    delta.ofv <- list.delta.ofv$delta.ofv
    row_infl <- list.delta.ofv$row_infl
    ID <- list.delta.ofv$ID
    # create a plot
    plot.delta.ofv(delta.ofv,model,ID,row_infl)
  } else {
    # create needed data for plotting
    list.delta.ofv <- delta.ofv.data(cdd.data.all,outlier_ID)
    model <- list.delta.ofv$model
    delta.ofv <- list.delta.ofv$delta.ofv
    row_outl_infl <- list.delta.ofv$row_outl_infl
    row_outl <- list.delta.ofv$row_outl
    row_infl <- list.delta.ofv$row_infl
    ID <- list.delta.ofv$ID
    # create a plot
    plot.delta.ofv(delta.ofv,model,ID,row_infl,row_outl,row_outl_infl)
  }
  
#close pdf
dev.off()
}
