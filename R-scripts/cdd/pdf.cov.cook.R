pdf.cov.cook <- function(raw.results.file,skipped.id.file,pdf) {
  # create pdf file
  pdf(file=pdf,width=7,height=7)
  
  # read in input data
  out_cdd.data.all <- create.data.full(raw.results.file,skipped.id.file)
  cdd.data.all <- out_cdd.data.all$cdd.data.all
  
  # 1. plot cook/cov
  plot.cov.cook(cdd.data.all)
  
  # use function for calsulating cov.ratio per parameter and identifying cook score per parameter
  out <- cov.cook.par.data(cdd.data.all)
  # unlist
  parameters <- out$parameters
  cook.par.data <- out$cook.par.data
  cov.par.data <- out$cov.par.data
  
  # 2. Plot cook(parameter)/cov(parameter)
  plot.cov.cook.par(cook.par.data,cov.par.data,parameters)
  
  #close pdf
  dev.off()
}
