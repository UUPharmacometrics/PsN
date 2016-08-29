pdf.cov.cook <- function(raw.results.file,skipped.id.file,pdf) {
  # create pdf file
  pdf(file=pdf,width=7,height=7)
  
  # read in input data
  source("../../../code/cdd/create.data.full.R")
  out_cdd.data.all <- create.data.full(raw.results.file,skipped.id.file)
  cdd.data.all <- out_cdd.data.all$cdd.data.all
  
  # 1. plot cook/cov
  source("../../../code/cdd/plot.cov.cook.R")
  plot.cov.cook(cdd.data.all)
  
  # use function for calsulating cov.ratio per parameter and identifying cook score per parameter
  source("../../../code/cdd/cov.cook.par.data.R")
  out <- cov.cook.par.data(cdd.data.all)
  # unlist
  parameters <- out$parameters
  cook.par.data <- out$cook.par.data
  cov.par.data <- out$cov.par.data
  
  # 2. Plot cook(parameter)/cov(parameter)
  source("../../../code/cdd/plot.cov.cook.par.R")
  plot.cov.cook.par(cook.par.data,cov.par.data,parameters)
  
  #close pdf
  dev.off()
}
