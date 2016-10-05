# Used libraries
library(ggplot2)

source(paste0(rscripts.directory, "/npfit/data.npfit.R"))
source(paste0(rscripts.directory, "/npfit/plot.npfit.R"))

list_npfit <- data.npfit(raw.nonparametric.file)
make_pdf <- list_npfit$make_pdf
raw_nonparametric <- list_npfit$raw_nonparametric
if (make_pdf) {
  # create pdf file
  pdf(file=pdf.filename,width=11.69, height=8.27)
  
  #make a npsupp/nofv plot 
  plot.npsupp.nofv(raw_nonparametric,n.indiv,n.eta)
  
  #close pdf
  dev.off()
}
