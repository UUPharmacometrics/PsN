# Used libraries
library(ggplot2)

source(paste0(rscripts.directory, "/npfit/plot.npfit.R"))

# create pdf file
pdf(file=pdf.filename,width=11.69, height=8.27)
  
  #make a npsupp/nofv plot 
  plot.npsupp.nofv(raw.nonparametric.file,n.indiv,n.eta)
  
#close pdf
dev.off()