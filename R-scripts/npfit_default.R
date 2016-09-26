# Used libraries
library(ggplot2)

source(paste0(rscripts.directory, "/npfit/plot.npfit.R"))

# create pdf file
raw_nonparametric_file <- paste0(working.directory,"raw_nonparametric_pheno.csv")
pdf(file=pdf.filename,width=11.69, height=8.27)
  
  #make a npsupp/nofv plot 
  plot.npsupp.nofv(raw_nonparametric_file)
  
#close pdf
dev.off()