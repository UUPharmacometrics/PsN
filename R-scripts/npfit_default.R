# Used libraries
library(PsNR)
library(magrittr)
library(methods)
# get libPaths
R_info(directory=working.directory,only_libPaths=T)
#add R_info to the meta file
R_info(directory=working.directory)

raw_nonparametric <- data.npfit(raw.nonparametric.file)

if (nrow(raw_nonparametric) == 0) {
  print("Don't have npofv values, can't create a plot.")
} else {
  # create pdf file
  pdf(file=pdf.filename,width=11.69, height=8.27)
  
  #make a npsupp/nofv plot 
  plot.npsupp.nofv(raw_nonparametric,n.indiv,n.eta)
  
  #close pdf
  dev.off()
}
