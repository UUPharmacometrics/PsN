library(ggplot2)
library(ncappc)
library(gridExtra)

pdf(file=pdf.filename, width=10, height=7, title=pdf.title)

data = read.table("nca_original.npctab.dta", header=TRUE, skip=1)

colnames(data)[names(data) == idv] = "Time"
colnames(data)[names(data) == dv] = "Conc"

dv.plot(data)

dev.off()


