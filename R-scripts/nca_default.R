library(ggplot2)
library(ncappc)
library(gridExtra)

if (rplots.level > 0) {
    pdf(file=pdf.filename, width=10, height=7, title=pdf.title)
}

if (rplots.level > 0) {
    data = read.table("nca_original.npctab.dta", header=TRUE, skip=1)

    colnames(data)[names(data) == "TIME"] = "Time"
    colnames(data)[names(data) == "DV"] = "Conc"

    dv.plot(data)
}

if (rplots.level > 0){
    dev.off()
}

