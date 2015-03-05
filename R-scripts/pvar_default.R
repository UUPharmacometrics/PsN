if (rplots.level > 0) {
    pdf(file=pdf.filename, width=10, height=7, title=pdf.title)
    par(mfrow=c(2,2))

    result = read.csv("result.csv", as.is=TRUE)

    models = result$Model.name
    models = models[seq(1, length(models), 3)]   # Extract every third
    models = gsub("\\.(mod|ctl)$", "", models)


    for (i in 4 : ncol(result)) {
        values = result[[i]]
        values = values[-seq(3, length(values), 3)]  # Remove PV
        dim(values) = c(2, length(values) / 2)      # make into 2 x n matrix
        colnames(values) = models
        rownames(values) = c("EPV", "UPV")

        barplot(values, xlab="Model", ylab="Variability", legend=TRUE, main=colnames(result)[i])
        abline(h=0)
    }

    dev.off()
}
