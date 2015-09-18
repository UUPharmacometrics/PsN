
pdf(file=pdf.filename,width=10,height=7,title=pdf.title)

rawres <- read.csv(raw.results.file)
rawres <- rawres[!is.na(rawres$ofv),]
orig <- rawres$ofv[1]
len <- length(rawres$ofv)
hist(rawres$ofv[2:len],breaks=30,main=model.filename,xlab="pOFV",freq=TRUE)
abline(v=orig,col="green",lwd=2)
abline(v=mean(rawres$ofv[2:len]),lty=5,lwd=2)

dev.off()


