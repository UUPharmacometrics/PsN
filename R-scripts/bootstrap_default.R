require(xpose4)
if(packageVersion("xpose4")<"4.5.0"){
		warning("xpose4 version must be 4.5.0 or later for bootstrap plot")	
}							 

if (rplots.level > 0){
    pdf(file=pdf.filename,width=10,height=7,title=pdf.title)
}


if (rplots.level > 0){
    bootplots<-boot.hist(results.file=raw.results.file,incl.ids.file=included.ids.file,
            min.failed=skip.minimization.terminated,
            cov.failed=skip.covariance.step.terminated,
            cov.warnings=skip.with.covstep.warnings,
            boundary=skip.estimate.near.boundary)
    print(bootplots[1]) #parameters
}

if (rplots.level > 1){
    print(bootplots[2:4]) #SEs ofv eigenvalues
}

if (rplots.level > 0){
    dev.off()
}

