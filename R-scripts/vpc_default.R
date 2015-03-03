library(xpose4)

if (rplots.level > 0){
    pdf(file=pdf.filename,width=10,height=7,title=pdf.title)
}


if (rplots.level > 0){
   done <- FALSE
   if (is.tte){
   	  #data is in the model directory, go there to read input
	  setwd(model.directory)
	  xpdb <- xpose.data(xpose.runno)
	  plots <- kaplan.plot(object=xpdb,VPC=T)
	  #go back to vpc directory 
	  setwd(working.directory)
	  done <- TRUE
	}  

   if (is.categorical & (!done)){
      plots<-xpose.VPC.categorical(vpc.info=tool.results.file,vpctab=vpctab)
	  done <- TRUE
	}

	if ((have.loq.data | have.censored) & (!done) ){
      	plots<-xpose.VPC.both(vpc.info=tool.results.file,vpctab=vpctab)
	  	done <- TRUE
	}

	if (!done){
		plots<-xpose.VPC(vpc.info=tool.results.file,vpctab=vpctab)
		done <- TRUE
	}

    print(plots) 
}

if (rplots.level > 0){
    dev.off()
}

