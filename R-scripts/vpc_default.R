
if (rplots.level > 0){
    pdf(file=pdf.filename,width=10,height=7,title=pdf.title)
}


if (rplots.level > 0){
   if (is.categorical){
      plots<-xpose.VPC.categorical(vpc.info=tool.results.file,vpctab=vpctab)
	}else{
		if (have.loq.data | have.censored){
      	   plots<-xpose.VPC.both(vpc.info=tool.results.file,vpctab=vpctab)
		}else{
			plots<-xpose.VPC(vpc.info=tool.results.file,vpctab=vpctab)
		}
	}
    print(plots) 
}

if (rplots.level > 0){
    dev.off()
}

