library(xpose4)

if (rplots.level > 0){
    pdf(file=pdf.filename,width=10,height=7,title=pdf.title)
}

if (rplots.level > 0){
    xpdb<-xpose.data(xpose.runno,directory=model.directory,tab.suffix=tab.suffix,mod.prefix=mod.prefix,mod.suffix=mod.suffix)
	#uncomment below to change the idv from TIME to something else such as TAD.
	#Other xpose preferences could also be changed
	#xpdb@Prefs@Xvardef$idv="TAD"
	runsum(xpdb,show.plots=TRUE,dir=model.directory)
	if (is.null(subset.variable)){
       print(basic.gof(xpdb))
       print(ranpar.hist(xpdb))
       print(ranpar.qq(xpdb))
       print(dv.preds.vs.idv(xpdb))
       print(dv.vs.idv(xpdb))
       print(ipred.vs.idv(xpdb))
       print(pred.vs.idv(xpdb))
	    
	}else{
	
		regexp <- paste0('^',subset.variable,'$')
		#cat(regexp)
		pos <- grep(regexp,names(xpdb@Data))
		#cat(pos)
		doloop <- FALSE
		if (is.factor(xpdb@Data[pos][,subset.variable])){
		   #below assumes subset.variable is already stored as a factor in xpdb
		   flagvector <- levels(xpdb@Data[pos][subset.variable,])
		}else{
			#using 'by' with Xpose will not work well
			doloop <- TRUE
		   flagvector <- levels(as.factor(xpdb@Data[pos][,subset.variable]))
		}
		#cat(flagvector)
		for (flag in flagvector){
			print(basic.gof(xpdb,subset=paste0(subset.variable,'==',flag)))
		}

		if (doloop){
			for (flag in flagvector){
				print(ranpar.hist(xpdb,subset=paste0(subset.variable,'==',flag)))
			}
			for (flag in flagvector){
				print(ranpar.qq(xpdb,subset=paste0(subset.variable,'==',flag)))
			}
			for (flag in flagvector){
				print(dv.preds.vs.idv(xpdb,subset=paste0(subset.variable,'==',flag)))
			}
			for (flag in flagvector){
				print(dv.vs.idv(xpdb,subset=paste0(subset.variable,'==',flag)))
			}
			for (flag in flagvector){
				print(ipred.vs.idv(xpdb,subset=paste0(subset.variable,'==',flag)))
			}
			for (flag in flagvector){
				print(pred.vs.idv(xpdb,subset=paste0(subset.variable,'==',flag)))
			}
		}else{
			print(ranpar.hist(xpdb,by=subset.variable))
			print(ranpar.qq(xpdb,by=subset.variable))
			print(dv.preds.vs.idv(xpdb,by=subset.variable))
			print(dv.vs.idv(xpdb,by=subset.variable))
			print(ipred.vs.idv(xpdb,by=subset.variable))
			print(pred.vs.idv(xpdb,by=subset.variable))
		}
	}

}

if (rplots.level > 1){
    #individual plots of ten random IDs
	#find idcolumn
    idvar <- xvardef("id",xpdb)
	regexp <- paste0('^',idvar,'$')
	idpos <- grep(regexp,names(xpdb@Data))
    ten.random.ids<-sort(sample(as.integer(levels(factor(xpdb@Data[idpos][,idvar]))),10,replace=FALSE))
    subset.string <- paste0(idvar,'==',paste(ten.random.ids,collapse=paste0(' | ',idvar,'==')))

	if (is.null(subset.variable)){
	    print(ind.plots(xpdb,subset=subset.string))
	}else{
		for (flag in flagvector){
			print(ind.plots(xpdb,subset=paste0(subset.variable,'==',flag,' & (',subset.string,')')))
		}
	}

}

if (rplots.level > 0){
    dev.off()
}
