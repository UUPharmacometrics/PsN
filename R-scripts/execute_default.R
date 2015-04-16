library(xpose4)

xpdb<-xpose.data(xpose.runno,directory=model.directory,tab.suffix=tab.suffix,mod.prefix=mod.prefix,mod.suffix=mod.suffix)
pdf(file=pdf.filename,width=10,height=7,title=pdf.title)

runsum(xpdb,show.plots=TRUE,dir=model.directory)
print(basic.gof(xpdb,by=subset.variable))
print(ranpar.hist(xpdb,by=subset.variable))
print(ranpar.qq(xpdb,by=subset.variable))
print(dv.preds.vs.idv(xpdb,by=subset.variable))
print(dv.vs.idv(xpdb,by=subset.variable))
print(ipred.vs.idv(xpdb,by=subset.variable))
print(pred.vs.idv(xpdb,by=subset.variable))

if (rplots.level > 1){
    #individual plots of ten random IDs
	#find idcolumn
    idvar <- xvardef("id",xpdb)
	regexp <- paste0('^',idvar,'$')
	idpos <- grep(regexp,names(xpdb@Data))
    ten.random.ids<-sort(sample(as.integer(levels(factor(xpdb@Data[idpos][,idvar]))),10,replace=FALSE))
    subset.string <- paste0(idvar,'==',paste(ten.random.ids,collapse=paste0(' | ',idvar,'==')))

    print(ind.plots(xpdb,subset=subset.string))
}

dev.off()

