
if (rplots.level > 0){
    pdf(file=pdf.filename,width=10,height=7,title=pdf.title)
}

if (rplots.level > 0){
    xpdb<-xpose.data(xpose.runno,directory=model.directory,tab.suffix=tab.suffix,mod.prefix=mod.prefix,mod.suffix=mod.suffix)
    runsum(xpdb,show.plots=FALSE,dir=model.directory)
    print(basic.gof(xpdb))
    print(ranpar.hist(xpdb))
    print(ranpar.qq(xpdb))
    print(ipred.vs.idv(xpdb))
    print(dv.vs.idv(xpdb))
    print(pred.vs.idv(xpdb))
}

if (rplots.level > 1){
    #individual plots of ten random IDs
    ten.random.ids<-sort(sample(as.integer(unlist(xpdb@Data.firstonly[2])),10,replace=FALSE))
    idvar <- xvardef("id",xpdb)
    subset.string <- paste0(idvar,'==',paste(ten.random.ids,collapse=paste0(' | ',idvar,'==')))
    print(ind.plots(xpdb,subset=subset.string))
}
