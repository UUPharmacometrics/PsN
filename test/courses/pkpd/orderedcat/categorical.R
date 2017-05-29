
#this template requires simulation tables from xpose.runno
setwd(model.directory)

library(xpose4)
xpdb<-xpose.data(xpose.runno)

pdf(file=paste0(working.directory,pdf.filename),width=10,height=7,title=pdf.title)

## make some stacked bar plots
print(cat.dv.vs.idv.sb(xpdb,idv=NULL,stack=F))
print(cat.dv.vs.idv.sb(xpdb,idv=NULL,stack=F,by="DOSE"))
print(cat.dv.vs.idv.sb(xpdb,idv="DOSE"))
print(cat.dv.vs.idv.sb(xpdb,idv=NULL,stack=F,by="TIME"))
print(cat.dv.vs.idv.sb(xpdb,idv="TIME"))
print(cat.dv.vs.idv.sb(xpdb,idv="CAVH"))
print(cat.dv.vs.idv.sb(xpdb,idv="TIME",by="DOSE",scales=list(x=list(rot=45))))

## make some mirror plots
print(cat.dv.vs.idv.sb(xpdb,idv="DOSE",mirror=1))
print(cat.dv.vs.idv.sb(xpdb,idv="CAVH",mirror=1,auto.key=F))

## create proportion visual predictive check
print(cat.pc(xpdb,idv=NULL))
print(cat.pc(xpdb,idv="DOSE"))
print(cat.pc(xpdb,idv="TIME",histo=T,level.to.plot=1))

dev.off()
