##################
## template settings
## not needed if running rigrectly in R instead of -rplots from PsN
library(PsNR)

meta <- PsNR::metadata(working.directory)

#this template requires both tables from runx.mod and runxsim.mod 
setwd(dirname(PsNR::model_path(meta)))
pdf(file=paste0(working.directory,pdf.filename),width=10,height=7,title=pdf.title) ## set output for plots
on.exit(dev.off(),add=T)
##################

library(xpose4)

## read in table files
## xpose.runno <- 36 # uncomment for use outside -rplots
xpdb<-xpose.data(xpose.runno)

## make some bar plots
cat.dv.vs.idv.sb(xpdb,idv=NULL,dv="RDV")
cat.dv.vs.idv.sb(xpdb,idv="TIME",dv="RDV")
cat.dv.vs.idv.sb(xpdb,idv="TIME",dv="RDV",by="DOSE")
cat.dv.vs.idv.sb(xpdb,idv="TIME",dv="RDV",by="DOSE",scales=list(x=list(rot=45)))

## make some mirror plots of bar plots 
cat.dv.vs.idv.sb(xpdb,idv="TIME",dv="RDV",by="DOSE",scales=list(x=list(rot=45)),mirror=3)
cat.dv.vs.idv.sb(xpdb,idv="TIME",dv="RDV",mirror=1)


## create binary predictive check
cat.pc(xpdb,dv="RDV",idv=NULL,level.to.plot=1)
cat.pc(xpdb,dv="RDV",idv="DOSE",level.to.plot=1)
cat.pc(xpdb,dv="RDV",idv="TIME",level.to.plot=1)

## VPCs using cat.pc
cat.pc(xpdb,dv="RDV",idv="TIME",histo=F)

cat.pc(xpdb,dv="RDV",idv="TIME",subset="DOSE==0",histo=F)
cat.pc(xpdb,dv="RDV",idv="TIME",subset="DOSE==10",histo=F)
cat.pc(xpdb,dv="RDV",idv="TIME",subset="DOSE==50",histo=F)
cat.pc(xpdb,dv="RDV",idv="TIME",subset="DOSE==200",histo=F)
