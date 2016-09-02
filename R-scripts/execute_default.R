source(paste0(rscripts.directory,"/common/file_existence_in_directory.R")) # the function for checking if the file exists in the directory
source(paste0(rscripts.directory,"/execute/data.obj.obsi.R")) 
source(paste0(rscripts.directory,"/execute/plot.obj.obsi.R"))

library(xpose4)

xpdb<-xpose.data(xpose.runno,directory=model.directory,tab.suffix=tab.suffix,mod.prefix=mod.prefix,mod.suffix=mod.suffix)

pdf(file=pdf.filename,width=10,height=7,title=pdf.title)

#uncomment below to change the idv from TIME to something else such as TAD.
#Other xpose preferences could also be changed
#xpdb@Prefs@Xvardef$idv="TAD"
runsum(xpdb,dir=model.directory,
         modfile=paste(model.directory,model.filename,sep=""),
         listfile=paste(model.directory,sub(mod.suffix,".lst",model.filename),sep=""))
if (is.null(subset.variable)){
    print(basic.gof(xpdb))
    print(ranpar.hist(xpdb))
    print(ranpar.qq(xpdb))
    print(dv.preds.vs.idv(xpdb))
    print(dv.vs.idv(xpdb))
    print(ipred.vs.idv(xpdb))
    print(pred.vs.idv(xpdb))
    
}else{
    # change the subset variable from categorical to continuous or vice versa.
    # change.cat.cont(xpdb) <- c(subset.variable)
    print(basic.gof(xpdb,by=subset.variable,max.plots.per.page=1))
    print(ranpar.hist(xpdb,by=subset.variable,scales="free",max.plots.per.page=1))
    print(ranpar.qq(xpdb,by=subset.variable,max.plots.per.page=1))
    print(dv.preds.vs.idv(xpdb,by=subset.variable))
    print(dv.vs.idv(xpdb,by=subset.variable))
    print(ipred.vs.idv(xpdb,by=subset.variable))
    print(pred.vs.idv(xpdb,by=subset.variable))
}
  
if (rplots.level > 1){
  #individual plots of ten random IDs
  #find idcolumn
  idvar <- xvardef("id",xpdb)
  ten.random.ids<-sort(sample(unique(xpdb@Data[,idvar]),10,replace=FALSE))
  subset.string <- paste0(idvar,'==',paste(ten.random.ids,collapse=paste0(' | ',idvar,'==')))
  
  
  if (is.null(subset.variable)){
    print(ind.plots(xpdb,subset=subset.string))
  }else{
    for (flag in unique(xpdb@Data[,subset.variable])){
      print(ind.plots(xpdb,subset=paste0(subset.variable,'==',flag,' & (',subset.string,')')))
    }
  }
  
  #check if files exist
  if (res.table != '') {
    file_1_exists <- file_existence_in_directory(directory=model.directory,file_name=paste0(mod.prefix,xpose.runno,".phi"))
    file_2_exists <- file_existence_in_directory(directory=model.directory,file_name=res.table)
    
    if ((file_1_exists == TRUE) && (file_2_exists == TRUE)) {
      # calculate data
      list_out <- data.obj.obsi(obj.data.dir=paste0(model.directory,mod.prefix,xpose.runno,".phi"),
                                obsi.data.dir=paste0(model.directory,res.table))
      OBJ_data <- list_out$OBJ_data
      OBSi_vector <- list_out$OBSi_vector
      OBJ_vector <- list_out$OBJ_vector
      
      # plot data
      plot.obj.obsi(OBJ_data,OBSi_vector,OBJ_vector)
    }
  }
}

dev.off()

