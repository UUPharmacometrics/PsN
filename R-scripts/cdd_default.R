library(PsNR)
library(gridExtra)
library(magrittr)
library(methods)
# get libPaths
R_info(directory=working.directory,only_libPaths=T)
#source needed functions
source(paste0(rscripts.directory, "/cdd/pdf.cdd.R"))
source(paste0(rscripts.directory,"/cdd/cov.cook.par.data.R"))
source(paste0(rscripts.directory,"/cdd/warnings.data.R"))
source(paste0(rscripts.directory,"/cdd/failed.values.R"))
source(paste0(rscripts.directory,"/cdd/plot.cdd.R"))
source(paste0(rscripts.directory,"/cdd/cutoff.cov.cook.R"))
source(paste0(rscripts.directory,"/cdd/delta.ofv.data.R"))
source(paste0(rscripts.directory,"/cdd/plot.delta.ofv.R"))
source(paste0(rscripts.directory,"/cdd/all.infl.indiv.table.R"))

#add R_info to the meta file
R_info(directory=working.directory)

pdf.cdd(raw.results.file,skipped.id.file,pdf.filename=pdf.filename,
        cutoff_delta.ofv=3.84)