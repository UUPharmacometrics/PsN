library(PsNR)
library(magrittr)
library(methods)
# get libPaths
R_info(directory=working.directory,only_libPaths=T)
#add R_info to the meta file
R_info(directory=working.directory)

pdf.cdd(raw.results.file,skipped.id.file,pdf.filename=pdf.filename,
        cutoff_delta.ofv=3.84)