cdd.simeval <- function(rscripts.directory,all.iofv.file,n.subjects,samples,
                        raw.results.file,skipped.id.file,pdf.filename) {
  library(ggplot2)
  
  source(paste0(rscripts.directory,"simeval/ofv.i_ofv_res.R")) 
  source(paste0(rscripts.directory,"cdd/create.data.full.R"))
  source(paste0(rscripts.directory,"cdd/delta.ofv.data.R"))
  source(paste0(rscripts.directory,"simeval_cdd/influential_outliers_data.R"))
  source(paste0(rscripts.directory,"simeval_cdd/plot_infl_outl_data.R"))
  source(paste0(rscripts.directory,"simeval_cdd/plot_summary.R"))
  source(paste0(rscripts.directory,"simeval_cdd/pdf_outl.infl.indiv.R"))
 
  pdf_outl.infl.indiv(all.iofv.file,n.subjects,samples,raw.results.file,skipped.id.file,pdf.filename)
}