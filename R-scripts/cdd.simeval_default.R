cdd.simeval <- function(rscripts.directory,
                        all.iofv.file,n.subjects,samples,raw.results.file,skipped.id.file,
                        residual.outliers.file,ebe.npde.file,eta.names,
                        pdf.filename) {
  library(ggplot2)
  source(paste0(rscripts.directory,"simeval/ofv.i_ofv_res.R"))
  source(paste0(rscripts.directory,"simeval/ebe.npde.input.data.R"))
  source(paste0(rscripts.directory,"simeval/ebe.npde.empirical.distance.R"))
  source(paste0(rscripts.directory,"simeval/ebe.npde.data.for.plots.R"))
  source(paste0(rscripts.directory,"simeval/ebe.npde.plot_1.R"))
  source(paste0(rscripts.directory,"simeval/ebe.npde.plot_2.R"))
  source(paste0(rscripts.directory,"simeval/ebe.npde.outlier.table.R"))
  source(paste0(rscripts.directory,"simeval/ebe.npde.all.outliers.R"))
  source(paste0(rscripts.directory,"simeval/residuals.outlier.table.R"))
  source(paste0(rscripts.directory,"simeval/all.outlier.report.table.R"))
  source(paste0(rscripts.directory,"cdd/create.data.full.R"))
  source(paste0(rscripts.directory,"cdd/delta.ofv.data.R"))
  source(paste0(rscripts.directory,"cdd/cutoff.cov.cook.R"))
  source(paste0(rscripts.directory,"cdd/all.infl.indiv.table.R"))
  source(paste0(rscripts.directory,"cdd/cov.cook.par.data.R"))
  source(paste0(rscripts.directory,"simeval_cdd/influential_outliers_data.R"))
  source(paste0(rscripts.directory,"simeval_cdd/plot_infl_outl_data.R"))
  source(paste0(rscripts.directory,"simeval_cdd/plot_summary.R"))
  source(paste0(rscripts.directory,"simeval_cdd/pdf_outl.infl.indiv.R"))
  source(paste0(rscripts.directory,"simeval_cdd/ebe.cook.score.data.R"))
  source(paste0(rscripts.directory,"simeval_cdd/plot.param.infl.outl.data.R"))
  source(paste0(rscripts.directory,"simeval_cdd/outlier.infl.table.R"))
  source(paste0(rscripts.directory,"common/plot.table.R"))
 
  cutoff_cook <- 0.8
  cutoff_delta.ofv <- 3.84
  pdf_outl.infl.indiv(all.iofv.file,n.subjects,samples,raw.results.file,skipped.id.file,
                      residual.outliers.file,ebe.npde.file,eta.names,
                      pdf.filename,cutoff_cook,cutoff_delta.ofv)
}