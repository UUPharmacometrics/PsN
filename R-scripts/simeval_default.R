# Used libraries
library(PerformanceAnalytics)

residual.outliers.file <- paste0(working.directory,'residual_outliers.csv')
# Source ebe npde functions
source(paste0(rscripts.directory,"/simeval/ebe.npde.input.data.R"))
source(paste0(rscripts.directory,"/simeval/ebe.npde.summary.table.R"))
source(paste0(rscripts.directory,"/simeval/ebe.npde.empirical.distance.R"))
source(paste0(rscripts.directory,"/simeval/ebe.npde.data.for.plots.R"))
source(paste0(rscripts.directory,"/simeval/ebe.npde.plot_1.R"))
source(paste0(rscripts.directory,"/simeval/ebe.npde.plot_2.R"))
source(paste0(rscripts.directory,"/simeval/ebe.npde.outlier.table.R"))
source(paste0(rscripts.directory,"/simeval/pdf.ebe.npde.R"))
# Source residual functions
source(paste0(rscripts.directory,"/simeval/residuals.histograms.R"))
source(paste0(rscripts.directory,"/simeval/residuals.summary.table.R"))
source(paste0(rscripts.directory,"/simeval/residuals.outlier.table.R"))
source(paste0(rscripts.directory,"/simeval/pdf.cwres.iwres.R"))
# Source ofv functions
source(paste0(rscripts.directory,"/simeval/ofv.p_ofv_ppc.R"))
source(paste0(rscripts.directory,"/simeval/ofv.i_ofv_npde.R"))
source(paste0(rscripts.directory,"/simeval/ofv.i_ofv_res.R"))
source(paste0(rscripts.directory,"/simeval/ofv.i_ofv_ppc.R"))
source(paste0(rscripts.directory,"/simeval/ofv.kld_i_ofv.R"))
source(paste0(rscripts.directory,"/simeval/pdf_ofv.R"))
# Source all outliers report functions
source(paste0(rscripts.directory,"/simeval/ebe.npde.outliers.R"))
source(paste0(rscripts.directory,"/simeval/all.outlier.report.table.R"))
source(paste0(rscripts.directory,"/simeval/pdf.all.outlier.report.R"))
# Source common function
source(paste0(rscripts.directory,"/common/plot.table.R"))

# Create a pdf files
pdf.ebe.npde(ebe.npde.file=ebe.npde.file,n.eta=n.eta,all.eta.names=all.eta.names,
             outlying_criteria=outlying_criteria,ebe.filename='PsN_ebe_npde_plots.pdf',
             model.filename=model.filename)

pdf.cwres.iwres(residual.files=residual.files,residual.outliers.file,
                residual.names=residual.names,pdf.filename='PsN_residual_plots.pdf')

pdf_ofv(raw.results.file=raw.results.file,iofv.file=iofv.file,all.iofv.file=all.iofv.file,
        n.subjects=n.subjects,samples=successful.samples,
        ofv.filename='PsN_OFV_plots.pdf',rplots.level=rplots.level,
        model.filename=model.filename)

pdf.all.outlier.report(report.file.name="PsN_outlier_report_table.pdf",
                       all.iofv.file=all.iofv.file,n.subjects=n.subjects,samples=successful.samples,
                       ebe.npde.file=ebe.npde.file,
                       n.eta=n.eta,outlying_criteria=outlying_criteria,
                       residual.outliers.file=residual.outliers.file)

#new pdf for vpc:s DV vs PRED, CWRES vs idv
library(xpose4)
n.vpc <- length(vpctab.filenames)
pdf(file='PsN_simeval_vpc_plots.pdf',width=10,height=7,title='simeval VPC plots')
for(j in 1:n.vpc){  
  plots <- xpose.VPC(vpc.info=vpc.result.files[j],vpctab=vpctab.filenames[j])
  print(plots) 
}
dev.off()