# Used libraries
library(PerformanceAnalytics)
library(grid)

residual.outliers.file <- paste0(working.directory,'residual_outliers.csv')
# Source ebe npde functions
source(paste0(rscripts.directory,"/simeval/ebe.npde.input.data.R"))
source(paste0(rscripts.directory,"/simeval/ebe.npde.two.data.cases.R"))
source(paste0(rscripts.directory,"/simeval/ebe.npde.summary.table.R"))
source(paste0(rscripts.directory,"/simeval/ebe.npde.empirical.distance.R"))
source(paste0(rscripts.directory,"/simeval/ebe.npde.data.for.plots.R"))
source(paste0(rscripts.directory,"/simeval/ebe.npde.plot_1.R"))
source(paste0(rscripts.directory,"/simeval/ebe.npde.plot_2.R"))
source(paste0(rscripts.directory,"/simeval/ebe.npde.outlier.table.R"))
# Source residual functions
source(paste0(rscripts.directory,"/simeval/residuals.histograms.R"))
source(paste0(rscripts.directory,"/simeval/residuals.summary.table.R"))
source(paste0(rscripts.directory,"/simeval/residuals.outlier.table.R"))
# Source ofv functions
source(paste0(rscripts.directory,"/simeval/ofv.p_ofv_ppc.R"))
source(paste0(rscripts.directory,"/simeval/ofv.i_ofv_npde.R"))
source(paste0(rscripts.directory,"/simeval/ofv.i_ofv_res.R"))
source(paste0(rscripts.directory,"/simeval/ofv.i_ofv_ppc.R"))
source(paste0(rscripts.directory,"/simeval/ofv.kld_i_ofv.R"))
# Source all outliers report functions
# source(paste0(rscripts.directory,"/simeval/ebe.npde.outliers.R"))
# source(paste0(rscripts.directory,"/simeval/all.outlier.report.table.R"))
# source(paste0(rscripts.directory,"/simeval/pdf.all.outlier.report.R"))
# Source common function
source(paste0(rscripts.directory,"/common/plot.table.R"))
# pdf function
source(paste0(rscripts.directory,"/simeval/pdf.simeval.R"))

# Create a pdf files
pdf.simeval(ebe.npde.file=ebe.npde.file,iiv.eta.names=iiv.eta.names,outlying_criteria=outlying_criteria,
            residual.files=residual.files,residual.outliers.file=residual.outliers.file,residual.names=residual.names,
            raw.results.file=raw.results.file,iofv.file=iofv.file,all.iofv.file=all.iofv.file,n.subjects=n.subjects,samples=successful.samples,
            model.filename=model.filename,rplots.level=rplots.level,pdf_filename=pdf.filename)

#new pdf for vpc:s DV vs PRED, CWRES vs idv
library(xpose4)
n.vpc <- length(vpctab.filenames)
pdf(file='PsN_simeval_vpc_plots.pdf',width=10,height=7,title='simeval VPC plots')
for(j in 1:n.vpc){  
  plots <- xpose.VPC(vpc.info=vpc.result.files[j],vpctab=vpctab.filenames[j])
  print(plots) 
}
dev.off()