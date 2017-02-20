source(paste0(rscripts.directory, "/scm/first.inclusion.step.R"))
source(paste0(rscripts.directory, "/scm/included.covariates.R"))
source(paste0(rscripts.directory, "/scm/plot_first.included.step.R"))
source(paste0(rscripts.directory, "/scm/plot_included.covariates.R"))

library(ggplot2)

pdf(file=pdf.filename,width=10,height=7,title=pdf.title)

# get included covariates data
list_incl_cov <- included_covariates(scm.short.log)
n1 <- list_incl_cov$n1 #Number of covariate included after forward step
sign <- list_incl_cov$sign
data <- list_incl_cov$data
name <- list_incl_cov$name
    
# plot SCM results included covariates   
p <- plot_included_covariates(data,sign,n1,name)
print(p)


list_first_incl_step <- first_inclusion_step(scm.log.file)
n1 <- list_first_incl_step$n1
data <- list_first_incl_step$data
base <- list_first_incl_step$base
sign <- list_first_incl_step$sign
name <- list_first_incl_step$name

p <- plot_first_included_step(data,base,sign,n1,name)
    
print(p)

dev.off()

