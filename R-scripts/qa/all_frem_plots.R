all_frem_plots <- function(working.directory) {
  if((file.exists(file.path(working.directory,"/postfrem_run/frem_ratio.csv")) || 
      file.exists(file.path(working.directory,"/postfrem_run/frem_id_ratios.csv"))) && 
     file.exists(file.path(working.directory,"/postfrem_run/covdata.csv")) &&
     file.exists(file.path(working.directory,"/postfrem_run/pardata.csv"))) {
    covdata <- read.csv(file.path(working.directory,"postfrem_run","covdata.csv"),header = T, as.is = T)
    pardata <- read.csv(file.path(working.directory,"postfrem_run","pardata.csv"),header = T, as.is = T)
    if(file.exists(file.path(working.directory,"/postfrem_run/frem_ratio.csv"))) {
      inTable_frem <- read.csv(file.path(working.directory,"postfrem_run","frem_ratio.csv"),header = T, as.is = T)
      cov_effect_on_param_plots <- parameter_ratio(inTable_frem,covdata,pardata)$plots
      grid::grid.draw(gridExtra::marrangeGrob(cov_effect_on_param_plots,nrow=2,ncol=1,top=''))
      cat("5th and 95th percentile of each covariate and the size of the effect on parameter (assuming log-normal). The bands show 90% confidence interval around the point estimate. Categorical covariates are compared to the other category.")
      grid::grid.newpage()
    }
    if(file.exists(file.path(working.directory,"/postfrem_run/frem_id_ratios.csv"))) {
      frem_id <- read.csv(file.path(working.directory,"postfrem_run","frem_id_ratios.csv"), header = T, as.is = T)
      indiv_for_param_plots <- ID_ratio(frem_id,covdata,pardata)$plots
      grid::grid.draw(gridExtra::marrangeGrob(indiv_for_param_plots,nrow=2,ncol=1,top=''))
      cat("Most extreme individuals (in both directions) in terms of expected effect size of their covariate sets, on parameter (assuming log-normal). The bands show 90% confidence interval around the point estimate.")
      grid::grid.newpage()
    }
    if(file.exists(file.path(working.directory,"/postfrem_run/sd_coefficients_summary.csv"))) {
      sd_coef_summary <- read.csv(file.path(working.directory,"postfrem_run","sd_coefficients_summary.csv"), header = T, as.is = T)
      sd_unexpl_var_plots <- sd_unexpl_var(sd_coef_summary,covdata,pardata)$plots
      grid::grid.draw(gridExtra::marrangeGrob(sd_unexpl_var_plots,nrow=2,ncol=1,top=''))
      cat("Amount of unexplained variability (on standard deviation scale) remaining in the parameters after knowledge of no covariates, each covariate separately and all covariates simultaneously.")
      grid::grid.newpage()
    }
    
  }
}