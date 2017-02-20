plot_included_covariates <- function(data,sign,n1,name) {
  p <- ggplot(data, aes(x, y)) + 
    geom_line() +
    geom_hline(yintercept = sign, colour="red") +
    scale_x_continuous(limits=c(0, n1+1), breaks=(c(0:n1)+0.5),labels=c(name),name="Included covariate") + 
    scale_y_continuous(name="OFV") +
    ggtitle("SCM results - included covariates")
  
  return(p)
}