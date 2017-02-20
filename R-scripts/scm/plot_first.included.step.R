plot_first_included_step <- function(data,base,sign,n1,name) {
  p <- ggplot(data, aes(x, y)) + 
    geom_point(shape="-", size=15) +
    geom_hline(yintercept = base, colour="blue") +
    geom_hline(yintercept = sign, colour="red") +
    scale_x_continuous(breaks=(c(1:n1)),labels=c(name),name="Covariate") + 
    scale_y_continuous(name="OFV") +
    ggtitle("SCM results - first inclusion step")
  
  return(p)
}