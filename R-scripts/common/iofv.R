library(ggplot2)

iofv_vs_iofv <- function(phi1, phi2) {
  make_plot <- TRUE
    phi1_tab <- read.table(phi1, skip=1, header=T)
    phi2_tab <- read.table(phi2, skip=1, header=T)

    phi1name <- basename(phi1)
    phi2name <- basename(phi2)
    
    if(length(phi1_tab$OBJ) == length(phi2_tab$OBJ)) {
      p <- ggplot(data=NULL, aes(x=phi1_tab$OBJ, y=phi2_tab$OBJ)) +
        geom_point(shape=1) +
        geom_abline(intercept = 0, slope = 1) +
        xlab(phi1name) +
        ylab(phi2name) +
        theme_bw() +
        ggtitle("iOFV vs iOFV")
      out<-list(plot=p,
                make_plot=make_plot)
    } else {
      make_plot <- FALSE
      out <- list(make_plot=make_plot)
    }
    return(out)
}