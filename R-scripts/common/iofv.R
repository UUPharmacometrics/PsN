iofv_vs_iofv <- function(phi1, phi2,quiet=F) {
  if(file.exists(phi1) && file.exists(phi2)) {
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
        theme_bw() 
      out<-list(plot=p,
                make_plot=make_plot)
    } else {
      if(!quiet) {
        message("WARNING: Length of ID numbers in files ",phi1," and ",phi2," are not equal!")
      }
      make_plot <- FALSE
      out <- list(make_plot=make_plot)
    }
  } else {
    if(!file.exists(phi1) && !quiet) {
      message("WARNING: File ",phi1," not found!")
    }
    if(!file.exists(phi2) && !quiet) {
      message("WARNING: File ",phi2," not found!")
    }
    make_plot <- FALSE
    out <- list(make_plot=make_plot)
  }
    return(out)
}