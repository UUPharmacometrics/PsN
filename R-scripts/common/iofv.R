library(ggplot2)

iofv_vs_iofv <- function(phi1, phi2) {
    phi1_tab <- read.table(phi1, skip=1, header=T)
    phi2_tab <- read.table(phi2, skip=1, header=T)

    phi1name <- basename(phi1)
    phi2name <- basename(phi2)   

    plot <- ggplot(data=NULL, aes(x=phi1_tab$OBJ, y=phi2_tab$OBJ)) +
        geom_point(shape=1) +
        geom_abline(intercept = 0, slope = 1) +
        xlab(phi1name) +
        ylab(phi2name) +
        ggtitle("iOFV vs iOFV")
    return(plot)
}