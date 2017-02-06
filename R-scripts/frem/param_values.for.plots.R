param_values_for_plots <- function(covdata,covariate) {
  # CREATE VECTOR OF POINT (MEAN) NAMES IN PLOT -----------------------------
  point_names <- c() # for point names in plot
  point_color <- c() # for point colors in plot
  MEAN <- c() # for text in left side of plot (mean)
  COVARIATE <- c() # for text in left side of plot (covariate)
  
  for (i in 1:nrow(covdata)) {
    if (covdata$is.categorical[i] != "1") {
      point_names_new <- c(covdata$perc5th[i],covdata$perc95th[i])
      point_color_new <- rbind("cov5th", "cov95th")
      MEAN_new <- rbind(paste0(format(signif(covdata$mean[i], digits=2))," ",covdata$unit[i]),"")
      COVARIATE_new <- rbind(covariate[i],"")
    } else {
      point_names_new <- covdata$category.other[i]
      point_color_new <- "other"
      MEAN_new <- covdata$category.reference[i]
      COVARIATE_new <- covariate[i]
    }
    point_names <- c(point_names,point_names_new)
    point_color <- c(point_color,point_color_new)
    MEAN <- c(MEAN,MEAN_new)
    COVARIATE <- c(COVARIATE,COVARIATE_new)
  }
  
  out <-list(point_names=point_names,
             point_color=point_color,
             MEAN=MEAN,
             COVARIATE=COVARIATE)
  return(out)
}
