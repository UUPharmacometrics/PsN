# PREPARE DATA FRAME FOR PLOTTING ----------------------------------------
param_plot_data <- function(list_DF,point_names,point_color,MEAN,COVARIATE) {
  list_outTable <- list()
  for (i in 1:length(list_DF)) {
    DF <- list_DF[[i]]
    # Create long data set (all columns in one column)
    DF_melt <- melt(DF)
  
    # summaryze dataframe and calculate mean, quantile for each group (for each column in DF)
    outTable <- DF_melt %>% group_by(variable) %>%
      summarise(mean = value[1],
                ci_low = quantile(value[-1], probs=c(0.05),type=2),
                ci_high = quantile(value[-1], probs=c(0.95),type=2))
    # calculating procentage of outTable
    outTablet_proc <- t(round(t((outTable[, 2:ncol(outTable)])-1) * 100, 2))
    outTable <- cbind(outTable[1],outTablet_proc)
    # add some needed columns for plotting
    outTable$EXPECTED <- sprintf("%+.3G %%  [%+.3G, %+.3G]",outTable$mean,outTable$ci_low,outTable$ci_high)
    outTable$points <- point_names
    outTable$group <- point_color
    outTable$MEAN <- MEAN
    outTable$COVARIATE <- COVARIATE
  
    # add an empty row to outTable
    empty_row <- c(rep(NA,ncol(outTable)))
    outTable <-rbind(empty_row,outTable)
    list_outTable[[i]] <- outTable
  }
  return(list_outTable)
}

