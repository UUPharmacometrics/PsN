unexplaned_variability <- function(frem_condvar,covdata,pardata) {
  # check if there are all 3 input data files
  files_exist <- (exists("frem_condvar") & exists("covdata") & exists("pardata"))
  if (files_exist) {
    library(grid)
    library(gridExtra)
    library(reshape2)
    library(dplyr)
    library(ggplot2)
    
    # in case if column names consist of not valid symbols, for example, "("
    parameter_names <- pardata[,1]
    col_names <- colnames(pardata)
    pardata <- data.frame(make.names(parameter_names),stringsAsFactors = F)
    colnames(pardata) <- col_names
    
    # colnames of frem data
    colnames_frem <- colnames(frem_condvar)
    # names of covariate (names of first column in covdata input table, header = FALSE)
    covariate <- as.character(covdata[[1]])
    covariate <- c("none",covariate,"allcov")
    
    # delete "LN" prefixes (if they exist) from any of covariates
    for (i in 1:length(covariate)) {
      if (grepl("^LN", covariate[i])) {
        covariate[i] <- gsub("\\LN","",covariate[i])
      }
    }
    
    # delete "LN" prefixes (if they exist) from any of colnames in frem_condvar data frame
    col_names <- colnames(frem_condvar)
    for (i in 1:length(col_names)) {
      if (grepl("\\.LN",col_names[i])) {
        col_names[i] <- gsub("\\.LN",".",col_names[i])
      }
    }
    colnames(frem_condvar) <- col_names
    
    # SORT NEEDED DATA FOR EACH PARAMETER -------------------------------------
    list_part <- list()
    list_colnames <- list()
    unexplaned_variability_plots <- list()
    param <- list()
    for (j in 1:nrow(pardata)) {
      DF <- as.data.frame(array(0,c(nrow(frem_condvar),length(covariate))))
      for (i in 1:length(covariate)) {
        if (covariate[i] != "none" && covariate[i] != "allcov") {
          DF[,i] <- frem_condvar[ , grepl(paste0("CONDVAR.par.",pardata$parname[j],".given.cov.",covariate[i]), names(frem_condvar))]
        } else {
          DF[,i] <- frem_condvar[ , grepl(paste0("CONDVAR.par.",pardata$parname[j],".given.",covariate[i]), names(frem_condvar))]
        }
      }
      colnames(DF) <- covariate
    
      # PREPARE DATA FRAME FOR PLOTTING ----------------------------------------
      # Create long data set
      DF_melt <- melt(DF)
      
      # summaryze dataframe and calculate mean, quantile for each group (for each column in DF)
      outTable <- DF_melt %>% group_by(variable) %>%
        summarise(mean = value[1],
                  perc_5th = quantile(value[-1], probs=c(0.05),type=2),
                  perc_95th = quantile(value[-1], probs=c(0.95),type=2))
      # add some needed columns for plotting
      outTable$unexp_var <- sprintf("%.3G [%.3G, %.3G]",outTable$mean,outTable$perc_5th,outTable$perc_95th)
      outTable$round_mean <- round(outTable$mean,3)
      outTable$covariates <- covariate
      
      
      # add an empty row to outTable
      empty_row <- c(rep(NA,ncol(outTable)))
      outTable <-rbind(empty_row,outTable)
      
      outTable$y <- factor(c(1:nrow(outTable)), levels=c(nrow(outTable):1))
      # MAKE FOREST PLOT --------------------------------------------------------
      p <- ggplot(outTable, aes(mean,y)) +
        geom_point(size = 2) +
        geom_text(aes(label = round_mean),size = 4, vjust = 0, nudge_y = 0.1) +
        geom_errorbarh(aes(xmax = perc_95th, xmin = perc_5th, height = 0.15)) +
        geom_vline(xintercept = 0, linetype = "longdash") +
        labs(x = "Conditional variability", y="") +
        theme_bw() +
        theme(legend.position = "none",
              panel.border = element_rect(),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              axis.line = element_line(),
              axis.ticks = element_blank(),
              axis.text.y = element_blank(),
              plot.margin = unit(c(1,0.1,1,1), "cm"))
      
      # create table with all needed information
      outTable <- outTable[-1,]
      outTable_text <- data.frame()
      V1 <- c("COVARIATE",outTable$covariates,"UNEXPLANED VARIABILITY",outTable$unexp_var)
      V05 <- rep(c(1:2),each = (nrow(outTable) +1) )
      outTable_text <- data.frame(V1,V05,V0 = factor(rep(c(1:(nrow(outTable) +1)),2),levels = c((nrow(outTable) +1):1)))
      
      # create plot of text table
      data_table <- ggplot(outTable_text,aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
        geom_text(size = 4, hjust = 0, vjust = 0.2) + theme_bw() +
        geom_hline(aes(yintercept = c(nrow(outTable) + 0.5))) +
        theme(panel.grid.major = element_blank(),
              panel.border = element_blank(),
              legend.position = "none",
              axis.text.x = element_text(size = 12, colour = "white"),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              plot.margin = unit(c(1,1,1,0), "cm")) +
        labs(x="",y="") +
        coord_cartesian(xlim = c(1,3))
      
      # Create title in the plot
      title <- paste0("Unexplaned variability on parameter ",parameter_names[j])
      
      # print out forest plot with table text
      unexplaned_variability_plots[[j]] <- arrangeGrob(p, data_table, ncol=2, top = textGrob(title,gp=gpar(fontsize=20)))
      
      # Save each plot with different names in different pdg files (based on each parameter j)
      param[[j]] <- paste0("UNEXPL.VAR.",parameter_names[j])
      
    }
    return(list(plots=unexplaned_variability_plots,
                param=param))
  } else {
    cat("Input data files are not found! Make sore that input data files are in your working directory!")
  }
}