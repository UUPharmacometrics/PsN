sd_unexpl_var <- function(sd_coef_summary,covdata,pardata) {
  # check if there are all 3 input data files
  files_exist <- (exists("sd_coef_summary") & exists("covdata") & exists("pardata"))
  if (files_exist) {
    library(grid)
    library(gridExtra)
    library(dplyr)
    library(ggplot2)
    
    # in case if column names consist of not valid symbols, for example, "("
    parameter_names <- pardata[,1]
    parameter_names <- make.names(parameter_names)

    # names of covariate (names of first column in covdata input table, header = FALSE)
    covariate <- as.character(covdata[[1]])
    covariate <- c("none",covariate,"allcov")
    
    # delete "LN" prefixes (if they exist) from any of covariates
    for (i in 1:length(covariate)) {
      if (grepl("^LN", covariate[i])) {
        covariate[i] <- gsub("\\LN","",covariate[i])
      }
    }
    
    #check if uncertanty exists
    uncertainty <- TRUE
    if(all(is.na(sd_coef_summary$sd.5th))) {
      uncertainty <- FALSE
    }
    
    # delete "LN" prefixes (if they exist) in the sd_coeficient_summary table
    for (i in 1:nrow(sd_coef_summary)) {
      if (grepl("\\.LN",sd_coef_summary[i,1])) {
        sd_coef_summary[i,1] <- gsub("\\.LN",".",sd_coef_summary[i,1])
      }
    }
    
    # SORT NEEDED DATA FOR EACH PARAMETER -------------------------------------
    sd_unexpl_var_plots <- list()
    param <- list()
    for (j in 1:length(parameter_names)) {
      DF <- sd_coef_summary %>%
        filter(grepl(paste0("^",parameter_names[j],"\\."), .$par.conditionedOn)) %>%
        select(1:4)
      DF[,3] <- as.numeric(DF[,3])
      DF[,4] <- as.numeric(DF[,4])
      DF[,1] <- sub(paste0(parameter_names[j],"\\."),'',DF[,1])
      
      if(uncertainty) {
        DF$expected <- sprintf("%.3G [%.3G, %.3G]",DF$observed.sd,DF$sd.5th,DF$sd.95th)
      } else {
        DF$expected <- sprintf("%.3G",DF$observed.sd)
      }
      DF$round_obs.sd <- round(DF$observed.sd,3)
      DF$group <- c(rep("all",nrow(DF)))
      # add an empty row to DF
      empty_row <- c(rep(NA,ncol(DF)))
      DF <-rbind(empty_row,DF)
      DF$y <- factor(c(1:nrow(DF)), levels=c(nrow(DF):1))
      
      # MAKE FOREST PLOT --------------------------------------------------------
      p <- ggplot(DF, aes(observed.sd,y)) +
        geom_point(aes(color = group),size = 2) +
        geom_text(aes(label = round_obs.sd,color = group),size = 4, vjust = 0, nudge_y = 0.1) +
        scale_colour_manual(values = c("all" = "gray30")) +
        geom_errorbarh(aes(xmax = sd.95th, xmin = sd.5th, color = group, height = 0.15)) +
        geom_vline(xintercept = 0, linetype = "longdash", alpha=0.4) +
        labs(x = "SD of unexplained variability", y="") +
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
      DF <- DF[-1,]
      DF_text <- data.frame()
      V1 <- c("COVARIATE",DF$par.conditionedOn,"EXPECTED SD",DF$expected)
      V05 <- rep(c(1:2),each = (nrow(DF) +1) )
      DF_text <- data.frame(V1,V05,V0 = factor(rep(c(1:(nrow(DF) +1)),2),levels = c((nrow(DF) +1):1)))
      DF_text$group <- rep(c("title",rep("all",nrow(DF))),2)
      
      # create plot of text table
      data_table <- ggplot(DF_text,aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
        scale_colour_manual(values = c("title" = "black", "all" = "gray30")) +
        geom_text(size = 4, hjust = 0, vjust = 0.2, aes(colour = group)) + theme_bw() +
        geom_hline(aes(yintercept = c(nrow(DF) + 0.5),colour = group)) +
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
      title <- paste0("Unexplained variability on ",parameter_names[j])
      
      # print out forest plot with table text
      sd_unexpl_var_plots[[j]] <- arrangeGrob(p, data_table, ncol=2, top = textGrob(title,gp=gpar(fontsize=20)))
      
      # Save each plot with different names in different pdg files (based on each parameter j)
      param[[j]] <- paste0("SD.OF.UNEXPL.VAR.",parameter_names[j])
      
    }
    return(list(plots=sd_unexpl_var_plots,
                param=param))
  } else {
    cat("Input data files are not found! Make sore that input data files are in your working directory!")
  }
}
