parameter_ratio <- function(inTable_frem,covdata,pardata) {

  # check if there are all 3 input data files
  files_exist <- (exists("inTable_frem") & exists("covdata") & exists("pardata"))
  if (files_exist) {
    library(grid)
    library(gridExtra)
    library(dplyr)
    library(ggplot2)
    library(tidyr)

    # in case if column names consist of not valid symbols, for example, "("
    parameter_names <- pardata[,1]
    parameter_names <- make.names(parameter_names)

    # colnames of frem data
    colnames_frem <- colnames(inTable_frem)
    # names of covariate (names of first column in covdata input table, header = FALSE)
    covariate <- as.character(covdata[[1]])

    # delete "LN" prefixes (if they exist) from any of covariates
    for (i in 1:length(covariate)) {
      if (grepl("^LN", covariate[i])) {
        covariate[i] <- gsub("\\LN","",covariate[i])
        covdata[i,2:5] <- exp(covdata[i,2:5])
      }
    }

    # delete "LN" prefixes (if they exist) from any of colnames in inTable_frem data frame
    col_names <- colnames(inTable_frem)
    for (i in 1:length(col_names)) {
      if (grepl("\\.LN",col_names[i])) {
        col_names[i] <- gsub("\\.LN",".",col_names[i])
      }
    }
    colnames(inTable_frem) <- col_names

    # round values to reasonable amount of significant figures (4 as maximum)
    for (i in 1:nrow(covdata)) {
      if (covdata$is.categorical[i] == "0") {
        # covdata contains percentiles and means
        covdata[i, 2:5] <- signif(covdata[i, 2:5], digits=4)
      }
    }

    # CREATE VECTOR OF POINT (MEAN) NAMES IN PLOT -----------------------------
    list_v <- list() # for pont names in plot
    list_color <- list() # for point colors in plot
    list_mean <- list() # for text in left side of plot
    list_covariate <- list()

    for (v in 1:nrow(covdata)) {
      if (covdata$is.categorical[v] != "1") {
        r <- rbind(covdata$perc5th[v],covdata$perc95th[v])
        p_color <- rbind("cov5th", "cov95th")
        m <- rbind(paste0(format(signif(covdata$mean[v], digits=2))," ",covdata$unit[v]),"")
        c <- rbind(covariate[v],"")
      } else {
        if(is.na(covdata$category.other[v]) || is.na(covdata$category.reference[v])) {
          m <- covdata$reference[v]
          if(covdata$reference[v] == covdata$perc95th[v]) {
            r <- covdata$perc5th[v]
          } else {
            r <- covdata$perc95th[v]
          }
        } else {
          r <- covdata$category.other[v]
          m <- covdata$category.reference[v]
        }
        p_color <- "other"
        c <- covariate[v]
      }
      list_v[v] <- list(r)
      list_color[v] <- list(p_color)
      list_mean[v] <- list(m)
      list_covariate[v] <- list(c)
    }
    point_names <- as.vector(do.call(rbind,list_v))
    point_color <- as.vector(do.call(rbind,list_color))
    MEAN <- as.vector(do.call(rbind,list_mean))
    COVARIATE <- as.vector(do.call(rbind,list_covariate))

    # SORT NEEDED DATA FOR EACH PARAMETER -------------------------------------
    list_part <- list()
    list_colnames <- list()
    cov_effect_on_param_plots <- list()
    param <- list()
    for (j in 1:length(parameter_names)) {
      for (i in 1:length(covariate)) {
        if (covdata$is.categorical[i] != "1") {
          part_5th <- inTable_frem[ , grepl(paste0("^RATIO\\.par\\.",parameter_names[j],"\\.given\\.cov5th\\.",covariate[i],"$"), names(inTable_frem))]
          name_5th <- paste0(covariate[i],".cov5th")
          part_95th <- inTable_frem[ , grepl(paste0("^RATIO\\.par\\.",parameter_names[j],"\\.given\\.cov95th\\.",covariate[i],"$"), names(inTable_frem))]
          name_95th <- paste0(covariate[i],".cov95th")
          part <- cbind(part_5th,part_95th)
          name <- cbind(name_5th,name_95th)
          list_part[i] <- list(part)
          list_colnames[i] <- list(name)
        } else {
          part <- inTable_frem[ , grepl(paste0("^RATIO\\.par\\.",parameter_names[j],"\\.given\\.other\\.",covariate[i],"$"), names(inTable_frem))]
          name <- paste0(covariate[i],".other")
          list_part[i] <- list(part)
          list_colnames[i] <- list(name)
        }
      }
      ColNames <- unlist(list_colnames)
      DF <- do.call(cbind,list_part) # matrix for each parameter
      colnames(DF) <- ColNames
      DF <- as.data.frame(DF) # data frame for each parameter with column names of covariate

      # PREPARE DATA FRAME FOR PLOTTING ----------------------------------------
      # Create long data set
      DF_melt <- tidyr::gather(DF, variable, value)

      # summaryze dataframe and calculate mean, quantile for each group (for each column in DF)
      outTable <- DF_melt %>% group_by(variable) %>%
      summarise(mean = mean(value),
                ci_low = quantile(value, probs=c(0.05),type=2),
                ci_high = quantile(value, probs=c(0.95),type=2))
      # calculating procentage of outTable
      outTablet_proc <- t(round(t((outTable[, 2:ncol(outTable)])-1) * 100, 2))
      outTable <- cbind(outTable[1],outTablet_proc)
      # add some needed columns for plotting
      if(nrow(inTable_frem)>1) {
        outTable$EXPECTED<-sprintf("%+.3G %%  [%+.3G, %+.3G]",outTable$mean,outTable$ci_low,outTable$ci_high)
      } else {
        outTable$EXPECTED<-sprintf("%+.3G %%",outTable$mean)
      }
      outTable$points <- point_names
      outTable$group <- point_color
      outTable$MEAN <- MEAN
      outTable$COVARIATE <- COVARIATE

      # add an empty row to outTable
      empty_row <- c(rep(NA,ncol(outTable)))
      outTable <-rbind(empty_row,outTable)

      outTable$y <- factor(c(1:nrow(outTable)), levels=c(nrow(outTable):1))
      # MAKE FOREST PLOT --------------------------------------------------------
      p <- ggplot(outTable, aes(mean,y)) +
        geom_point(aes(color = group, shape = group),size = 2) +
        geom_text(aes(label = points, color = group),size = 4, vjust = 0, nudge_y = 0.1) +
        scale_colour_manual(values = c("red","blue","green")) +
        geom_errorbarh(aes(xmax = ci_high, xmin = ci_low, color = group, height = 0.15)) +
        geom_vline(xintercept = 0, linetype = "longdash", alpha=0.4) +
        labs(x = "Effect size in percentage (%)", y="") +
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
      V1 <- c(colnames(outTable)[9], outTable$COVARIATE, colnames(outTable)[8],outTable$MEAN, colnames(outTable)[5], outTable$EXPECTED)
      V05 <- rep(c(1:3),each = (nrow(outTable) +1) )
      outTable_text <- data.frame(V1,V05,V0 = factor(rep(c(1:(nrow(outTable) +1)),3),levels = c((nrow(outTable) +1):1)))
      outTable_text$group <- rep(c("title",rep("all",nrow(outTable))),3)

      # create plot of text table
      data_table <- ggplot(outTable_text,aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
        geom_text(size = 4, hjust = 0, vjust = 0.2,aes(colour = group)) + theme_bw() +
        geom_hline(aes(yintercept = c(nrow(outTable) + 0.5))) +
        scale_colour_manual(values = c("title" = "black", "all" = "gray30")) +
        theme(panel.grid.major = element_blank(),
              panel.border = element_blank(),
              legend.position = "none",
              axis.text.x = element_text(size = 12, colour = "white"),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              plot.margin = unit(c(1,1,1,0), "cm")) +
        labs(x="",y="") +
        coord_cartesian(xlim = c(1,5))

      # Create title in the plot
      title <- paste0("Covariate effects on parameter ",parameter_names[j])

      # print out forest plot with table text
      cov_effect_on_param_plots[[j]] <- arrangeGrob(p, data_table, ncol=2, top = textGrob(title,gp=gpar(fontsize=20)))

      # Save each plot with different names in different pdg files (based on each parameter j)
      param[[j]] <- paste0(parameter_names[j])

    }
    return(list(plots=cov_effect_on_param_plots,
                param=param))
  } else {
    cat("Input data files are not found! Make sore that input data files are in your working directory!")
  }
}

