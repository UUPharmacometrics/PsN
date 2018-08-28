ID_ratio <- function(frem_id,covdata,pardata) {

  # check if there are all 3 input data files
  files_exist <- (exists("frem_id") & exists("covdata") & exists("pardata"))
  if (files_exist) {

    library(grid)
    library(gridExtra)
    library(dplyr)
    library(ggplot2)

    # Half of numbers of ID shown in plots.
    EXTR_ID_NUM = 10

    # names of parameter (names of first column in pardata input table,header = FALSE)
    parameter_names <- pardata[[1]]
    parameter_names <- make.names(parameter_names)
    # names of covariate (names of first column in covdata input table,header = FALSE)
    covariate <- as.character(covdata[[1]])
    
    # delete "LN" prefixes (if they exist) from any of covariates
    for (i in 1:length(covariate)) {
      if (grepl("^LN", covariate[i])) {
        covariate[i] <- gsub("\\LN","",covariate[i])
        covdata[i,2:5] <- exp(covdata[i,2:5])
      }
    }
    
    # delete "LN" prefixes (if they exist) from any of colnames in frem_id data frame
    col_names <- colnames(frem_id)
    for (i in 1:length(col_names)) {
      if (grepl("^cov\\.LN",col_names[i])) {
        col_names[i] <- gsub("\\LN","",col_names[i])
        frem_id[i] <- exp(frem_id[i])
      }
    }
    colnames(frem_id) <- col_names

    # change values of categorical covariates based on references in covdata
    for (i in 1:nrow(covdata)) {
      if (covdata$is.categorical[i] == "1" && (!is.na(covdata$category.reference[i]) && !is.na(covdata$category.other[i]))) {
        col_nr <- grep(paste0("cov\\.",covariate[i]),colnames(frem_id))
        categ_col <- frem_id[col_nr]
        for (n in 1:nrow(categ_col))
          if (categ_col[n,] == covdata$reference[i]) {
            categ_col[n,] <- covdata$category.reference[i]
          } else {
            categ_col[n,] <- covdata$category.other[i]
          }
        frem_id[col_nr] <- categ_col
      }
    }

    # round values to reasonable amount of significant figures (4 as maximum)
    for (i in 1:nrow(covdata)) {
      if (covdata$is.categorical[i] == "0") {
        col_nr <- grep(paste0("cov\\.",covariate[i]),colnames(frem_id))
        cont_col <- frem_id[col_nr]
        for (n in 1:length(cont_col)) {
            cont_col[n] <- signif(cont_col[n], digits=4)
        }
        frem_id[col_nr] <- cont_col
        # covdata contains percentiles and means
        covdata[i, 2:5] <- signif(covdata[i, 2:5], digits=4)
      }
    }

    # SORT NEEDED DATA FOR EACH PARAMETER -------------------------------------
    indiv_for_param_plots <- list()
    param <- list()
    for (j in 1:length(parameter_names)) {
      # Sort data into neat order
      obs_col <- paste0(parameter_names[j], ".observed")
      frem_id <- frem_id[order(frem_id[, obs_col]),]
      rownames(frem_id) <- NULL
      num_ids <- nrow(frem_id)

      # Build new data frame from original data
      DF <- data.frame(id = frem_id[, "ID"],
                       mean = frem_id[, obs_col]*100-100,
                       ci_low = frem_id[, paste0(parameter_names[j], ".5th")]*100-100,
                       ci_high = frem_id[, paste0(parameter_names[j],".95th")]*100-100)
      DF$interval <- sprintf("%.2f %%  [%.2f, %.2f]", DF$mean, DF$ci_low,DF$ci_high)
      DF$group <- "other"

      # Make space for typical ID
      empty <- c(NA,NA,NA,NA,NA,NA)

      if (num_ids > (EXTR_ID_NUM * 2) ) {
        DF_low <- DF[1:EXTR_ID_NUM,]
        DF_high <- DF[(nrow(DF)-(EXTR_ID_NUM-1)):nrow(DF),]
        DF <- rbind(DF_low, empty, DF_high)
      } else {
        low_nr <- 1:(nrow(DF)/2)
        DF_low <- DF[low_nr,]
        DF_high <- DF[(max(low_nr)+1):nrow(DF),]
        DF <- rbind(DF_low, empty, DF_high)
      }
      rownames(DF) <- NULL
      # Save information about subset (for table creation later)
      num_ids <- nrow(DF)

      # Add two empty rows (because we're putting the typical individual in the table here)
      empty_row <- c(rep(NA,ncol(DF)))
      DF <-rbind(empty_row,DF)

      # Create that y thingy column (what is it good for?)
      DF$y <- factor(c(1:(num_ids+1)), levels=c((num_ids+1):1))

      # MAKE FOREST PLOT
      p <- ggplot(DF, aes(mean,y)) +
        geom_point(aes(color = group, shape = group),size = 2) +
        geom_errorbarh(aes(xmax = ci_high, xmin = ci_low, color = group, height = 0.15)) +
        scale_colour_manual(values = c("other" = "gray30")) +
        geom_vline(xintercept = 0, linetype = "longdash", alpha=0.4) +
        labs(x = "Effect size in percentage (%)", y="") +
        theme_bw() +
        theme(legend.position = "none",
              panel.border = element_rect(),
              axis.title = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_blank(),
              axis.line = element_line(),
              axis.ticks = element_blank(),
              plot.margin = unit(c(1,0.1,1,1), "cm"))

      # Create table for table output
      DF_text <- data.frame()
      
      # Remember that we might have sampled the original data
      selected_rows <- which(frem_id$ID %in% DF$id, arr.ind=TRUE)
      frem_id_subset <- frem_id[selected_rows,]
      # devide data set in 2 peaces
      lower <- c(1:((length(selected_rows))/2))
      upper <- c((max(lower)+1):(length(selected_rows)))
      # Create first column, the ID column
      col1 <- c("ID",frem_id_subset[lower,"ID"], "typical",frem_id_subset[upper,"ID"])
      DF_text <- data.frame(V1 = col1, V05 = 1)
      column_size <- nrow(DF_text)
      ncolumns <- 1

      # Create one column per each covariate
      for (cov_num in 1:length(covariate)) {
        cov_name <- covariate[cov_num]
        # Construct column name, typical value and values from each id
        col_name <- paste0("cov.", cov_name)
        if (covdata$is.categorical[cov_num] != "1") {
          typ_value <- median(frem_id[,col_name])
        } else {
          typ_value <- names(which.max(table(frem_id[,col_name])))
        }
        # TODO: Fix median thingy should be frequency instead
        id_values <- frem_id_subset[, col_name]

        # Construct new columns and append them to data frame
        V1 <- c(cov_name, id_values[lower],typ_value, id_values[upper])
        DF_cov <- data.frame(V1 = V1, V05 = cov_num + 1)
        DF_text <- rbind(DF_text, DF_cov)

        # Keep track on how many columns we've gathered
        ncolumns <- ncolumns + 1
      }

      # Construct that weird factor column that we apparently need
      DF_text$V0 <- factor(rep(1:column_size, ncolumns), levels = column_size:1)
      DF_text$group <- rep(c("title",rep("other",length(lower)),"typical",rep("other",length(upper))), ncolumns)

      # create plot of text table
      t <- ggplot(DF_text,aes(x = V05, y = V0, label = format(V1))) +
        geom_text(size = 4, hjust = 0, vjust = 0.5, aes(colour = group)) +
        scale_colour_manual(values = c("title" = "black", "typical" = "blue", "other" = "gray30")) +
        theme_bw() +
        geom_hline(aes(yintercept = c(column_size - 0.5))) +
        theme(panel.grid.major = element_blank(),
              panel.border = element_blank(),
              legend.position = "none",
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12, colour = "white"),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              plot.margin = unit(c(1,1,1,0), "cm")) +
        labs(x="",y="") +
        coord_cartesian(xlim = c(1,ncolumns + 0.5))

      # print out forest plot with table text
      if (ncolumns >= 16) {
        indiv_for_param_plots[[j]] <- arrangeGrob(p, t, ncol = 2,top = textGrob(paste0("Individuals for parameter ", parameter_names[j]), gp = gpar(fontsize=20)), widths = c(1:2))
      } else if ((ncolumns >= 8) && (ncolumns < 16)) {
        indiv_for_param_plots[[j]] <- arrangeGrob(p, t, ncol = 2,top = textGrob(paste0("Individuals for parameter ", parameter_names[j]), gp = gpar(fontsize=20)), widths = c(2:3))
      } else {
        indiv_for_param_plots[[j]] <- arrangeGrob(p, t, ncol = 2,top = textGrob(paste0("Individuals for parameter ", parameter_names[j]), gp = gpar(fontsize=20)))
      }

      # Save each plot with different names in different pdf files (based on each parameter j)
      param[[j]] <- paste0("ID.",parameter_names[j])
      
    }
    return(list(plots=indiv_for_param_plots,
                param=param))
    
  } else {
    cat("Input data files are not found! Make sore that input data files are in your working directory!")
  }
  
}
