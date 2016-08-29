cutoff.cov.cook <- function(raw.results.file,skipped.id.file,cutoff_cook,cutoff_cov) {
  
  # add ID numbers to the data set
  out_cdd.data.all <- create.data.full(raw.results.file,skipped.id.file)
  cdd.data.all <- out_cdd.data.all$cdd.data.all
  
  # subset overall cook score and cov ratios column (except 1 row)
  cook.scores <- cdd.data.all$cook.scores[-1]
  cov.ratios <- cdd.data.all$cov.ratios[-1]
  ID <- cdd.data.all$ID[-1]
  
  # get cook scores and cov ratios per individual parameter
  out <- cov.cook.par.data(cdd.data.all)
  # unlist
  cook.par.data <- round(out$cook.par.data,5)
  cov.par.data <- round(out$cov.par.data,5)
  
  # Make data frame for cook score and add ID numbers
  cook.data <- cbind(ID,cook.scores,cook.par.data)
  cov.data <- cbind(ID,cov.ratios,cov.par.data)
  
  # create tables
  library(gridExtra)

# -------------------------------------------  cutoff_cook -------------------------------------
  # Detect cook score which are bigger than cutoff_cook
  if (!missing(cutoff_cook)) {
    cook_outliers <- c()
    j <- 0
    for (i in 1:nrow(cook.data)) {
      if (cook.data$cook.scores[i] > cutoff_cook) {
        j <- j + 1
        cook_outliers[j] <- cook.data$cook.scores[i]
      }
    }
    
    # Subset data frame where cov ratio and cook score are bigger than cutoffs
    if (length(cook_outliers) > 0) {
      cook_outliers_data <- cook.data[cook.data$cook.scores %in% cook_outliers,]
      rownames(cook_outliers_data) <- NULL
    } else {
      cook_outliers_data <- data.frame(C = c("Don't have cook scores bigger than cutoff."))
      names(cook_outliers_data) <- NULL
    }
    
    # rotate column names
    rotate = ttheme_default(colhead=list(fg_params=list(rot=90)))
    
    # draw a table
    plot.table(cook_outliers_data,rotate)
    
    list_out <- list(cook.data=cook.data,
                     cook_outliers=cook_outliers,
                     cook_outliers_data=cook_outliers_data)
  }

  # -------------------------------------------  cutoff_cov -------------------------------------
  # Detect cov ratio which are bigger than cutoff
  if (!missing(cutoff_cov)) {
    cov_outliers <- c()
    j <- 0
    for (i in 1:nrow(cov.data)) {
      if (cov.data$cov.ratios[i] > cutoff_cov) {
        j <- j + 1
        cov_outliers[j] <- cov.data$cov.ratios[i]
      }
    }
    
    if (length(cov_outliers) > 0) {
      cov_outliers_data <- cov.data[cov.data$cov.ratios %in% cov_outliers,]
      rownames(cov_outliers_data) <- NULL
    } else {
      cov_outliers_data <- data.frame(C = c("Don't have cov ratios bigger than cutoff."))
      names(cov_outliers_data) <- NULL
    }
    
    # draw a table
    plot.table(cov_outliers_data,rotate)
    
    list_out <- list(cov.data=cov.data,
                     cov_outliers=cov_outliers,
                     cov_outliers_data=cov_outliers_data)
  }
  
  # output
  if (!missing(cutoff_cook) && !missing(cutoff_cov)) {
    list_out <- list(cook.data=cook.data,
                     cov.data=cov.data,
                     cook_outliers=cook_outliers,
                     cov_outliers=cov_outliers,
                     cook_outliers_data=cook_outliers_data,
                     cov_outliers_data=cov_outliers_data)
  }
  return(list_out)
}
