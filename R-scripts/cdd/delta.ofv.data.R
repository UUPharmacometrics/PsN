delta.ofv.data <- function(cdd.data.all,cutoff_delta.ofv,outlier_ID) {
  # Default for outlier_ID
  if (!missing(outlier_ID)) {
    outlier_ID_all <- outlier_ID
  }
 
  cdd.data <- cdd.data.all[-1,]
  rownames(cdd.data) <- NULL
  
  #find negative delta ofv values, if exist
  fail_ID <- c()
  if (any(cdd.data$cdd.delta.ofv < 0)) {
    negat.delta.row <- which(cdd.data$cdd.delta.ofv < 0)
    fail_ID <- cdd.data$ID[negat.delta.row]
    cdd.data <- cdd.data[-negat.delta.row,]
  }
  
  row_infl_all <- c()
  delta.ofv_infl <- c()
  for (i in 1:nrow(cdd.data)) {
    if(cdd.data$cdd.delta.ofv[i] > cutoff_delta.ofv) {
      row_infl_all <- c(row_infl_all,i)
    }
  }
  delta.ofv_infl <- cdd.data$cdd.delta.ofv[row_infl_all]
  
  #plotting data
  data_plot <- cdd.data[,c("ID","cdd.delta.ofv","cook.scores")]
  data_plot$cook.scores <- as.numeric(as.character(data_plot$cook.scores))
  rownames(data_plot) <- NULL
  
  #influential data
  ofv_data <- data_plot[,1:2]
  if(length(row_infl_all)>0) {
    infl_ofv <- ofv_data[row_infl_all,]
    rownames(infl_ofv) <- NULL
  } else {
    infl_ofv <- data.frame(C = c("No influential individuals detected"))
    names(infl_ofv) <- NULL
  }
  
#   nr_of_delta.ofv <- length(cdd.data$cdd.delta.ofv)
#   # 10% highest values of the delta ofvs
#   delta.ofv_10_pr <- sort(cdd.data$cdd.delta.ofv,decreasing=TRUE)[1:round((nr_of_delta.ofv/10),0)]
#   # rows of those values
#   row_infl_all <- which(cdd.data$cdd.delta.ofv %in% delta.ofv_10_pr)
#   # Id numbers of those values
#   influential_ID_all <- cdd.data$ID[row_infl_all]
  
  if (exists("outlier_ID_all")) {
    # numbers of rows of outlier_ID
    row_outl_all <- which(cdd.data$ID %in% outlier_ID_all)
    # find those row numbers where subjects are both influential and outliers
    row_outl_infl <- intersect(row_outl_all,row_infl_all)
    
    # only outlier ID numbers and row nr
    row_outl <- setdiff(row_outl_all,row_outl_infl)
    
    # only influential ID numbers and row nr
    row_infl <- setdiff(row_infl_all,row_outl_infl)
    
    #output for testing
    out <- list(data_plot=data_plot,
                delta.ofv_infl=delta.ofv_infl,
                row_infl_all=row_infl_all,
                row_outl_all=row_outl_all,
                row_outl_infl=row_outl_infl,
                row_outl=row_outl,
                row_infl=row_infl,
                ID_infl=cdd.data$ID[row_infl],
                ID_outl=cdd.data$ID[row_outl],
                ID_outl_infl=cdd.data$ID[row_outl_infl],
                fail_ID=fail_ID,
                infl_ofv=infl_ofv
    )
  } else {
    row_infl <- row_infl_all
    
    #output for testing
    out <- list(data_plot=data_plot,
                delta.ofv_infl=delta.ofv_infl,
                row_infl=row_infl,
                ID_infl=cdd.data$ID[row_infl],
                fail_ID=fail_ID,
                infl_ofv=infl_ofv
    )
  }
return(out)
}