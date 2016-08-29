delta.ofv.data <- function(cdd.data.all,outlier_ID) {
  # Default for outlier_ID
  if (!missing(outlier_ID)) {
    outlier_ID_all <- outlier_ID
  }
  
  cdd.data <- cdd.data.all[-1,]
  rownames(cdd.data) <- NULL
  
  nr_of_delta.ofv <- length(cdd.data$cdd.delta.ofv)
  # 10% highest values of the delta ofvs
  delta.ofv_10_pr <- sort(cdd.data$cdd.delta.ofv,decreasing=TRUE)[1:round((nr_of_delta.ofv/10),0)]
  # rows of those values
  row_infl_all <- which(cdd.data$cdd.delta.ofv %in% delta.ofv_10_pr)
  # Id numbers of those values
  influential_ID_all <- cdd.data$ID[row_infl_all]
  
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
    out <- list(model=cdd.data$model,
                delta.ofv=cdd.data$cdd.delta.ofv,
                delta.ofv_10_pr=delta.ofv_10_pr,
                row_infl_all=row_infl_all,
                row_outl_all=row_outl_all,
                row_outl_infl=row_outl_infl,
                row_outl=row_outl,
                row_infl=row_infl,
                ID_infl=cdd.data$ID[row_infl],
                ID_outl=cdd.data$ID[row_outl],
                ID_outl_infl=cdd.data$ID[row_outl_infl],
                ID=cdd.data$ID
    )
  } else {
    row_infl <- row_infl_all
    #output for testing
    out <- list(model=cdd.data$model,
                delta.ofv=cdd.data$cdd.delta.ofv,
                delta.ofv_10_pr=delta.ofv_10_pr,
                row_infl=row_infl,
                ID_infl=cdd.data$ID[row_infl],
                ID=cdd.data$ID
    )
  }
return(out)
}