create.data <- function(input_cdd.data,cdd.inds) {
  # check if exist column method
  if ("method" %in% colnames(input_cdd.data)) {
    # remember which rows don't have method cdd (maybe will be needed)
    not.cdd.method <- which(input_cdd.data$method != "cdd")
    # subset needed data (where method is cdd)
    input_cdd.data <- subset(input_cdd.data, method == "cdd") 
  }
  
  # give a name to column with ID numbers
  names(cdd.inds)[1] <- "ID"
  
  ## skip first record (no cases deleted)
  cdd.data <- input_cdd.data[-1,]
  # bind ID column with all data file
  cdd.data <- cbind(cdd.data,cdd.inds[1])
  row.names(cdd.data) <- NULL
  return(cdd.data)
}