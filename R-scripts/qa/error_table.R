error_table <- function(first_column="",col=2,column_names) {
  if(missing(column_names)) {
    if(col>=2) column_names <- c("","dofv",rep("",(col-2)))
    if(col<2) column_names <- ""
  }
  nr_rows <- length(first_column)
  if(col > 2) {
    error.table <- data.frame(matrix(c("ERROR",rep("",col-2)),1,(col-1)),stringsAsFactors = F)
    error.table <- cbind(first_column,error.table)
    colnames(error.table) <- column_names
  } else if(col == 2){
    error.table <- data.frame(first_column,"ERROR",stringsAsFactors = F)
    colnames(error.table) <- column_names
  } else {
    error.table <- data.frame("ERROR",stringsAsFactors = F)
    colnames(error.table) <- column_names
  }
  return(error.table)
}