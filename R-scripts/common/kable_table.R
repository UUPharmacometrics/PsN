kable_table <- function(table,format,...) {
  library(kableExtra)
  table_new <- kable(table,...)
  if(format=="latex") {
    if(ncol(table)>1 && all(is.null(colnames(table)))) {
      table_new <- table_new
    } else {
      table_new <- table_new %>% kableExtra::row_spec(0,bold=T)
    }
  }
  if(format=="html") {
    if(ncol(table)==1 && is.null(colnames(table))) {
      table_new <- table_new %>% kableExtra::row_spec(1,bold=T)
    }
  }
  return(table_new)  
}