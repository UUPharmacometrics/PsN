plot.table <- function(table, rotate) {
  library(gridExtra)
  # draw only 20 rows of table per each page
  total_rows_per_page <- 20
  start_row <- 1
  if (total_rows_per_page > nrow(table)) {
    end_row <- nrow(table)
  } else {
    end_row <- total_rows_per_page
  }
  # divide table in so many pages as it is needed
  for (i in 1:ceiling(nrow(table)/total_rows_per_page)){
    frame() # need for drawing table on the next page  
    if(packageVersion("gridExtra") < "2.0.0"){
      if(!missing(rotate)){
        grid.table(table[start_row:end_row,],show.rownames=FALSE,theme=rotate)
      } else {
        grid.table(table[start_row:end_row,],show.rownames=FALSE)
      }
       
    } else {
      if(!missing(rotate)) {
        grid.table(table[start_row:end_row,],rows=NULL,theme=rotate)
      } else {
        grid.table(table[start_row:end_row,],rows=NULL)
      }
    }
    start_row <- end_row + 1
    if((total_rows_per_page + end_row) < nrow(table)){
      end_row <- total_rows_per_page + end_row
    }else {
      end_row <- nrow(table)
    }    
  }
}
