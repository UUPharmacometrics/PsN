plot.table <- function(table, rotate) {
  library(gridExtra)
  # draw only 20 rows of table per each page
  total_rows_per_page <- 19
  start_row <- 1
  if (total_rows_per_page > nrow(table)) {
    end_row <- nrow(table)
  } else {
    end_row <- total_rows_per_page
  }
  # divide table in so many pages as it is needed
  for (i in 1:ceiling(nrow(table)/total_rows_per_page)){
    
    # add text "continues", if table continues in other pages
    table_plot <- table[start_row:end_row,]
    nr_col <- ncol(table_plot)
    add_vect <- c(rep("",nr_col-1),"continues...")
    if(i != ceiling(nrow(table)/total_rows_per_page)) {
      table_plot <- rbind(table_plot,add_vect)
    }
    
    #plot table
    frame() # need for drawing table on the next page  
    if(packageVersion("gridExtra") < "2.0.0"){
      if(!missing(rotate)){
        grid.table(table_plot,show.rownames=FALSE,theme=rotate)
      } else {
        grid.table(table_plot,show.rownames=FALSE)
      }
       
    } else {
      if(!missing(rotate)) {
        grid.table(table_plot,rows=NULL,theme=rotate)
      } else {
        grid.table(table_plot,rows=NULL)
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
