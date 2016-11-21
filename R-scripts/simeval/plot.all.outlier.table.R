plot.all.outlier.table <- function(all_outlier_table) {
  if((nrow(all_outlier_table) == 1) && (ncol(all_outlier_table)==1)) {
    plot.table(all_outlier_table)
  } else {
    total_rows_per_page <- 19
    start_row <- 1
    if (total_rows_per_page > nrow(all_outlier_table)) {
      end_row <- nrow(all_outlier_table)
    } else {
      end_row <- total_rows_per_page
    }
    for (i in 1:ceiling(nrow(all_outlier_table)/total_rows_per_page)){
      all_outlier_table_part <- all_outlier_table[start_row:end_row,]
      
      # add text "continues", if table continues in other pages
      nr_col <- ncol(all_outlier_table_part)
      add_vect <- c(rep("",nr_col-1),"continues...")
      if(i != ceiling(nrow(all_outlier_table)/total_rows_per_page)) {
        all_outlier_table_part <- rbind(all_outlier_table_part,add_vect)
      }
      
      
      start_row <- end_row + 1
      if((total_rows_per_page + end_row) < nrow(all_outlier_table)){
        end_row <- total_rows_per_page + end_row
      }else {
        end_row <- nrow(all_outlier_table)
      }
      
      tab <- tableGrob(all_outlier_table_part, rows=NULL)
      header <- tableGrob(all_outlier_table_part[1, 1:3], rows=NULL, cols=c("","Individual level", "Observation level")) 
      
      jn <- combine(header[1,], tab, along=2)
      # jn$widths <- rep(max(jn$widths), length(jn$widths)) # make column widths equal
      
      # change the relevant rows of gtable
      jn$layout[1:6 , c("l","r")] <- list(c(1,2,4),c(1,3,5))
      
      grid.newpage()
      grid.draw(jn) 
      
    }
  }
}