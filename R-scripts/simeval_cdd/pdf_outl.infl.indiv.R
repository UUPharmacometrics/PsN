pdf_outl.infl.indiv <- function(all.iofv.file,n.subjects,samples,raw.results.file,skipped.id.file,
                                residual.outliers.file,ebe.npde.file,eta.names,
                                pdf.filename,cutoff_cook,cutoff_delta.ofv) {
  outlying_criteria <- 3 # no -3, because we compare max(abs(EBE_NPDE values)) always positive
  ##################################################    delta odv and simeval OFV  ##########################################################
  # Compare outliers and influential individuals
  outdata <- influential_outliers_data(all.iofv.file,n.subjects,samples,
                                       raw.results.file,skipped.id.file,cutoff_delta.ofv) # use function
  # unlist
  table_for_plot <- outdata$table_for_plot
  ID <- outdata$ID
  row <- outdata$row
  infl_outl <- outdata$infl_outl
  infl_not_outl <- outdata$infl_not_outl
  outl_not_infl <- outdata$outl_not_infl
  not_outl_not_infl <- outdata$not_outl_not_infl
  fail_ID_text <- outdata$fail_ID_text
  deleted_outliers_text <- outdata$deleted_outliers_text
  row_outl_not_infl <- outdata$row_outl_not_infl
  row_infl_not_outl <- outdata$row_infl_not_outl
  
  # create pdf file
  pdf(file=pdf.filename,width=10, height=7)
  # plot points, mark outliers and influential individuals ir red
  plot_infl_outl_data(table_for_plot,ID,row,row_outl_not_infl,row_infl_not_outl,fail_ID_text,deleted_outliers_text,cutoff_delta.ofv)
  
  # plot summary about how many are only influential, only outliers, both and none.
  plot_summary(infl_outl,infl_not_outl,outl_not_infl,not_outl_not_infl)
  
  ####################################################     cook.score and simeval EBE NPDE    ################################################
  # u distribution (in Perl calculated statistics::Distributions::udistr(1/successful.samples)), outlying_criteria
  
  list_data <- ebe_cook.score_data(ebe.npde.file,eta.names,outlying_criteria,
                                   raw.results.file,skipped.id.file,cutoff_cook)
  
  if(class("list_data") == "list") {
    #unlist
    par_table_for_plot <- list_data$table_for_plot
    par_ID <- list_data$ID
    par_row <- list_data$row
    par_infl_outl <- list_data$infl_outl
    par_infl_not_outl <- list_data$infl_not_outl
    par_outl_not_infl <- list_data$outl_not_infl
    par_not_outl_not_infl <- list_data$not_outl_not_infl
    par_row_infl_not_outl <- list_data$row_infl_not_outl
    par_row_outl_not_infl <- list_data$row_outl_not_infl
    # plot points, mark outliers and influential individuals ir red
    plot_param_infl_outl_data(par_table_for_plot,par_ID,par_row,par_row_outl_not_infl,par_row_infl_not_outl,cutoff_cook,outlying_criteria)
    
    # plot summary about how many are only influential, only outliers, both and none.
    plot_summary(par_infl_outl,par_infl_not_outl,par_outl_not_infl,par_not_outl_not_infl)
  } else {
    no.cook.cov <- list_data
  }
  
  ###################################################   summary ourlier influential indiv table  ###########################################################
  if(!no.cook.cov) {
    table_list <- outlier_infl_table(all.iofv.file,n.subjects,samples,ebe.npde.file,eta.names,outlying_criteria,
                                     residual.outliers.file,raw.results.file,skipped.id.file,cutoff_delta.ofv)
    all_infl_indiv_table <- table_list$all_infl_indiv_table
    all_outlier_table <- table_list$all_outlier_table
    outl_infl_table <- table_list$outl_infl_table
    col_amount <- table_list$col_amount
    
    # draw the table 
    if((nrow(outl_infl_table) == 1) && (ncol(outl_infl_table)==1)) {
      plot.table(outl_infl_table)
    } else {
      total_rows_per_page <- 18
      start_row <- 1
      if (total_rows_per_page > nrow(outl_infl_table)) {
        end_row <- nrow(outl_infl_table)
      } else {
        end_row <- total_rows_per_page
      }
      
      for (i in 1:ceiling(nrow(outl_infl_table)/total_rows_per_page)){
        outl_infl_table_part <- outl_infl_table[start_row:end_row,]
        
        # add text "continues", if table continues in other pages
        nr_col <- ncol(outl_infl_table_part)
        add_vect <- c(rep("",nr_col-1),"continues...")
        if(i != ceiling(nrow(outl_infl_table)/total_rows_per_page)) {
          outl_infl_table_part <- rbind(outl_infl_table_part,add_vect)
        }
        
        start_row <- end_row + 1
        if((total_rows_per_page + end_row) < nrow(outl_infl_table)){
          end_row <- total_rows_per_page + end_row
        }else {
          end_row <- nrow(outl_infl_table)
        }
        tab <- tableGrob(outl_infl_table_part, rows=NULL)
        header <- tableGrob(outl_infl_table_part[1, 1:3], rows=NULL, cols=c("","SIMEVAL outliers", "CDD influentials"))  # extra header
        
        jn <- combine(header[1,], tab, along=2)
        # jn$widths <- rep(max(jn$widths), length(jn$widths)) # make column widths equal
        
        # change the relevant rows of gtable
        if(col_amount==8) {
          jn$layout[1:6 , c("l","r")] <- list(c(1,2,6),c(1,5,8))
        } else {
          jn$layout[1:6 , c("l","r")] <- list(c(1,2,4),c(1,3,6))
        }
        
        grid.newpage()
        grid.draw(jn)
      }
    }
  }
  
  dev.off()
}