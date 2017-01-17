influential_outliers_data <- function(all.iofv.file,n.subjects,samples,
                                      raw.results.file,skipped.id.file,cutoff_delta.ofv) {
  # GET OUTLIERS FROM SIMEVAL 
  # simeval (iOFV RES) ------------------------------------------------------------
  list_i_ofv_res <- i_ofv_res(all.iofv.file,n.subjects,samples) # use function
  simeval_id_sorted <- list_i_ofv_res$id_sorted
  iofv_res <- list_i_ofv_res$result$x
  outlier_ID <- list_i_ofv_res$outlier_ID
  ID_deleted_ofv <- list_i_ofv_res$ID_deleted_ofv
  
  # create a data frame
  outl_data <- as.data.frame(cbind(simeval_id_sorted,iofv_res))
  colnames(outl_data) <- c("ID_simeval","iofv_res")
  
  # GET INFLUENTIAL INDIVIDUALS FROM CDD
  # cdd (create.data.full.R, delta.ofv.data.R) --------------------------------------------------------------------------
  out_cdd.data.all <- create.data.full(raw.results.file,skipped.id.file) # use function
  cdd.data.all <- out_cdd.data.all$cdd.data.all

  list_ofv_cdd <- delta.ofv.data(cdd.data.all,cutoff_delta.ofv) # use function
  data_plot <- list_ofv_cdd$data_plot
  ID_infl <- list_ofv_cdd$ID_infl
  fail_ID <- list_ofv_cdd$fail_ID
  
  # save needed columns in one data frame
  infl_data <- data_plot[,1:2]
  colnames(infl_data) <- c("ID_cdd","delta.ofv")
  
  # check if all ID numbers exist
  deleted_outliers <- integer(0)
  if(length(fail_ID)>0) {
    if (length(outlier_ID)>0) {
      deleted_outliers <- intersect(outlier_ID,fail_ID)
      outlier_ID <- setdiff(outlier_ID,deleted_outliers)
    }
    delete_rows <- which(outl_data$ID_simeval %in% fail_ID)
    outl_data <- outl_data[-delete_rows,]
    rownames(outl_data) <- NULL
  }
  
  # cdd and simeval ------------------------------------------------------------------------
  # find which individuals are infuencial and outliers
  ID <- intersect(outlier_ID,ID_infl)

  #order influential individual table and outlier table by iD numbers
  ID_ordered_influential_table <- infl_data[order(infl_data$ID_cdd),]
  rownames(ID_ordered_influential_table) <- NULL
  ID_ordered_outlier_table <- outl_data[order(outl_data$ID_simeval),]
  rownames(ID_ordered_outlier_table) <- NULL
  
  # create a table for plotting
  table_for_plot <- as.data.frame(cbind(ID_ordered_influential_table$ID_cdd,ID_ordered_influential_table$delta.ofv,ID_ordered_outlier_table$iofv_res))
  colnames(table_for_plot) <- c("ID","cdd_delta.ofv","simeval_iofv_res")
  
  #rows of infl and outl individuals
  row <- c()
  for (i in 1:length(ID)) {
    row[i] <- which(table_for_plot$ID %in% ID[i])
  }
  
  # which ID are influential and outliers, influential but not outliers, --------------
  # outliers but not influential, not outliers and not influential
  infl_outl <- ID
  infl_not_outl <- setdiff(ID_infl,ID)
  outl_not_infl <- setdiff(outlier_ID,ID)
  not_outl_not_infl <- setdiff(table_for_plot$ID,unique(c(outlier_ID,ID_infl)))
  
  #rows of influential indiv but not outliers
  row_infl_not_outl <- c()
  if(length(infl_not_outl) >0) {
    for (i in 1:length(infl_not_outl)) {
      row_infl_not_outl[i] <- which(table_for_plot$ID %in% infl_not_outl[i])
    }
  }
  row_outl_not_infl <- c()
  if(length(outl_not_infl) > 0) {
    for (i in 1:length(outl_not_infl)) {
      row_outl_not_infl[i] <- which(table_for_plot$ID %in% outl_not_infl[i])
    }
  }
  
  # order fail_ID and deleted_outliers, save as text
  fail_ID_text <- ''
  if (length(fail_ID) > 0) {
    fail_ID <- sort(fail_ID)
    for (i in 1:length(fail_ID)) {
      if(i == 1) {
        fail_ID_text <- fail_ID[i]
      } else {
        fail_ID_text <- paste0(fail_ID_text,", ",fail_ID[i])
      }
    }
  }
  deleted_outliers_text <- ''
  if(length(deleted_outliers) > 0) {
    deleted_outliers <- sort(deleted_outliers)
    for (i in 1:length(deleted_outliers)) {
      if(i == 1) {
        deleted_outliers_text <- deleted_outliers[i]
      } else {
        deleted_outliers_text <- paste0(deleted_outliers_text,", ",deleted_outliers[i])
      }
    }
  }
  
  # return
  out <- list(infl_data=infl_data,
              outl_data=outl_data,
              table_for_plot=table_for_plot,
              ID=ID,
              row=row,
              infl_outl=infl_outl,
              infl_not_outl=infl_not_outl,
              outl_not_infl=outl_not_infl,
              not_outl_not_infl=not_outl_not_infl,
              fail_ID_text=fail_ID_text,
              deleted_outliers_text=deleted_outliers_text,
              row_infl_not_outl=row_infl_not_outl,
              row_outl_not_infl=row_outl_not_infl)
  return(out)
}