influential_outliers_data <- function(all.iofv.file,n.subjects,samples,
                                      raw.results.file,skipped.id.file) {
  # GET OUTLIERS FROM SIMEVAL 
  # simeval (iOFV RES) ------------------------------------------------------------
  list_i_ofv_res <- i_ofv_res(all.iofv.file,n.subjects,samples) # use function
  simeval_id_sorted <- list_i_ofv_res$id_sorted
  iofv_res <- list_i_ofv_res$result$x
  outlier_ID <- list_i_ofv_res$outlier_ID
  
  # create a data frame
  outl_data <- as.data.frame(cbind(simeval_id_sorted,iofv_res))
  colnames(outl_data) <- c("ID_simeval","iofv_res")
  
  # GET INFLUENTIAL INDIVIDUALS FROM CDD
  # cdd (create.data.full.R, delta.ofv.data.R) --------------------------------------------------------------------------
  out_cdd.data.all <- create.data.full(raw.results.file,skipped.id.file) # use function
  cdd.data.all <- out_cdd.data.all$cdd.data.all
  
  list_ofv_cdd <- delta.ofv.data(cdd.data.all) # use function
  delta.ofv <- list_ofv_cdd$delta.ofv
  ID_cdd <- list_ofv_cdd$ID
  ID_infl <- list_ofv_cdd$ID_infl
  
  # save needed columns in one data frame
  infl_data <- as.data.frame(cbind(ID_cdd,delta.ofv))
  colnames(infl_data) <- c("ID_cdd","delta.ofv")
  
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
  
  # count how many are influential and outliers, influential but not outliers, --------------
  # outliers but not influential, not outliers and not influential
  infl_outl <- ID
  infl_not_outl <- setdiff(ID_infl,ID)
  outl_not_infl <- setdiff(outlier_ID,ID)
  not_outl_not_infl <- setdiff(simeval_id_sorted,unique(c(outlier_ID,ID_infl)))

  # return
  out <- list(infl_data=infl_data,
              outl_data=outl_data,
              table_for_plot= table_for_plot,
              ID=ID,
              row=row,
              infl_outl=infl_outl,
              infl_not_outl=infl_not_outl,
              outl_not_infl=outl_not_infl,
              not_outl_not_infl=not_outl_not_infl)
  return(out)
}