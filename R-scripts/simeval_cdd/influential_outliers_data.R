influential_outliers_data <- function(all.iofv.file,n.subjects,samples,ofv_outlier_criteria,
                                      raw.results.file,skipped.id.file) {
  # GET OUTLIERS FROM SIMEVAL 
  # simeval (iOFV RES) ------------------------------------------------------------
  list_i_ofv_res <- i_ofv_res(all.iofv.file,n.subjects,samples,ofv_outlier_criteria) # use function
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
  
  # order cdd data
  cdd_order <- sort(delta.ofv,index.return=TRUE)
  delta.ofv_order <- cdd_order$x
  #order id cdd
  cdd_id_sorted <- array(0,c(length(ID_cdd),1))
  for (i in 1:length(ID_cdd)) {
    cdd_id_sorted[i] <- ID_cdd[cdd_order$ix[i]]  
  }
  # create a data frame
  infl_data <- as.data.frame(cbind(cdd_id_sorted,delta.ofv_order))
  colnames(infl_data) <- c("ID_cdd","delta.ofv")
  
  # cdd and simeval ------------------------------------------------------------------------
  # find which individuals are infuencial and outliers
  ID <- intersect(outlier_ID,ID_infl)
  row_cdd <- which(infl_data$ID_cdd %in% ID)
  row_simeval <- which(outl_data$ID_simeval %in% ID)
  
  # count how many are influential and outliers, influential but not outliers, --------------
  # outliers but not influential, not outliers and not influential
  infl_outl <- ID
  infl_not_outl <- setdiff(ID_infl,ID)
  outl_not_infl <- setdiff(outlier_ID,ID)
  not_outl_not_infl <- setdiff(simeval_id_sorted,unique(c(outlier_ID,ID_infl)))
  
  # return
  out <- list(infl_data=infl_data,
              outl_data=outl_data,
              ID=ID,
              row_cdd=row_cdd,
              row_simeval=row_simeval,
              infl_outl=infl_outl,
              infl_not_outl=infl_not_outl,
              outl_not_infl=outl_not_infl,
              not_outl_not_infl=not_outl_not_infl)
  return(out)
}