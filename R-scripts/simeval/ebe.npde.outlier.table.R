outlier.table.ebe.npde <- function(ebenpde_tmp,eta_names,outlier_id_row) {

  #CREATE FINAL TABLE (use function outlier_table to create a table fortable1)
  if(length(outlier_id_row) > 0){
    ncolunmns <- 3
    #get all eta values
    eta_names <- gsub("\\ETA.","",eta_names)
    all.eta_values <- gsub("\\.","",eta_names)
    
    #summary off all values
    ebenpde_tmp_outliers <- ebenpde_tmp[outlier_id_row,]
    fortable <- data.frame()
    
    # get eta values and eta numbers
    eta_numbers_list <- list()
    eta_values_list <- list()
    for (i in 1:nrow(ebenpde_tmp_outliers)) {
      table_row <- ebenpde_tmp_outliers[i,-c(1,2)]
      eta_values <- c()
      eta_numbers <- c()
      for (j in 1:ncol(table_row)) {
        if(!(is.na(table_row[,j]))) {
          if(length(eta_numbers) == 0) {
            eta_numbers <- paste0(all.eta_values[j])
          } else {
            eta_numbers <- paste0(eta_numbers,",",all.eta_values[j])
          }
          if(length(eta_values) == 0) {
            eta_values <- paste0(round(table_row[,j],3))
          } else {
            eta_values <- paste0(eta_values,",",round(table_row[,j],3))
          }
        }
      }
      # put all values in the ebe npde outlier table
      fortable[i,1] <- as.numeric(ebenpde_tmp_outliers$ID[i])
      fortable[i,2] <- eta_numbers
      fortable[i,3] <- eta_values
    }
#     for(i in 1:noutlier){ 
#       index_text <- as.numeric(outlier_id_row[i]) # plot the ID of the last subjects
#       fortable[i,1] <- as.numeric(ebenpde_tmp$ID[index_text])
#       fortable[i,2] <- as.numeric(format((summary$vector_theor_dist[noutlier-i + 1]-summary$emp_distance_sort[noutlier-i + 1])/sqrt(2),digits=5))
#       fortable[i,3] <- as.numeric(format(summary$emp_distance_sort[noutlier-i + 1],digits=5))
#       for(j in 1:length(eta_names)){  
#         fortable[i,3+j] <- as.numeric(ebenpde_obs[index_text,j])
#       }   
#     }   
    
    # if outliers exist (ONE MORE PAGE OF PDF FILE WITH FINAL TABLE)
    fortable1 <- fortable
    colnames(fortable1) <- c("ID", "Outlying EBE (ETA numbers)","ETA values")
    
  }else{
    # if no outliers (FOURTH PAGE OF PDF FILE WITH FINAL TEXT)
    fortable1 <- data.frame(C = c("No EBE NPDE outliers detected"))
    names(fortable1) <- NULL
  }
  
  return(fortable1)
}
