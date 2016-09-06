outlier.table.ebe.npde <- function(noutlier,outlier_id_row,ebenpde_tmp,ebenpde_obs,index_emp_distance,
                          emp_distance_sort,vector_theor_dist,n.subjects,all.eta.names,n.eta) {
  #summary off all values
  summary <- as.data.frame(cbind(row=index_emp_distance$ix,emp_distance_sort=emp_distance_sort,
                                 vector_theor_dist=as.vector(vector_theor_dist)))
  summary <- summary[summary$row %in% outlier_id_row,]
  #CREATE FINAL TABLE (use function outlier_table to create a table fortable1)
  if(noutlier > 0){
    ncolunmns <- (3+n.eta)
    fortable <-data.frame()
    #fortable <- array(0,c(noutlier,ncolunmns))
    for(i in 1:noutlier){ 
      index_text <- as.numeric(outlier_id_row[i]) # plot the ID of the last subjects
      fortable[i,1] <- as.numeric(ebenpde_tmp$ID[index_text])
      fortable[i,2] <- as.numeric(format((summary$vector_theor_dist[noutlier-i + 1]-summary$emp_distance_sort[noutlier-i + 1])/sqrt(2),digits=5))
      fortable[i,3] <- as.numeric(format(summary$emp_distance_sort[noutlier-i + 1],digits=5))
      for(j in 1:n.eta){  
        fortable[i,3+j] <- as.numeric(ebenpde_obs[index_text,j])
      }   
    }                      
    # if outliers exist (ONE MORE PAGE OF PDF FILE WITH FINAL TABLE)
    fortable1 <- fortable
    colnames(fortable1) <- c("ID", "outlying criteria","MD distance",all.eta.names)
    
  }else{
    # if no outliers (FOURTH PAGE OF PDF FILE WITH FINAL TEXT)
    fortable1 <- data.frame(C = c("No outliers detected"))
    names(fortable1) <- NULL
  }
  
  return(fortable1)
}
