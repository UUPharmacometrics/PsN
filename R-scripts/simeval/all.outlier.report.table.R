all.outlier.report.table <- function(all.iofv.file,n.subjects,samples,
                               ebe.npde.file,n.eta,outlying_criteria,
                               residual.outliers.file) {

  # Use functions
  ofv_out <- i_ofv_res(all.iofv.file,n.subjects,samples)
  ofv_outliers <- ofv_out$ofv_outliertable
  
  model.filename <- " " # because we are not going to see the model name
  ebe.npde_outliers <- ebe.npde.outliers(ebe.npde.file,n.eta,outlying_criteria,model.filename)
  
  cwres.iwres_out <- outlier.table(residual.outliers.file)
  cwres.iwres_outliers <- cwres.iwres_out$outliers_count
  
  # ID numbers of all ourliers if they exist
  id <- c()
  if (ncol(ofv_outliers) > 1) {
    ofv_ID <- ofv_outliers$ID
    id <- ofv_ID
  } 
  if (ncol(ebe.npde_outliers) > 1) {
    ebe.npde_ID <- ebe.npde_outliers$ID
    id <- c(id,ebe.npde_ID)
  }
  if (ncol(cwres.iwres_outliers) > 1) {
    cwres.iwres_ID <- as.numeric(as.vector(cwres.iwres_outliers$ID))
    id <- c(id,cwres.iwres_ID)
  }
  
  # create outlier table (if exists)
  if (length(id) > 0) {
    # Unique ID numbers
    id_unique <- unique(id)
    id_unique <- sort(id_unique)
    
    # create all outlier table
    all_outlier_table <- array(" ", c(length(id_unique),5))
    for (n in 1:length(id_unique)) {
      id_nr <- id_unique[n]
      all_outlier_table[n,1] <- id_nr
      
      # Add OFV values
      if (exists("ofv_ID")) {
        if (any(ofv_ID %in% id_nr)) {
          ofv_value_row <- which(ofv_ID == id_nr)
          all_outlier_table[n,2] <- round(ofv_outliers$MEDIAN[ofv_value_row],3)
        } else {
          all_outlier_table[n,2] <- " "
        }
      }
      
      # Add EBE NPDE values
      if (exists("ebe.npde_ID")) {
        if (any(ebe.npde_ID %in% id_nr)) {
          ebe_value_row <- which(ebe.npde_ID == id_nr)
          all_outlier_table[n,3] <- round(ebe.npde_outliers[ebe_value_row,2],3)
        } else {
          all_outlier_table[n,3] <- " "
        }
      }
      
      # Add CWRES and IWRES values
      if (exists("cwres.iwres_ID")) {
        if (any(cwres.iwres_ID %in% id_nr)) {
          residual_value_row <- which(cwres.iwres_ID == id_nr)
          cwres.iwres_outliers$OUTLIERS.IWRES <- as.vector(cwres.iwres_outliers$OUTLIERS.IWRES)
          cwres.iwres_outliers$OUTLIERS.CWRES <- as.vector(cwres.iwres_outliers$OUTLIERS.CWRES)
          all_outlier_table[n,4] <- cwres.iwres_outliers$OUTLIERS.IWRES[residual_value_row]
          all_outlier_table[n,5] <- cwres.iwres_outliers$OUTLIERS.CWRES[residual_value_row]
        } else {
          all_outlier_table[n,4] <- " "
          all_outlier_table[n,5] <- " "
        }
      }
    }
    all_outlier_table <- as.data.frame(all_outlier_table)
    colnames(all_outlier_table) <- c("ID","OFV outliers","EBE NPDE outliers","IWRES outliers","CWRES outliers")
  } else {
    all_outlier_table <- data.frame(C = c("No outliers detected"))
    names(all_outlier_table) <- NULL
  }
  return(all_outlier_table)
}

