all.outlier.report.table <- function(ofv_outliers,ebe.npde_outliers,cwres.iwres_outliers,ID_deleted_ebe,ID_deleted_ofv) {
  if(missing(cwres.iwres_outliers)) {
    col_amount <- 3
  } else {
    col_amount <- 5
  }
  
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
  if(col_amount == 5) {
    if (ncol(cwres.iwres_outliers) > 1) {
      cwres.iwres_ID <- as.numeric(as.vector(cwres.iwres_outliers$ID))
      id <- c(id,cwres.iwres_ID)
    }
  }

  # create outlier table (if exists)
  if (length(id) > 0) {
    # Unique ID numbers
    id_unique <- unique(id)
    id_unique <- sort(id_unique)
    
    # create all outlier table
    all_outlier_table <- array("", c(length(id_unique),col_amount))
    for (n in 1:length(id_unique)) {
      col_nr <- 1
      id_nr <- id_unique[n]
      all_outlier_table[n,col_nr] <- id_nr
      
      col_nr <- col_nr + 1
      #ofv deleted id if exists
      if(length(ID_deleted_ofv)>0) {
        if(any(ID_deleted_ofv %in% id_nr)) {
          all_outlier_table[n,col_nr] <- "NA"
        }
      }
      
      # Add OFV values
      if (exists("ofv_ID")) {
        if (any(ofv_ID %in% id_nr)) {
          ofv_value_row <- which(ofv_ID == id_nr)
          all_outlier_table[n,col_nr] <- round(ofv_outliers[ofv_value_row,2],3)
        }
      }
      
      col_nr <- col_nr + 1
      #ebe deleted id if exists
      if(length(ID_deleted_ebe)>0) {
        if(any(ID_deleted_ebe %in% id_nr)) {
          all_outlier_table[n,col_nr] <- "NA"
        }
      }
      # Add EBE NPDE values
      if (exists("ebe.npde_ID")) {
        if (any(ebe.npde_ID %in% id_nr)) {
          ebe.npde_value_row <- which(ebe.npde_ID == id_nr)
          all_outlier_table[n,col_nr] <- ebe.npde_outliers[ebe.npde_value_row,2]
        }
      }
      
      if(col_amount == 5) {
        col_nr <- col_nr + 1
        # Add CWRES and IWRES values
        if (exists("cwres.iwres_ID")) {
          if (any(cwres.iwres_ID %in% id_nr)) {
            residual_value_row <- which(cwres.iwres_ID == id_nr)
            cwres.iwres_outliers$OUTLIERS.IWRES <- as.vector(cwres.iwres_outliers$OUTLIERS.IWRES)
            cwres.iwres_outliers$OUTLIERS.CWRES <- as.vector(cwres.iwres_outliers$OUTLIERS.CWRES)
            all_outlier_table[n,col_nr] <- cwres.iwres_outliers$OUTLIERS.IWRES[residual_value_row]
            all_outlier_table[n,col_nr+1] <- cwres.iwres_outliers$OUTLIERS.CWRES[residual_value_row]
          }
        }
      }

    }
    all_outlier_table <- as.data.frame(all_outlier_table)
    # save as character
    all_outlier_table <- data.frame(lapply(all_outlier_table, as.character), stringsAsFactors=FALSE) 
    if(col_amount == 5) {
      colnames(all_outlier_table) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","IWRES outliers","CWRES outliers")
    } else {
      colnames(all_outlier_table) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)")
    }
    
  } else {
    all_outlier_table <- data.frame(C = c("No outliers detected"))
    names(all_outlier_table) <- NULL
  }
  return(all_outlier_table)
}






# all.outlier.report.table <- function(ofv_outliers,cwres.iwres_outliers,list_ebe_outlier_table,cases,deleted_ID) {
#   # ebe outliers
#   list_out <- ebe.npde.all.outliers(list_ebe_outlier_table,cases,deleted_ID)
#   ebe.npde_outliers <- list_out$ebe.npde_outliers
#   case_message <- list_out$case_message
#   
#   if((case_message != '') && (length(case_message) == 2)) {
#     col_amount <- 6
#   } else {
#     col_amount <- 5
#   }
#   
#   # ID numbers of all ourliers if they exist
#   id <- c()
#   if (ncol(ofv_outliers) > 1) {
#     ofv_ID <- ofv_outliers$ID
#     id <- ofv_ID
#   } 
#   if (ncol(ebe.npde_outliers) > 1) {
#     ebe.npde_ID <- ebe.npde_outliers$ID
#     id <- c(id,ebe.npde_ID)
#   }
#   if (ncol(cwres.iwres_outliers) > 1) {
#     cwres.iwres_ID <- as.numeric(as.vector(cwres.iwres_outliers$ID))
#     id <- c(id,cwres.iwres_ID)
#   }
#   
#   # create outlier table (if exists)
#   if (length(id) > 0) {
#     # Unique ID numbers
#     id_unique <- unique(id)
#     id_unique <- sort(id_unique)
#     
#     # create all outlier table
#     all_outlier_table <- array("", c(length(id_unique),col_amount))
#     for (n in 1:length(id_unique)) {
#       col_nr <- 1
#       id_nr <- id_unique[n]
#       all_outlier_table[n,col_nr] <- id_nr
#       
#       col_nr <- col_nr + 1
#       # Add OFV values
#       if (exists("ofv_ID")) {
#         if (any(ofv_ID %in% id_nr)) {
#           ofv_value_row <- which(ofv_ID == id_nr)
#           all_outlier_table[n,col_nr] <- round(ofv_outliers$MEDIAN[ofv_value_row],3)
#         }
#       }
#       
#       col_nr <- col_nr + 1
#       # Add EBE NPDE values
#       if (any(deleted_ID %in% id_nr)) {
#         all_outlier_table[n,col_nr] <- "NA"
#       }
#       if (exists("ebe.npde_ID")) {
#         if (any(ebe.npde_ID %in% id_nr)) {
#           ebe_value_row <- which(ebe.npde_ID == id_nr)
#           if(ebe.npde_outliers[ebe_value_row,2] == "NA") {
#             all_outlier_table[n,col_nr] <- "NA"
#           } else {
#             all_outlier_table[n,col_nr] <- round(as.numeric(as.character(ebe.npde_outliers[ebe_value_row,2])),3)
#           }
#           if(ncol(ebe.npde_outliers) > 2) {
#             all_outlier_table[n,col_nr+1] <- round(as.numeric(as.character(ebe.npde_outliers[ebe_value_row,3])),3)
#           }
#         }
#         if(ncol(ebe.npde_outliers) > 2) {
#           col_nr <- col_nr + 1
#         }
#       } else {
#         if(col_amount == 6) {
#           col_nr <- col_nr + 1
#         }
#       }
#       
#       col_nr <- col_nr + 1
#       # Add CWRES and IWRES values
#       if (exists("cwres.iwres_ID")) {
#         if (any(cwres.iwres_ID %in% id_nr)) {
#           residual_value_row <- which(cwres.iwres_ID == id_nr)
#           cwres.iwres_outliers$OUTLIERS.IWRES <- as.vector(cwres.iwres_outliers$OUTLIERS.IWRES)
#           cwres.iwres_outliers$OUTLIERS.CWRES <- as.vector(cwres.iwres_outliers$OUTLIERS.CWRES)
#           all_outlier_table[n,col_nr] <- cwres.iwres_outliers$OUTLIERS.IWRES[residual_value_row]
#           all_outlier_table[n,col_nr+1] <- cwres.iwres_outliers$OUTLIERS.CWRES[residual_value_row]
#         }
#       }
#     }
#     all_outlier_table <- as.data.frame(all_outlier_table)
#     if(length(case_message) == 1 && (case_message == '')) {
#       colnames(all_outlier_table) <- c("ID","OFV outliers (SD)","EBE NPDE outliers","IWRES outliers","CWRES outliers")
#     } else if (col_amount == 6) {
#       colnames(all_outlier_table) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (1)","EBE NPDE outliers (2)","IWRES outliers","CWRES outliers")
#     } else {
#       colnames(all_outlier_table) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (1)","IWRES outliers","CWRES outliers")
#     }
#     # save as character
#     # all_outlier_table <- data.frame(lapply(all_outlier_table, as.character), stringsAsFactors=FALSE) 
#   } else {
#     all_outlier_table <- data.frame(C = c("No outliers detected"))
#     names(all_outlier_table) <- NULL
#   }
#   return(all_outlier_table)
# }

