ebe.npde.all.outliers <- function(list_ebe_outlier_table,cases,deleted_ID) {
  # add two tables (if exists) in one table
  if (length(list_ebe_outlier_table) > 1) {
    # unique ID numbers
    id <- c()
    if(ncol(list_ebe_outlier_table[[1]])>1) {
      EBE_ID_1 <- list_ebe_outlier_table[[1]]$ID
      id <- EBE_ID_1
    }
    if (ncol(list_ebe_outlier_table[[2]])>1) {
      EBE_ID_2 <- list_ebe_outlier_table[[2]]$ID
      id <- c(id,EBE_ID_2)
    }
    if (length(id) > 0) {
      # Unique ID numbers
      id_unique <- unique(id)
      id_unique <- sort(id_unique)
      
      # create ebe outlier table
      ebe.npde_outliers <- array("", c(length(id_unique),3))
      for (n in 1:length(id_unique)) {
        id_nr <- id_unique[n]
        ebe.npde_outliers[n,1] <- id_nr
        
        # Add EBE NPDE values 1
        if (any(deleted_ID %in% id_nr)) {
          ebe.npde_outliers[n,2] <- "NA"
        }
        if (exists("EBE_ID_1")) {
          if (any(EBE_ID_1 %in% id_nr)) {
            ebe_value_row <- which(EBE_ID_1 == id_nr)
            ebe.npde_outliers[n,2] <- round(list_ebe_outlier_table[[1]][ebe_value_row,2],3)
          }
        }
        
        # Add EBE NPDE values 2
        if (exists("EBE_ID_2")) {
          if (any(EBE_ID_2 %in% id_nr)) {
            ebe_value_row <- which(EBE_ID_2 == id_nr)
            ebe.npde_outliers[n,3] <- round(list_ebe_outlier_table[[2]][ebe_value_row,2],3)
          }
        }
      }
      
      ebe.npde_outliers <- as.data.frame(ebe.npde_outliers)
      colnames(ebe.npde_outliers) <- c("ID","EBE NPDE outliers (1)","EBE NPDE outliers (2)")
      ebe.npde_outliers$ID <- as.numeric(as.character(ebe.npde_outliers$ID))
      case_message <- cases
    } else {
      ebe.npde_outliers <- data.frame(C = c("No outliers detected"))
      names(ebe.npde_outliers) <- NULL
      case_message <- cases
    }
    
  } else {
    ebe.npde_outliers <- list_ebe_outlier_table[[1]]
    case_message <- cases[1]
    if((case_message != '') && (ncol(ebe.npde_outliers)>1)) {
      colnames(ebe.npde_outliers) <- c("ID","EBE NPDE outliers (1)")
      ebe.npde_outliers$ID <- as.numeric(as.character(ebe.npde_outliers$ID))
      ebe.npde_outliers <- ebe.npde_outliers[order(ebe.npde_outliers$ID),]
      ebe.npde_outliers[,2] <- as.factor(round(ebe.npde_outliers[,2],3))
      rownames(ebe.npde_outliers) <- NULL
    }
  }
  out <- list(ebe.npde_outliers=ebe.npde_outliers,
              case_message=case_message)
  return(out)
}