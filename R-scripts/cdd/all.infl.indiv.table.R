all.infl.indiv.table <- function(infl_ofv,infl_cook_data,infl_cov_data,fail_ID,ID_failed_cov) {
  col_amount <- 4
  # ID numbers of all influential individuals if they exist
  id <- c()
  if (ncol(infl_ofv) > 1) {
    ofv_ID <- infl_ofv$ID
    id <- ofv_ID
  } 
  if (ncol(infl_cook_data) > 1) {
    cook_ID <- infl_cook_data$ID
    id <- c(id,cook_ID)
  }
  if (ncol(infl_cov_data) > 1) {
    cov_ID <- infl_cov_data$ID
    id <- c(id,cov_ID)
  }
  
  # create influential individual table (if exists)
  if (length(id) > 0) {
    # Unique ID numbers
    id_unique <- unique(id)
    id_unique <- sort(id_unique)
    
    # create all influential individual table
    all_infl_indiv_table <- array("", c(length(id_unique),col_amount))
    for (n in 1:length(id_unique)) {
      col_nr <- 1
      id_nr <- id_unique[n]
      all_infl_indiv_table[n,col_nr] <- id_nr
      
      col_nr <- col_nr + 1
      #ofv deleted id if exists
      if(length(fail_ID)>0) {
        if(any(fail_ID %in% id_nr)) {
          all_infl_indiv_table[n,col_nr] <- "NA"
        }
      }
      
      # Add OFV values
      if (exists("ofv_ID")) {
        if (any(ofv_ID %in% id_nr)) {
          ofv_value_row <- which(ofv_ID == id_nr)
          all_infl_indiv_table[n,col_nr] <- round(infl_ofv$cdd.delta.ofv[ofv_value_row],3)
        }
      }
      
      col_nr <- col_nr + 1
      # Add cook scores values
      if (exists("cook_ID")) {
        if (any(cook_ID %in% id_nr)) {
          cook_value_row <- which(cook_ID == id_nr)
          all_infl_indiv_table[n,col_nr] <- round(infl_cook_data$cook.scores[cook_value_row],3)
        }
      }
      
      col_nr <- col_nr + 1
      #cov.ratios deleted id if exists
      if(length(ID_failed_cov)>0) {
        if(any(ID_failed_cov %in% id_nr)) {
          all_infl_indiv_table[n,col_nr] <- "NA"
        }
      }
      
      # Add cov ratios values
      if (exists("cov_ID")) {
        if (any(cov_ID %in% id_nr)) {
          cov_value_row <- which(cov_ID == id_nr)
          all_infl_indiv_table[n,col_nr] <- round(infl_cov_data$cov.ratios[cov_value_row],3)
        }
      }
    }
    all_infl_indiv_table <- as.data.frame(all_infl_indiv_table)
    # save as character
    all_infl_indiv_table <- data.frame(lapply(all_infl_indiv_table, as.character), stringsAsFactors=FALSE)
    colnames(all_infl_indiv_table) <- c("ID","Delta OFV influentials","Cook score influentials","Cov ratio influentials")
    
  } else {
    all_infl_indiv_table <- data.frame(C = c("No influential individuals detected"))
    names(all_infl_indiv_table) <- NULL
  }
  return(all_infl_indiv_table)
}

