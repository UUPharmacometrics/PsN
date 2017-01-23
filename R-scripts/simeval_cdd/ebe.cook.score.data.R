ebe_cook.score_data <- function(ebe.npde.file,eta.names,outlying_criteria,
                                raw.results.file,skipped.id.file,cutoff_cook) {
  # GET OUTLIERS FROM SIMEVAL 
  do_outlier_plot <- FALSE
  model.filename <- ''

  # simeval (ebe outliers) ------------------------------------------------------------
  list_input <- input.data(ebe.npde.file,eta.names)
  
  ebenpde_tmp <- list_input$ebenpde_tmp
  n.subjects <- list_input$n.subjects
  ebenpde_obs <- list_input$ebenpde_obs
  ID_deleted_ebe <- list_input$ID_deleted
  
  #get EBE_NPDE values
  EBE_NPDE_max_abs <- c()
  for (i in 1:nrow(ebenpde_obs)) {
    ebe_row <- ebenpde_obs[i,]
    ebe_row <- ebe_row[!is.na(ebe_row)]
    EBE_NPDE_max_abs[i] <- max(abs(ebe_row))
  }

  # create a data frame
  outl_data <- as.data.frame(cbind(ebenpde_tmp$ID,EBE_NPDE_max_abs))
  colnames(outl_data) <- c("ID_simeval","EBE_NPDE")
  
  # find outliers
  outlier_row <- c()
  for (i in 1:nrow(outl_data)) {
    if(outl_data$EBE_NPDE[i] > outlying_criteria) { # if something is bigger than 3 (because ebe npde values are calculated with abs)
      outlier_row <- c(outlier_row,i)
    }
  }
  if(length(outlier_row)>0) {
    outlier_ID <- outl_data$ID[outlier_row]
  } else {
    outlier_ID <- c()
  }
  
  # GET INFLUENTIAL INDIVIDUALS FROM CDD
  # cdd (create.data.full.R) --------------------------------------------------------------------------
  out_cdd.data.all <- create.data.full(raw.results.file,skipped.id.file) # use function
  cdd.data.all <- out_cdd.data.all$cdd.data.all
  cdd.data.all <- cdd.data.all[-1,]
  no.cook.cov <- out_cdd.data.all$no.cook.cov
  
  if(!no.cook.cov) {
    # save needed columns in one data frame
    infl_data <- as.data.frame(cbind(cdd.data.all$ID,cdd.data.all$cook.scores))
    colnames(infl_data) <- c("ID_cdd","cook_scores")
    
    # find influential
    infl_row <- c()
    for (i in 1:nrow(infl_data)) {
      if(infl_data$cook_scores[i] > cutoff_cook) {
        infl_row <- c(infl_row,i)
      }
    }
    if(length(infl_row)>0) {
      infl_ID <- infl_data$ID[infl_row]
    } else {
      infl_ID <- c()
    }
    
    # cdd and simeval ------------------------------------------------------------------------
    #delete ID numbers (deleted ID numbers from EBE) from CDD and compute a warning message
    if(length(ID_deleted_ebe)>0) {
      
      # delete individuals from cdd which have been deleted from simeval EBE
      if(any(infl_data$ID %in% ID_deleted_ebe)) {
        deleted_cdd_ID.ebe <- intersect(ID_deleted_ebe,infl_data$ID)
        # if there are deleted influentials
        if(length(infl_ID)>0) {
          deleted_infl_ID <- intersect(ID_deleted_ebe,infl_ID)
        }
        # message
        if(length(deleted_cdd_ID.ebe)>0) {
          deleted_cdd_ID.ebe_text <- c()
          for (i in 1:length(deleted_cdd_ID.ebe)) {
            if(i == 1) {
              deleted_cdd_ID.ebe_text <- deleted_cdd_ID.ebe[i]
            } else {
              deleted_cdd_ID.ebe_text <- paste0(deleted_cdd_ID.ebe_text,", ",deleted_cdd_ID.ebe[i])
            }
          }
          if(exists("deleted_infl_ID")) {
            deleted_infl_ID_text <- c()
            for (i in 1:length(deleted_infl_ID)) {
              if(i == 1) {
                deleted_infl_ID_text <- deleted_infl_ID[i]
              } else {
                deleted_infl_ID_text <- paste0(deleted_infl_ID_text,", ",deleted_infl_ID[i])
              }
            }
            message(paste0("WARNING! Deleted individuals ",deleted_cdd_ID.ebe_text," from which individuals ",deleted_infl_ID_text," are influential!"))
          } else {
            message(paste0("WARNING! Deleted individuals ",deleted_cdd_ID.ebe_text,"!"))
          }
          
        }
        #delete rows
        row_deleted_infl <- c()
        row_delete_ebe <- c()
        for(i in 1:length(ID_deleted_ebe)) {
          row_delete_ebe_i <- which(infl_data$ID %in% ID_deleted_ebe[i])
          row_delete_ebe <- c( row_delete_ebe,row_delete_ebe_i)
        }
        infl_data <- infl_data[-row_delete_ebe,]
        rownames(infl_data) <- NULL
        infl_ID <- intersect(infl_ID,infl_data$ID)
      }
    }
    
    # find which individuals are infuencial and outliers
    ID <- intersect(outlier_ID,infl_ID)
    
    #order influential individual table and outlier table by iD numbers
    ID_ordered_influential_table <- infl_data[order(infl_data$ID_cdd),]
    rownames(ID_ordered_influential_table) <- NULL
    ID_ordered_outlier_table <- outl_data[order(outl_data$ID_simeval),]
    rownames(ID_ordered_outlier_table) <- NULL
    
    # create a table for plotting
    table_for_plot <- as.data.frame(cbind(ID_ordered_influential_table$ID_cdd,ID_ordered_influential_table$cook_scores,ID_ordered_outlier_table$EBE_NPDE))
    colnames(table_for_plot) <- c("ID","cdd_cook_scores","simeval_EBE_NPDE")
    
    #rows of infl and outl individuals
    row <- c()
    for (i in 1:length(ID)) {
      row[i] <- which(table_for_plot$ID %in% ID[i])
    }
    
    # count how many are influential and outliers, influential but not outliers, --------------
    # outliers but not influential, not outliers and not influential
    infl_outl <- ID
    infl_not_outl <- setdiff(infl_ID,ID)
    outl_not_infl <- setdiff(outlier_ID,ID)
    not_outl_not_infl <- setdiff(table_for_plot$ID,unique(c(outlier_ID,infl_ID)))
    
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
    
    # return
    out <- list(infl_data=infl_data,
                outl_data=outl_data,
                table_for_plot= table_for_plot,
                ID=ID,
                row=row,
                infl_outl=infl_outl,
                infl_not_outl=infl_not_outl,
                outl_not_infl=outl_not_infl,
                not_outl_not_infl=not_outl_not_infl,
                row_infl_not_outl=row_infl_not_outl,
                row_outl_not_infl=row_outl_not_infl,
                no.cook.cov=no.cook.cov)
    return(out)
  } else {
    return(no.cook.cov)
    message("No cook score or cov ratio values are found in the raw results csv file!")
  }
  
}