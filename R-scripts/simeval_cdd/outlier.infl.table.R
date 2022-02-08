outlier_infl_table <- function(all.iofv.file,n.subjects,samples,ebe.npde.file,eta.names,outlying_criteria,
                               residual.outliers.file,
                               raw.results.file,skipped.id.file,cutoff_delta.ofv,cutoff_cook,show.warning=TRUE) {
  outlying_criteria <- -outlying_criteria #need negative outlying criteria 
################################################################   SIMEVAL   ##############################################################################  
  # ebe npde part
  list_input <- input.data(ebe.npde.file,eta.names)
  
  ebenpde_tmp <- list_input$ebenpde_tmp
  n.subjects <- list_input$n.subjects
  ebenpde_obs <- list_input$ebenpde_obs
  ID_deleted_ebe <- list_input$ID_deleted

    do_outlier_plot <- FALSE
    model.filename <- '' # because we are not plotting that
    # create outlier grafs and tables
      list_emp_distance <- empirical.distance(ebenpde_obs,n.subjects)
      emp_distance <- list_emp_distance$emp_distance
      #...........................................(4)out_tables......................................................   
      # Sort emp_distance values and remember rows, where they were in the beginning, 
      # create a vector of probability,
      # compute the inverse Chi^2 distribution,
      # create out_distance table
      out_tables <- data.for.plots(emp_distance,n.subjects,eta.names)
      index_emp_distance <- out_tables$index_emp_distance
      emp_distance_sort <- out_tables$emp_distance_sort
      theor_distance <- out_tables$theor_distance
      out_distance <- out_tables$out_distance
      
      #...........................................(5)plot_1.....................................................    
      # ChiSq Q-Q plot (save flag and noutlier values from function)
      list_plot_1 <- plot_1(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
                            out_distance,n.subjects,outlying_criteria,do_outlier_plot,
                            model.filename)
      flag <- list_plot_1$flag
      noutlier <- list_plot_1$noutlier
      outlier_id_row <-list_plot_1$outlier_id_row
      
      #............................................(6)plot_2....................................................
      # MORE PAGES OF PDF FILE WITH ChiSq Q-Q plot grafs for each n.subjects (only if out_distance < outlying_criteria && flag==1)
      # vector with values of theor_distance to print on the plot
      list_plot_2 <- plot_2(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,
                            noutlier,flag,n.subjects,eta.names,outlying_criteria,outlier_id_row,
                            do_outlier_plot,model.filename)
      vector_theor_dist <- list_plot_2$vector_theor_dist
      noutlier <- list_plot_2$noutlier
      outlier_id_row <- list_plot_2$outlier_id_row
      
      #............................................(7)outlier_table......................................................    
      #CREATE FINAL TABLE (use function outlier_table to create a table fortable1)
      fortable1 <- outlier.table.ebe.npde(ebenpde_tmp,eta.names,outlier_id_row)

      #for ebe.npde.all.outliers
      if (ncol(fortable1) > 1) {
        ebe.npde_outliers <- fortable1[,1:2]
      } else {
        ebe.npde_outliers <- fortable1
      }
      
      #.................................................................................................    
  
  # ofv part
  list_i_ofv_res <- i_ofv_res(all.iofv.file,n.subjects,samples,show.warning)# calculation
  # for all outlier table
  ofv_outliers <- list_i_ofv_res$ofv_outliertable
  ID_deleted_ofv <- list_i_ofv_res$ID_deleted_ofv
  
  # residuals
  list <- outlier.table(residual.outliers.file)
  cwres.iwres_outliers <- list$outliers_count
  
  #outlier table
  #if (file.exists(residual.files[1]) && file.exists(residual.files[2]) && file.exists(residual.outliers.file)) {
  if (file.exists(residual.outliers.file)) {
    all_outlier_table <- PsNR::all_outlier_report_table(ofv_outliers,ebe.npde_outliers=ebe.npde_outliers,cwres.iwres_outliers,
                                                  ID_deleted_ebe,ID_deleted_ofv)
  } else {
    all_outlier_table <- PsNR::all_outlier_report_table(ofv_outliers,ebe.npde_outliers=ebe.npde_outliers,
                                                  ID_deleted_ebe=ID_deleted_ebe,ID_deleted_ofv=ID_deleted_ofv)
  }
  
  #########################################################   CDD   ###############################################################################
  #cook, cov
  if(missing("cutoff_cook")) {
    cutoff_cook <- 0.8
  }
  list_out <- cutoff.cov.cook(raw.results.file,skipped.id.file,cutoff_cook)
  cutoff_cov <- list_out$cutoff_cov
  cook_outliers_data <- list_out$cook_outliers_data
  cov_outliers_data <- list_out$cov_outliers_data
  if(nrow(cook_outliers_data)) {
    infl_cook_data <- cook_outliers_data[,1:2]
  } else {
    infl_cook_data <- data.frame(C = c("No influential individuals detected (cook)"))
    names(infl_cook_data) <- NULL
  }
  if(nrow(cov_outliers_data)) {
    infl_cov_data <-  cov_outliers_data[,1:2]
  } else {
    infl_cov_data <- data.frame(C = c("No influential individuals detected (cov)"))
    names(infl_cov_data) <- NULL
  }

  # ofv
  out_cdd.data.all <- create.data.full(raw.results.file,skipped.id.file)
  cdd.data.all <- out_cdd.data.all$cdd.data.all
  
  list.delta.ofv <- delta.ofv.data(cdd.data.all,cutoff_delta.ofv=cutoff_delta.ofv)
  infl_ofv <- list.delta.ofv$infl_ofv
  fail_ID <- list.delta.ofv$fail_ID

  all_infl_indiv_table <- all_infl_indiv_table(infl_ofv,infl_cook_data,infl_cov_data,fail_ID)
  
  ##########################################################   summary table   ##################################################################
  #create a influential_outlier table
  if(ncol(all_outlier_table)==3) {
    col_amount <- 6
  } else {
    col_amount <- 8
  }

  # ID numbers of all influential individuals if they exist
  id <- c()
  if (ncol(all_outlier_table) > 1) {
    outl_ID <- as.numeric(as.character(all_outlier_table$ID))
    id <- outl_ID
  } 
  if (ncol(all_infl_indiv_table) > 1) {
    infl_ID <- as.numeric(as.character(all_infl_indiv_table$ID))
    id <- c(id,infl_ID)
  }

  # create influential individual table (if exists)
  if (length(id) > 0) {
    # Unique ID numbers
    id_unique <- unique(id)
    id_unique <- sort(id_unique)

    # create all influential individual table
    outl_infl_table <- array("", c(length(id_unique),col_amount))

    for (n in 1:length(id_unique)) {
      col_nr <- 1
      id_nr <- id_unique[n]
      outl_infl_table[n,col_nr] <- id_nr
      
      #chech if id_nr is from deleted simeval ID numbers
      if(any(ID_deleted_ebe %in% id_nr) || any(ID_deleted_ofv %in% id_nr)) {
        NA_simeval <- TRUE
      } else {
        NA_simeval <- FALSE
      }
      #chech if id_nr is from deleted CDD ID numbers
      if(any(fail_ID %in% id_nr)) {
        NA_CDD <- TRUE
      } else {
        NA_CDD <- FALSE
      }
      
      col_nr <- col_nr + 1
      # Add outlier values
      if (exists("outl_ID")) {
        if (any(outl_ID %in% id_nr)) {
          outl_value_row <- which(outl_ID == id_nr)
          j <- 0
          for (i in 2:ncol(all_outlier_table)) {
            if(all_outlier_table[outl_value_row,i] != ''){
              outl_infl_table[n,col_nr+j] <- all_outlier_table[outl_value_row,i]
            }
          j <- j + 1
          }
        } else {
          if(NA_simeval==TRUE) {
            j <- 0
            for (i in 2:ncol(all_outlier_table)) {
              if(any(ID_deleted_ofv %in% id_nr) && (col_nr+j==2)) {
                outl_infl_table[n,col_nr+j] <- 'NA'
              }
              if(any(ID_deleted_ebe %in% id_nr) && (col_nr+j==3)) {
                outl_infl_table[n,col_nr+j] <- 'NA'
              }
              j <- j + 1
            }
          }
        }
      }
      
      col_nr <- col_nr + 4
      # Add OFV values
      if (exists("infl_ID")) {
        if (any(infl_ID %in% id_nr)) {
          infl_value_row <- which(infl_ID == id_nr)
          j <- 0
          for (i in 2:ncol(all_infl_indiv_table)) {
            if(all_infl_indiv_table[infl_value_row,i] != '') {
              outl_infl_table[n,col_nr+j] <- all_infl_indiv_table[infl_value_row,i]
            }
            j <- j + 1
          }
        } else {
          if(NA_CDD==TRUE) {
            j <- 0
            for (i in 2:ncol(all_outlier_table)) {
              if(any(fail_ID %in% id_nr) && (col_amount-2==col_nr+j)) {
                outl_infl_table[n,col_nr+j] <- 'NA'
              }
              j <- j + 1
            }
          }
        }
      }
    }
    
    outl_infl_table <- as.data.frame(outl_infl_table, stringsAsFactors=FALSE)
    if(col_amount==8) {
      colnames(outl_infl_table) <- c("ID","OFV (SD)","EBE NPDE\n(ETA nr.)","IWRES","CWRES",
                                     "Delta OFV","Cook\nscores","Cov\nratios")
    } else {
      colnames(outl_infl_table) <- c("ID","OFV (SD)","EBE NPDE\n(ETA nr.)",
                                     "Delta OFV","Cook\nscores","Cov\nratios")
    }

    
  } else {
    outl_infl_table <- data.frame(C = c("No influential individuals and outliers detected"))
    names(outl_infl_table) <- NULL
  }
  
  outlying_criteria <- -outlying_criteria # set it back to positive
  out <- list(col_amount=col_amount,
              all_outlier_table=all_outlier_table,
              all_infl_indiv_table=all_infl_indiv_table,
              outl_infl_table=outl_infl_table)
  return(out)
}
