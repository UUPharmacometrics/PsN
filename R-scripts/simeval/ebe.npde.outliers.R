ebe.npde.outliers <- function(ebe.npde.file,iiv.eta.names,outlying_criteria,model.filename) {
  #.........................................(1)input_ebe_npde.............................     
  list_input <- list()
  list_input[[1]] <- input.data(ebe.npde.file,iiv.eta.names)
  #unlist
  ebenpde_tmp <- list_input[[1]]$ebenpde_tmp
  #find rows with NA values and save ID numbers of those rows in the vector
  deleted_ID <- c()
  for (i in 1:nrow(ebenpde_tmp)) {
    if(any(is.na(ebenpde_tmp[i,]))) {
      deleted_ID <- c(deleted_ID,ebenpde_tmp$ID[i])
    }
  }
  # if two cases
  if(any(is.na(ebenpde_tmp))) {
    list_input <- list()
    list_input <- two.data.cases(ebenpde_tmp)
  }
  
  list_outlier_table <- list()
  cases <- c()
  for (i in 1:length(list_input)) {
    ebenpde_tmp <- list_input[[i]]$ebenpde_tmp
    n.subjects <- list_input[[i]]$n.subjects
    ebenpde_obs <- list_input[[i]]$ebenpde_obs
    iiv.eta.names <- list_input[[i]]$iiv.eta.names
    case <- list_input[[i]]$case
  
    # create outlier table
    if (require("PEIP") == TRUE){
      #...........................................(2)emp_distance....................................................    
      # Calsulate empirical distance
      emp_distance <- empirical.distance(ebenpde_obs,n.subjects,iiv.eta.names)
      #...........................................(3)out_tables......................................................   
      # Sort emp_distance values and remember rows, where they were in the beginning, 
      # create a vector of probability,
      # compute the inverse Chi^2 distribution,
      # create out_distance table
      out_tables <- data.for.plots(emp_distance,n.subjects,iiv.eta.names)
      index_emp_distance <- out_tables$index_emp_distance
      emp_distance_sort <- out_tables$emp_distance_sort
      theor_distance <- out_tables$theor_distance
      out_distance <- out_tables$out_distance
      #...........................................(4)plot_1.....................................................    
      # THIRD PAGE OF PDF FILE WITH ChiSq Q-Q plot (save flag and noutlier values from function)
      list_plot_1 <- plot_1(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
                      out_distance,n.subjects,outlying_criteria,do_outlier_plot=FALSE,
                      model.filename)
      flag <- list_plot_1$flag
      noutlier <- list_plot_1$noutlier
      outlier_id_row <-list_plot_1$outlier_id_row
      #............................................(5)plot_2....................................................
      # MORE PAGES OF PDF FILE WITH ChiSq Q-Q plot grafs for each n.subjects (only if out_distance < outlying_criteria && flag==1)
      # vector with values of theor_distance to print on the plot
      list_plot_2 <- plot_2(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,
                        noutlier,flag,n.subjects,iiv.eta.names,outlying_criteria,outlier_id_row,
                        do_outlier_plot=FALSE,model.filename)
      vector_theor_dist <- list_plot_2$vector_theor_dist
      noutlier <- list_plot_2$noutlier
      outlier_id_row <- list_plot_2$outlier_id_row
      #............................................(6)outlier_table......................................................    
      #CREATE FINAL TABLE (use function outlier_table to create a table fortable1)
      fortable1 <- outlier.table.ebe.npde(noutlier,outlier_id_row,ebenpde_tmp,ebenpde_obs,
                                          index_emp_distance,emp_distance_sort,vector_theor_dist,
                                          n.subjects,iiv.eta.names)
    
    #.................................................................................................    
    } else{
      print("library PEIP not installed, cannot create outlier results for ebe npde")
    }
  
    if (ncol(fortable1) > 1) {
      ebe.npde_outliers <- fortable1[,1:2]
    } else {
      ebe.npde_outliers <- fortable1
    }
    list_outlier_table[[i]] <- ebe.npde_outliers
    cases[i] <- case
  }
  
  # add two tables (if exists) in one table
  if (length(list_outlier_table) > 1) {
    # unique ID numbers
    id <- c()
    if(ncol(list_outlier_table[[1]])>1) {
      EBE_ID_1 <- list_outlier_table[[1]]$ID
      id <- EBE_ID_1
    }
    if (ncol(list_outlier_table[[2]])>1) {
      EBE_ID_2 <- list_outlier_table[[2]]$ID
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
            ebe.npde_outliers[n,2] <- round(list_outlier_table[[1]][ebe_value_row,2],3)
          }
        }
        
        # Add EBE NPDE values 2
        if (exists("EBE_ID_2")) {
          if (any(EBE_ID_2 %in% id_nr)) {
            ebe_value_row <- which(EBE_ID_2 == id_nr)
            ebe.npde_outliers[n,3] <- round(list_outlier_table[[2]][ebe_value_row,2],3)
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
    ebe.npde_outliers <- list_outlier_table[[1]]
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
              case_message=case_message,
              deleted_ID=deleted_ID)
  return(out)
}