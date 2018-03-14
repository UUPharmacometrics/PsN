simeval_all_ouliers_table <- function(simeval_directory,ebe_npde_outl_crit=-3,quiet=F) {
  if(file.exists(file.path(working.directory,"simeval_run/raw_all_iofv.csv")) 
     && file.exists(file.path(working.directory,"simeval_run/ebe_npde.csv"))) {
    
    #ofv outliers
    iofv_res <- i_ofv_res(file.path(simeval_directory, "raw_all_iofv.csv"),show.warning=F)
    ofv_outliers <- iofv_res$ofv_outliertable
    ID_deleted_ofv <- iofv_res$ID_deleted_ofv
    
    # ebe_npde outliers
    input_data <- input.data(file.path(simeval_directory, "ebe_npde.csv"),show.warning=F)
    ebenpde_tmp <- input_data$ebenpde_tmp
    n.subjects <- input_data$n.subjects
    ebenpde_obs <- input_data$ebenpde_obs
    ID_deleted_ebe <- input_data$ID_deleted
    eta.names <- input_data$eta.names
    
    if(n.subjects>0) {
      list_emp_distance <- empirical.distance(ebenpde_obs,n.subjects)
      emp_distance <- list_emp_distance$emp_distance
      
      out_tables <- data.for.plots(emp_distance,n.subjects,eta.names)
      index_emp_distance <- out_tables$index_emp_distance
      emp_distance_sort <- out_tables$emp_distance_sort
      theor_distance <- out_tables$theor_distance
      out_distance <- out_tables$out_distance
      
      list_plot_1 <- plot_1(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
                            out_distance,n.subjects,outlying_criteria=ebe_npde_outl_crit,do_outlier_plot=FALSE,
                            model.filename='')
      flag <- list_plot_1$flag
      noutlier <- list_plot_1$noutlier
      outlier_id_row <-list_plot_1$outlier_id_row
      
      list_plot_2 <- plot_2(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,
                            noutlier,flag,n.subjects,eta.names,outlying_criteria=ebe_npde_outl_crit,outlier_id_row,
                            do_outlier_plot=FALSE,model.filename='')
      vector_theor_dist <- list_plot_2$vector_theor_dist
      noutlier <- list_plot_2$noutlier
      outlier_id_row <- list_plot_2$outlier_id_row
      
      ebe.npde_outliers <- outlier.table.ebe.npde(ebenpde_tmp,eta.names,outlier_id_row)
      if (ncol(ebe.npde_outliers) > 1) {
        ebe.npde_outliers <- ebe.npde_outliers[,1:2]
      } else {
        ebe.npde_outliers <- data.frame()
      }
    } else {
      ebe.npde_outliers <- data.frame()
    }
    
    #residual outliers
    list_residuals <- outlier.table(residual.outliers.file=file.path(simeval_directory, "residual_outliers.csv"))
    cwres.iwres_outliers <- list_residuals$outliers_count
    residual_names <- list_residuals$residual_names
    
    #all outlier table
    all_outlier_table <- all.outlier.report.table(ofv_outliers,ebe.npde_outliers,cwres.iwres_outliers,
                                                  residual_names,ID_deleted_ebe,ID_deleted_ofv)
    
    add_header_above <- c(" "=1,"Individual level"=2)
    if(length(residual_names)>0) {
      add_header_above <- c(" "=1,"Individual level"=2,"Observation level"=length(residual_names))
    }
    if(ncol(all_outlier_table)==1) {
      add_header_above <- NULL
    }
    out <- list(all_outlier_table=all_outlier_table,
                add_header_above=add_header_above,
                files_exist=TRUE)
  } else {
    if(!file.exists(file.path(working.directory,"simeval_run/raw_all_iofv.csv")) && !quiet) {
      message("WARNING: File ",file.path(working.directory,"simeval_run/raw_all_iofv.csv")," not found!")
    }
    if(!file.exists(file.path(working.directory,"simeval_run/ebe_npde.csv")) && !quiet) {
      message("WARNING: File ",file.path(working.directory,"simeval_run/ebe_npde.csv")," not found!")
    }
    out <- list(files_exist=FALSE)
  }
  
  return(out)
}