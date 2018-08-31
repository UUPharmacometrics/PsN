yaml_summary <- function(ofv_table,
                         list_par_var_models,
                         resmod_structural_details,
                         full_omega_block_list,
                         boxcox_lambdas_list,
                         tdist_list,
                         add_etas_list,
                         iov_list,
                         frem_table_list,
                         scm_table_list,
                         resmod_table_list,
                         ii_list,
                         outlier_table_list,
                         overview_table_list
                         ) {
  #function to check varables existance
  check_argument_existance <- function(x) {
    result <- tryCatch({
      x
    }, error = function(e){
      NULL
    })
    return(result)
 }
 #check if variables exist
  resmod_structural_details <- check_argument_existance(resmod_structural_details)
  full_omega_block_list <- check_argument_existance(full_omega_block_list)
  boxcox_lambdas_list <- check_argument_existance(boxcox_lambdas_list)
  tdist_list <- check_argument_existance(tdist_list)
  add_etas_list <- check_argument_existance(add_etas_list)
  iov_list <- check_argument_existance(iov_list)
  
 #add nonlinear and linearized ofv values to the tree
  tree <- list(
    nonlinear_base_model_ofv = ofv_table[1,2],
    linearized_base_model_ofv_before_estimation = ofv_table[2,2],
    linearized_base_model_ofv_after_estimation = ofv_table[3,2],
    sum_of_individual_ofv_values = ofv_table[4,2]
  )
  
  # overview table
  ovw_table <- overview_table_list$overview_table
  colnames(ovw_table)[1] <- "name"
  # group_name <- rep(overview_table_list$rgroup_names,times = overview_table_list$n.rgroup)
  ovw_table <- ovw_table %>%
    mutate(group_name=rep(overview_table_list$rgroup_names,times = overview_table_list$n.rgroup),
           model_name=paste0(group_name," (",name,")"))
  #add overview table to the tree
  tree$dofv <- list()
  tree$additional_param <- list()
  for(i in 1:nrow(ovw_table)) {
    tree$dofv[[ovw_table$model_name[i]]] <- ovw_table$dOFV[i]
    tree$additional_param[[ovw_table$model_name[i]]] <- ovw_table$`Additional parameters`[i]
  }
  
  #strunctural tables
  ######################   ckeck if multiple dvids  ###############################
  tree$structural <- list()
  if(!is.null(resmod_structural_details)) {
    for(i in 1:length(resmod_structural_details)) {
      resmod_structural_details_modified <- resmod_structural_details[[i]]
      resmod_structural_details_modified$idv_text <- NULL
      resmod_structural_details_modified$perc <- NULL
      resmod_structural_details_modified$first_table <- NULL
      tree$structural[[i]] <- resmod_structural_details_modified
    }
  }
  
  #parameter variability tables
  tree$parameter_variability <- list()
  if(!is.null(full_omega_block_list)) {
    tree$parameter_variability$full_omega_block_table <- full_omega_block_list$full_omega_block_table
  }
  if(!is.null(boxcox_lambdas_list)) {
    tree$parameter_variability$boxcox_table <- boxcox_lambdas_list$param_extra_table_orig
  }
  if(!is.null(tdist_list)) {
    tree$parameter_variability$tdist_table <- tdist_list$param_extra_table_orig
  }
  if(!is.null(add_etas_list)) {
    #################   check  #################
  }
  if(!is.null(iov_list)) {
    #################   check  #################
  }
  
  #frem table
  tree$frem <- frem_table_list
  
  #scm tables
  tree$scm <- scm_table_list
  
  #resmod tables
  tree$resmod <- list()
  if(length(resmod_table_list$dvid_nr==1) & resmod_table_list$dvid_nr=="NA") {
    tree$resmod$dvid_values <- "No DVIDs"
  } else {
    tree$resmod$dvid_values <- resmod_table_list$dvid_nr
  }
  if(tree$resmod$dvid_values=="No DVIDs") {
    tree$resmod[[paste0("resmod_table_no_dvids")]] <- resmod_table_list$resmod_ruv_table_list[[1]]
  } else {
    for(i in 1:length(resmod_table_list$dvid_nr)) {
      tree$resmod[[paste0("remosd_table_with_DVID_",i)]] <- resmod_table_list$resmod_ruv_table_list[[i]]
    }
  }
  
  #influential id tables
  ii_list_modified <- ii_list
  ii_list_modified$all_dofv <- NULL
  ii_list_modified$cdd.data <- NULL
  ii_list_modified$cdd_highest_dofv <- NULL
  ii_list_modified$fig_height_infl <- NULL
  tree$cdd <- ii_list_modified
  
  #outliers tables
  outlier_table_list_modified <- outlier_table_list
  outlier_table_list_modified$fig_height_outl <- NULL
  tree$simeval <- outlier_table_list_modified
 
  #overall outliers table
  all_outl_list_modified <- all_outl_list
  all_outl_list_modified$add_header_above <- NULL
  tree$overall_outliers <- all_outl_list_modified
  
  #create a yaml file
  yaml <- as.yaml(tree, indent.mapping.sequence=TRUE)
  cat(yaml, file="results_summary.yaml")
}

