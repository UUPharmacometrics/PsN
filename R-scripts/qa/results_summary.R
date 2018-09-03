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
                         overview_table_list,
                         dvid_name
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
  
 #check if dvid exists
  if(dvid_name!='') {
    tree$dvid_exists <- TRUE
    tree$dvid_name <- dvid_name
  } else {
    tree$dvid_exists <- FALSE
  }
  
  #overview table
  ovw_table <- overview_table_list$overview_table
  colnames(ovw_table)[1] <- "name"
  group_name <- rep(overview_table_list$rgroup_names,times = overview_table_list$n.rgroup)
  ovw_table <- ovw_table %>%
    mutate(group_name=rep(overview_table_list$rgroup_names,times = overview_table_list$n.rgroup))
  
  # overview (structural)
  tree$overview <- list()
  overview <- ovw_table %>%
    filter(group_name=="Structural Model") 
  if(tree$dvid_exists==TRUE) {
    overview <- overview %>%
      mutate(dvid=case_when(grepl(dvid_name,name) ~ name,
                            TRUE ~ as.character(NA)),
             dvid=as.numeric(gsub("\\D", "", dvid))) %>%
      fill(dvid, .direction = "down") %>%
      filter(!(grepl(dvid_name,name)))
    
    tree$overview$structural <- list()
    for(i in 1:length(unique(overview$dvid))) {
      tree$overview$structural[[i]] <- list()
      tree$overview$structural[[i]]$dvid_value <- unique(overview$dvid)[i]
      overview_dvid <- overview %>%
        filter(dvid==unique(overview$dvid)[i])

      for(j in 1:nrow(overview_dvid)) {
        tree$overview$structural[[i]]$dofv[[overview$name[j]]] <- overview_dvid$dOFV[j]
        tree$overview$structural[[i]]$additional_param[[overview$name[j]]] <- overview_dvid$`Additional parameters`[j]
      }
    }
  } else {
    tree$overview$structural[[1]] <- list()
    for(i in 1:nrow(overview)) {
      tree$overview$structural[[1]]$dofv[[overview$name[i]]] <- overview$dOFV[i]
      tree$overview$structural[[1]]$additional_param[[overview$name[i]]] <- overview$`Additional parameters`[i]
    }
  }
  
  # overview (parameter variability)
  tree$overview$param_variability <- list()
  par_variab <- ovw_table %>%
    filter(group_name=="Parameter Variability Model")
  for(i in 1:nrow(par_variab)) {
    tree$overview$param_variability$dofv[[par_variab$name[i]]] <- par_variab$dOFV[i]
    tree$overview$param_variability$additional_param[[par_variab$name[i]]] <- par_variab$`Additional parameters`[i]
  }
  
  # overview (frem)
  tree$overview$frem <- list()
  frem_part <- ovw_table %>%
    filter(group_name=="Covariates") %>%
    slice(1)
  tree$overview$frem$dofv[[frem_part$name]] <- frem_part$dOFV
  tree$overview$frem$additional_param[[frem_part$name]] <- frem_part$`Additional parameters`
  
  # overview (scm)
  tree$overview$scm <- list()
  scm_part <- ovw_table %>%
    filter(group_name=="Covariates") %>%
    slice(2)
  tree$overview$scm$dofv[[scm_part$name]] <- scm_part$dOFV
  tree$overview$scm$additional_param[[scm_part$name]] <- scm_part$`Additional parameters`
  
  # overview (ruv)
  overview <- ovw_table %>%
    filter(group_name=="Residual Error Model") 
  if(tree$dvid_exists==TRUE) {
    overview <- overview %>%
      mutate(dvid=case_when(grepl(dvid_name,name) ~ name,
                            TRUE ~ as.character(NA)),
             dvid=as.numeric(gsub("\\D", "", dvid))) %>%
      fill(dvid, .direction = "down") %>%
      filter(!(grepl(dvid_name,name)))
    
    tree$overview$residual_error <- list()
    for(i in 1:length(unique(overview$dvid))) {
      tree$overview$residual_error[[i]] <- list()
      tree$overview$residual_error[[i]]$dvid_value <- unique(overview$dvid)[i]
      overview_dvid <- overview %>%
        filter(dvid==unique(overview$dvid)[i])
      
      for(j in 1:nrow(overview_dvid)) {
        tree$overview$residual_error[[i]]$dofv[[overview$name[j]]] <- overview_dvid$dOFV[j]
        tree$overview$residual_error[[i]]$additional_param[[overview$name[j]]] <- overview_dvid$`Additional parameters`[j]
      }
    }
  } else {
    tree$overview$residual_error[[1]] <- list()
    for(i in 1:nrow(overview)) {
      tree$overview$residual_error[[1]]$dofv[[overview$name[i]]] <- overview$dOFV[i]
      tree$overview$residual_error[[1]]$additional_param[[overview$name[i]]] <- overview$`Additional parameters`[i]
    }
  }
  
  # overview (cdd)
  tree$overview$infl_id <- list()
  cdd_part <- ovw_table %>%
    filter(group_name=="Influential Individuals") %>%
    slice(1)
  tree$overview$infl_id$dofv[[cdd_part$name]] <- cdd_part$dOFV
  tree$overview$infl_id$additional_param[[cdd_part$name]] <- cdd_part$`Additional parameters`
  
  # overview (simeval)
  tree$overview$outliers <- list()
  simeval_part <- ovw_table %>%
    filter(group_name=="Outliers") %>%
    slice(1)
  tree$overview$outliers$dofv[[simeval_part$name]] <- simeval_part$dOFV
  tree$overview$outliers$additional_param[[simeval_part$name]] <- simeval_part$`Additional parameters`
  
  #structural part
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
  if(tree$dvid_exists==TRUE) {
    for(i in 1:length(resmod_table_list$dvid_nr)) {
      tree$resmod[[i]] <- list()
      tree$resmod[[i]]$dvid_value <- resmod_table_list$dvid_nr[i]
      tree$resmod[[i]]$resmod_ruv_table  <- resmod_table_list$resmod_ruv_table_list[[i]]
    }
  } else {
    tree$resmod[[1]]$resmod_ruv_table  <- resmod_table_list$resmod_ruv_table_list
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

