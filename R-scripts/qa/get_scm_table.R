get_scm_table <- function(scm_directory,frem_directory,parameters,continuous,categorical,skip){
  rawres_file <- file.path(scm_directory,"raw_results_scm.csv")
  scm_files_exists <- file.exists(rawres_file)
  if(any(skip=="scm")) {
    scm_table <- data.frame("SCM","SKIPPED",stringsAsFactors = F)
    colnames(scm_table) <- c("","dOFV")
    max_scm_table <- cbind(scm_table,"",stringsAsFactors = F)
  } else {
    if(length(parameters)!=0 && (length(categorical)!=0 || (length(continuous)!=0))) {
      if(scm_files_exists) {
        scm_table <- read.csv(rawres_file,stringsAsFactors = F) %>%
          dplyr::mutate(dOFV = ofv[step.number==0]-ofv) %>%
          dplyr::slice(-1) %>%
          dplyr::select(relation, dOFV)
        colnames(scm_table) <- c("","dOFV")
        #max_table
        max_scm_table <- cbind(scm_table[which.max(scm_table$dOFV),],1)
        
        # add coefficient
        scm_table_coef <- read.csv(rawres_file,stringsAsFactors = F) %>%
          dplyr::slice(-1)
        column_names <- paste0(scm_table_coef$relation,"-1")
        column_names <- gsub("\\-","\\.",column_names)
        coef_table <- scm_table_coef[,which(colnames(scm_table_coef) %in% column_names)]
        scm_table <- cbind(scm_table,"Coef"=as.numeric(as.character(rep(NA,nrow(scm_table)))))
        for(i in 1:length(coef_table)) {
          row_nr <- which(paste0(gsub("\\-","\\.",scm_table[,1]),".1") == colnames(coef_table[i]))
          if(!all(is.na(coef_table[i]))) {
            scm_table$Coef[row_nr] <- as.numeric(coef_table[!is.na(coef_table[i]),i])
          }
        }
        colnames(scm_table) <- c("","dOFV","Coef")
        
        # Add coef/sd column
        # if(file.exists(file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv"))) { 
        #   # all param cov/cat combination (check if all are in table, skip those that are not)
        #   cov <- c(continuous,categorical)
        #   used_cov <- c()
        #   for(i in 1:length(cov)) {
        #     comb <- paste0(parameters,cov[i])
        #     for(j in 1:length(comb)) {
        #       if(any(grepl(paste0(comb[j],"-"),scm_table[,1]))) {
        #         used_cov <- c(used_cov,cov[i])
        #       }
        #     }
        #   }
        #   used_cov <- unique(used_cov)
        #   
        #   # get sd from frem model 4
        #   needed_columns <- paste0("BSV_",cov)
        #   frem_sd_cov <- read.csv(file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv")) %>% 
        #     dplyr::select(intersect(needed_columns,colnames(.))) %>%
        #     sqrt()
        #   colnames(frem_sd_cov) <- sub("BSV_","",colnames(frem_sd_cov))
        #   # frem_sd <- sqrt(get_omega_values(file.path(frem_directory,"final_models/model_4.ext"),omegas = "var"))
        #   # add coeffsd column
        #   scm_table$Coef_sd <- as.numeric(rep(as.numeric(NA)),length(scm_table$Coef_sd))
        #   for(j in 1:length(parameters)) {
        #     for(i in 1:length(cov)) {
        #       if(any(grepl(paste0("^",parameters[j],cov[i]),scm_table[,1]))) {
        #         nr <- grep(paste0("^",parameters[j],cov[i]),scm_table[,1])
        #         if(any(grepl(cov[i],colnames(frem_sd_cov)))) {
        #           frem_sd_nr <- grep(cov[i],colnames(frem_sd_cov))
        #           scm_table$Coef_sd[nr] <- as.numeric(scm_table$Coef[nr]/frem_sd_cov[frem_sd_nr])
        #         }
        #       }
        #     }
        #   }
        # }
      } else {
        scm_table <- error_table("SCM")
        max_scm_table <- cbind(scm_table,"",stringsAsFactors = F)
      }
    } else {
      scm_files_exists <- FALSE
      scm_table <- data.frame("SCM","NA",stringsAsFactors = F)
      colnames(scm_table) <- c("","dOFV")
      max_scm_table <- cbind(scm_table,"",stringsAsFactors = F)
    }
  }
  
  colnames(max_scm_table) <- c("","dOFV","Add.params")
  return(list(scm_files_exists=scm_files_exists,
              scm_table=scm_table,
              max_scm_table=max_scm_table))
}