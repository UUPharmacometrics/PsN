get_scm_table <- function(scm_directory,frem_directory,parameters,covariates,categorical,skip){
  rawres_file <- file.path(scm_directory,"raw_results_scm.csv")
  scm_files_exists <- file.exists(rawres_file)
  if(any(skip=="scm")) {
    scm_table <- data.frame("SCM","SKIPPED",stringsAsFactors = F)
    colnames(scm_table) <- c("","dOFV")
    max_scm_table <- cbind(scm_table,"",stringsAsFactors = F)
  } else {
    if(length(parameters)!=0 && (length(categorical)!=0 || (length(covariates)!=0))) {
      if(scm_files_exists) {
        scm_table <- read.csv(rawres_file,stringsAsFactors = F) %>%
          mutate(dOFV = ofv[step.number==0]-ofv) %>%
          slice(-1) %>%
          select(relation, dOFV)
        colnames(scm_table) <- c("","dOFV")
        #max_table
        max_scm_table <- cbind(scm_table[which.max(scm_table$dOFV),],1)
        
        # add coefficient
        scm_table_coef <- read.csv(rawres_file,stringsAsFactors = F) %>%
          slice(-1)
        column_names <- paste0(scm_table_coef$relation,"-1")
        column_names <- gsub("-",".",column_names)
        coef_table <- scm_table_coef[,which(colnames(scm_table_coef) %in% column_names)]
        scm_table <- cbind(scm_table,"Coef"=as.numeric(as.character(rep(NA,nrow(scm_table)))))
        for(i in 1:length(coef_table)) {
          row_nr <- which(paste0(gsub("-",".",scm_table[,1]),".1") == colnames(coef_table[i]))
          if(!all(is.na(coef_table[i]))) {
            scm_table$Coef[row_nr] <- as.numeric(coef_table[!is.na(coef_table[i]),i])
          }
        }
        colnames(scm_table) <- c("","dOFV","Coef")
        
        # Add coef/sd column
        if(file.exists(file.path(frem_directory,"final_models/model_4.ext")) &&
           file.exists(file.path(scm_directory,"scmlog.txt")) &&
           file.size(file.path(scm_directory,"scmlog.txt"))!=0) {
          # get sd from frem model 4 (final model)
          all_frem_omegas <- get_omega_values(file.path(frem_directory,"final_models/model_4.ext"),omegas = "var")
          frem_sd <- sqrt(all_frem_omegas[,1:length(parameters)])
          # which parameter with which omega
          scm_log_file <- read.table(file.path(scm_directory,"scmlog.txt"),sep="\t",stringsAsFactors = F) %>%
            filter(grepl("^Detected(\\D+)ETA(\\d+) on ",V1))
          scm_table$Coef_sd <- scm_table$Coef
          for(i in 1:nrow(scm_log_file)) {
            row_text <- strsplit(scm_log_file[i,1],split = " ")
            eta <- row_text[[1]][which(row_text[[1]] == "on") - 1]
            param <- row_text[[1]][which(row_text[[1]] == "on") + 1]
            nr <- as.numeric(sub("\\ETA","",eta))
            sd <- frem_sd[1,grepl(nr,colnames(frem_sd))]
            par_cov_combination <- sub('\\-.*',"",scm_table[,1])
            for(j in 1:nrow(scm_table)) {
              check_cov_cat <- sub(param,"",par_cov_combination[j])
              if(any(categorical==check_cov_cat) || any(covariates==check_cov_cat)){
                scm_table$Coef_sd[j] <- scm_table$Coef_sd[j]/sd
              }
            }
          }
        }

        
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