get_scm_table <- function(rawres_file,parameters,covariates,categorical){
  scm_files_exists <- file.exists(rawres_file)
  if(length(parameters)!=0 && (length(categorical)!=0 || (length(covariates)!=0))) {
    if(scm_files_exists) {
      scm_table <- read.csv(rawres_file,stringsAsFactors = F) %>%
        mutate(dOFV = ofv[step.number==0]-ofv) %>%
        slice(-1) %>%
        select(relation, dOFV)
        colnames(scm_table) <- c("","dOFV")
      #max_table
      scm_table$dOFV <- round(scm_table$dOFV, 1)
      max_scm_table <- cbind(scm_table[which.max(scm_table$dOFV),],1)
      max_scm_table$dOFV <- round(max_scm_table$dOFV,1)
      
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
          scm_table$Coef[row_nr] <- round(coef_table[!is.na(coef_table[i]),i],2)
        }
      }
      scm_table$Coef <- as.character(scm_table$Coef)

    } else {
      scm_table <- error_table("SCM")
      max_scm_table <- scm_table
      max_scm_table <- cbind(scm_table,"")
    }
  } else {
    scm_files_exists <- FALSE
    scm_table <- data.frame("SCM",NA,stringsAsFactors = F)
    colnames(scm_table) <- c("","dOFV")
    max_scm_table <- cbind(scm_table,"")

  }
  colnames(max_scm_table) <- c("","dOFV","Add.params")
  return(list(scm_files_exists=scm_files_exists,
              scm_table=scm_table,
              max_scm_table=max_scm_table))
}