get_resmod_ruv_table <- function(directory, suffix){
  resmod_file_exists <- get_resmod_table(directory=working.directory, suffix=suffix)$resmod_file_exists
  if(resmod_file_exists) {
    resmod_table <- get_resmod_table(directory, suffix)$resmod_table %>%
      select(-iteration, -dvid)
    non_time_var <- resmod_table %>%
      filter(!grepl("idv_varying", model)) %>%
      mutate(df = stringr::str_count(parameters, "="))
    time_var_cutoff <- resmod_table %>%
      filter(grepl("idv_varying_RUV_cutoff",model)) %>%
      mutate(df = 2) %>%
      arrange(desc(dofv))
    resmod_ruv_table <- bind_rows(non_time_var, 
                                  time_var_cutoff %>% 
                                    slice(1) %>%
                                    mutate(model = "time varying"))
    resmod_ruv_table <- resmod_ruv_table[order(resmod_ruv_table$dofv,decreasing = T),]
    rownames(resmod_ruv_table) <- NULL
    colnames(resmod_ruv_table)[which(colnames(resmod_ruv_table)=="model")] <- "Model"
    
    #replace symbol "_" with the space
    nr_rows <- grep("_",resmod_ruv_table[,1])
    for(i in 1:length(nr_rows)) {
      resmod_ruv_table[nr_rows[i],1] <- gsub("_"," ",resmod_ruv_table[nr_rows[i],1])
    }
    
    resmod_ruv_overview <- resmod_ruv_table[c(1:2),] #the highest ofv values
    #choose only 3 columns
    resmod_ruv_table <- resmod_ruv_table[,c("Model","dofv","df")]
    resmod_ruv_table[,2] <- as.character(round(resmod_ruv_table[,2],1))
    resmod_ruv_table[,3] <- as.character(resmod_ruv_table[,3])
    colnames(resmod_ruv_table) <- c("Model","dofv","Additional parameters")
    resmod_ruv_overview <- resmod_ruv_overview[,c("Model","dofv","df")]
    colnames(resmod_ruv_overview) <- c("","dofv","Add.params")
    resmod_ruv_overview$dofv <- round(resmod_ruv_overview$dofv,1)
    
  } else {
    resmod_ruv_table <- error_table(col=1)
    resmod_ruv_overview <- error_table("RESMOD")
  }
  
  return(list(resmod_ruv_table=resmod_ruv_table,
              resmod_ruv_overview=resmod_ruv_overview))
}