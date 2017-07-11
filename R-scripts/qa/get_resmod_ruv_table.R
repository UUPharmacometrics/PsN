get_resmod_ruv_table <- function(directory, idv_name, dvid_name){
  resmod_file_exists <- get_resmod_table(directory=working.directory, idv_name)$resmod_file_exists
  if(resmod_file_exists) {
    resmod_table_full <- get_resmod_table(directory, idv_name)$resmod_table 
    if(any(resmod_table_full$dvid!="NA")) {
      dvid_nr <- unique(resmod_table_full$dvid)
      dvid_nr <- as.numeric(dvid_nr[-which(dvid_nr=="sum")])
    } else {
      dvid_nr <- 'NA'
    }
    
    if(length(dvid_nr) == 1 && dvid_nr=="NA"){
      resmod_ruv_overview <- as.data.frame(array(0,c(2,3)))
    } else {
      resmod_ruv_overview <- as.data.frame(array(0,c((2*length(dvid_nr)+length(dvid_nr)),3)))
    }
    
    resmod_ruv_table_list <- list()
    k <- 1
    for (j in 1:length(dvid_nr)) {
      resmod_table <- subset(resmod_table_full,dvid==dvid_nr[j])
      resmod_table <- resmod_table %>% select(-iteration, -dvid)
      non_time_var <- resmod_table %>%
        filter(!grepl("idv_varying", model)) %>%
        mutate(df = stringr::str_count(parameters, "="))
      time_var_cutoff <- resmod_table %>%
        filter(grepl("idv_varying_RUV_cutoff",model)) %>%
        mutate(df = 2) %>%
        arrange(desc(dOFV))
      resmod_ruv_table <- bind_rows(non_time_var, 
                                    time_var_cutoff %>% 
                                      slice(1) %>%
                                      mutate(model = "time varying"))
      resmod_ruv_table <- resmod_ruv_table[order(resmod_ruv_table$dOFV,decreasing = T),]
      rownames(resmod_ruv_table) <- NULL
      colnames(resmod_ruv_table)[which(colnames(resmod_ruv_table)=="model")] <- "Model"
      
      #choose only 3 columns
      resmod_ruv_table <- resmod_ruv_table[,c("Model","dOFV","df")]
      resmod_ruv_table[,2] <- as.character(round(resmod_ruv_table[,2],1))
      resmod_ruv_table[,3] <- as.character(resmod_ruv_table[,3])
      colnames(resmod_ruv_table) <- c("Model","dOFV","Additional parameters")
      
      #replace symbol "_" with the space
      nr_rows <- grep("_",resmod_ruv_table[,1])
      for(i in 1:length(nr_rows)) {
        resmod_ruv_table[nr_rows[i],1] <- gsub("_"," ",resmod_ruv_table[nr_rows[i],1])
      }
      resmod_ruv_table_list[[j]] <- resmod_ruv_table
      
      if(length(dvid_nr) == 1 && dvid_nr=="NA") {
        resmod_ruv_overview <- resmod_ruv_table[c(1:2),c("Model","dOFV","Additional parameters")] #the highest ofv values
      } else {
        resmod_ruv_overview[k,] <- c(paste0("(",dvid_name,"=",dvid_nr[j],")"),'','')
        resmod_ruv_overview[c(k+1,k+2),] <- resmod_ruv_table[c(1:2),c("Model","dOFV","Additional parameters")] #the highest ofv values
        k <- k + 3
      }

    }
    colnames(resmod_ruv_overview) <- c("","dOFV","Add.params")
    
  } else {
    resmod_ruv_table_list[[1]] <- error_table(col=1)
    resmod_ruv_overview <- error_table("RESMOD")
    dvid_nr <- 'NA'
  }
  
  return(list(resmod_ruv_table_list=resmod_ruv_table_list,
              resmod_ruv_overview=resmod_ruv_overview,
              dvid_nr=dvid_nr))
}